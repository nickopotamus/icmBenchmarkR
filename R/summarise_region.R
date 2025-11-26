#' Summarise regional scores
#'
#' @param df_clean Cleaned data frame from read_clean_form().
#' @param category_map Optional list mapping categories to question names.
#'   Example:
#'   list(
#'     "Effective educational time" = c("Supervisor meetings occur", "Clinical time spent"),
#'     "Non-clinical curriculum"    = c("Induction", "Study leave"),
#'     "Safe environment"           = c("The rota", "Supervision")
#'   )
#'
#' @return A list containing:
#'   - mean_q_by_hosp: mean score for each question per hospital
#'   - mean_hospital: overall mean score per hospital
#'   - mean_q_by_region: region-level mean per question
#'   - question_cols: detected question column names
#'   - category_map: validated version of user-supplied category_map
#'   - hospital_counts: total respondents per hospital
#'   - n_total: total respondents overall
#'
#' @export
summarise_region <- function(df_clean, category_map = NULL) {

  # Extract lookup table for original question labels
  lookup <- attr(df_clean, "question_lookup")

  # Identify cleaned question columns
  question_cols <- lookup$cleaned


  # Validate / construct category map ####

  # If none supplied, treat all questions as one category
  if (is.null(category_map) || length(category_map) == 0) {
    category_map <- list("All_questions" = question_cols)
  }

  # Drop any questions not found in the dataframe
  category_map <- lapply(category_map, function(qs) {
    intersect(qs, question_cols)
  })

  # Remove empty categories
  category_map <- category_map[lengths(category_map) > 0]

  # Identify unassigned questions
  assigned <- unique(unlist(category_map))
  unassigned <- setdiff(question_cols, assigned)

  if (length(unassigned) > 0) {
    category_map$Unassigned <- unassigned
  }

  # Compute mean of each question per hospital ####
  question_means_by_hosp <- df_clean %>%
    dplyr::select(Hospital, dplyr::all_of(question_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(question_cols),
      names_to = "question",
      values_to = "value"
    ) %>%
    dplyr::group_by(Hospital, question) %>%
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Overall mean score per hospital ####
  hospital_means <- question_means_by_hosp %>%
    dplyr::group_by(Hospital) %>%
    dplyr::summarise(mean_score = mean(mean_value), .groups = "drop") %>%
    dplyr::arrange(mean_score)

  # Regional mean for each question (mean of hospital means) ####
  region_question_means <- question_means_by_hosp %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute category-level means ####
  category_summary <- purrr::map_dfr(
    names(category_map),
    function(cat) {

      qs <- category_map[[cat]]

      df_clean %>%
        dplyr::select(Hospital, dplyr::all_of(qs)) %>%
        dplyr::mutate(
          category = cat,
          category_score = rowMeans(across(all_of(qs)), na.rm = TRUE)
        ) %>%
        dplyr::group_by(Hospital, category) %>%
        dplyr::summarise(
          mean_score = mean(category_score, na.rm = TRUE),
          .groups = "drop"
        )
    }
  )

  # Calculate totals ####
  hospital_counts <- df_clean %>%
    dplyr::group_by(Hospital) %>%
    dplyr::summarise(n_responses = dplyr::n(), .groups = "drop")

  # Return combined summary object ####

  return(list(
    hospital_means         = hospital_means,
    question_means_by_hosp = question_means_by_hosp,
    region_question_means  = region_question_means,
    category_summary       = category_summary,
    category_map           = category_map,
    lookup                 = lookup,
    hospital_counts        = hospital_counts,
    n_total                = nrow(df_clean)
  ))
}
