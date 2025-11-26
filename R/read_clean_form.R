#' Read and clean ICM training quality assessment data
#'
#' @param path Path to the Excel file exported from M365 Forms.
#' @return A cleaned tibble with derived variables.
#' @export
read_clean_form <- function(path) {

  # Load raw table ####
  if (grepl("\\.csv$", path)) {
    df <- read.csv(path, check.names = FALSE)
  } else {
    df <- readxl::read_excel(path)
  }

  df <- df %>% mutate(across(everything(), as.character))

  # Remove MS Forms metadata columns ####
  msforms_meta <- c(
    "ID", "Id", "id",
    "Submission ID", "Submission_Id",
    "Start time", "Start Time",
    "Completion time", "Completion Time",
    "Email", "E-mail", "Responder Email", "Responder email",
    "Name",
    "Duration",
    "Score"
  )

  df <- df[, !(names(df) %in% msforms_meta)]

  # Parse demographic fields ####

  # LTFT
  if ("LTFT trainee" %in% names(df)) {
    df <- df %>% mutate(ltft = factor(`LTFT trainee`, levels = c("No","Yes"), labels = c(0,1)))
  }

  # Recommend placement
  rec_col <- names(df)[stringr::str_detect(names(df), "(?i)recommend")]
  if (length(rec_col) == 1) {
    df <- df %>% mutate(recommend = factor(.data[[rec_col]], levels = c("No","Yes"), labels = c(0,1)))
  }

  # Stage of training
  if ("Stage of training" %in% names(df)) {
    df <- df %>%
      mutate(
        stage = forcats::fct_na_value_to_level(
          factor(
            readr::parse_number(
              .data[["Stage of training"]],
              na = "Out of programme ICM"
            )
          ),
          level = "Out of programme ICM"
        )
      )
  }

  # Rotation period
  if ("Rotation period" %in% names(df)) {
    df <- df %>%
      mutate(
        rotation_start = stringr::str_extract(`Rotation period`, "(?i)[A-Za-z]+\\s+[0-9]{4}"),
        rotation_start_date = suppressWarnings(lubridate::parse_date_time(rotation_start, orders = "B Y")),
        rotation = forcats::fct_reorder(`Rotation period`, rotation_start_date)
      )
  }

  # Hospital
  if ("Hospital" %in% names(df)) {
    df <- df %>% mutate(Hospital = factor(Hospital))
  }

  # Identify demographic and derived columns ####

  demographic_cols <- c(
    "Rotation period",
    "Stage of training",
    "Hospital",
    "LTFT trainee",
    "Do you have any additional comments about the training site to make that aren't covered above?"
  )

  # Add in recommended col
  if (length(rec_col) == 1) {
    demographic_cols <- c(demographic_cols, rec_col)
  }
  demographic_cols <- demographic_cols[demographic_cols %in% names(df)]

  # Also exclude any derived demographic columns created above
  derived_cols <- c("stage", "ltft", "recommend",
                    "rotation", "rotation_start", "rotation_start_date")
  derived_cols <- derived_cols[derived_cols %in% names(df)]

  demographic_all <- c(demographic_cols, derived_cols)

  # Everything else is a question column
  question_cols_raw <- setdiff(names(df), demographic_all)

  # Standardise question names ####

  # Function to produce machine readable names
  clean_question_names <- function(x) {
    x %>%
      stringr::str_replace_all("\\r\\n", " ") %>%
      stringr::str_replace_all("\\n", " ") %>%
      stringr::str_replace_all("\\.{2,}", " ") %>%
      stringr::str_replace_all("[[:punct:]]", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim() %>%
      stringr::str_replace_all(" ", "_")
  }

  # Create mapping of original → cleaned names
  question_lookup <- tibble::tibble(
    original = question_cols_raw,
    cleaned  = clean_question_names(question_cols_raw)
  )

  new_names <- names(df)
  q_idx <- match(question_cols_raw, names(df))
  new_names[q_idx] <- clean_question_names(question_cols_raw)
  names(df) <- new_names

  # Re-identify question columns (numeric scoring 1–4) ####
  question_cols <- df %>%
    dplyr::select(all_of(new_names[q_idx])) %>%
    dplyr::select(where(~ all(stringr::str_detect(.x, "^[1-4]") | is.na(.x)))) %>%
    names()

  # Parse numeric scoring
  df <- df %>%
    dplyr::mutate(across(all_of(question_cols), readr::parse_number))

  attr(df, "question_lookup") <- question_lookup
  return(df)
}
