#' Generate reports for all hospitals in a dataset
#'
#' @param input_file Path to the Excel export.
#' @param output_dir Directory for PDFs.
#' @param template Optional template override.
#' @param category_map Optional category structure.
#'
#' @export
generate_all_reports <- function(
    input_file,
    output_dir = "hospital_reports",
    template = NULL,
    category_map = NULL
) {
  # Normalise and ensure directory is created
  output_dir <- fs::path_norm(output_dir)
  fs::dir_create(output_dir)

  # Clean data
  df_clean <- read_clean_form(input_file)

  # Summaries (with or without category map)
  regional_summary <- summarise_region(df_clean, category_map = category_map)

  # Template
  if (is.null(template)) {
    template <- system.file("rmd", "hospital_report_template.Rmd",
                            package = "icmBenchmarkR")
  }

  # Hospitals
  hospital_list <- unique(df_clean$Hospital)

  # Render each report
  for (hosp in hospital_list) {

    safe_name <- gsub("[^A-Za-z0-9_]+", "_", hosp)

    file_out <- fs::path(output_dir, paste0(safe_name, "_report.pdf"))

    render_hospital_report(
      hospital_name    = hosp,
      df_clean         = df_clean,
      regional_summary = regional_summary,
      output_file      = file_out,
      template         = template
    )
  }

  invisible(output_dir)
}
