#' Render a report for a single hospital
#'
#' @param hospital_name Name of the hospital.
#' @param df_clean Cleaned dataset.
#' @param regional_summary Output of summarise_region().
#' @param output_file Full path to the PDF to be created.
#' @param template Path to the Rmd template.
#'
#' @export
render_hospital_report <- function(
    hospital_name,
    df_clean,
    regional_summary,
    output_file,
    template = system.file("rmd", "hospital_report_template.Rmd",
                           package = "icmBenchmarkR")
) {

  # Ensure directory exists
  out_dir  <- dirname(output_file)
  out_file <- basename(output_file)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Parameters to pass to markdown
  params <- list(
    hospital_name    = hospital_name,
    hospital_data    = df_clean,
    regional_summary = regional_summary
  )

  # Render the document
  rmarkdown::render(
    input       = template,
    output_file = out_file,
    output_dir  = out_dir,
    params      = params,
    envir       = new.env(parent = globalenv())
  )
}
