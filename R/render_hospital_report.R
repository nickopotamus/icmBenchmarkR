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

  # Path to the package Rmd
  rmd_path <- template

  # 1. Create a safe temp directory
  tmp_rmd_dir <- file.path(tempdir(), "icm_rmd")
  if (!dir.exists(tmp_rmd_dir)) dir.create(tmp_rmd_dir, recursive = TRUE)

  # 2. Copy Rmd into temp directory
  tmp_rmd <- file.path(tmp_rmd_dir, "hospital_report_template.Rmd")
  file.copy(rmd_path, tmp_rmd, overwrite = TRUE)

  # 3. Render *inside* temp directory
  rmarkdown::render(
    input = tmp_rmd,
    output_file = output_file,
    params = list(
      hospital_name    = hospital_name,
      hospital_data    = df_clean,
      regional_summary = regional_summary
    ),
    envir = new.env(parent = globalenv())
  )

  return(invisible(TRUE))
}
