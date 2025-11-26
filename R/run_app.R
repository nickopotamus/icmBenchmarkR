#' Launch the ICM Benchmarking Shiny App
#'
#' @export
run_app <- function() {
  app_dir <- system.file("shiny", package = "icmBenchmarkR")
  shiny::runApp(app_dir, launch.browser = TRUE)
}
