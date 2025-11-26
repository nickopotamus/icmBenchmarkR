
<!-- README.md is generated from README.Rmd. Please edit that file -->

# icmBenchmarkR

Automates the generation of ICM training quality benchmarking reports
from Microsoft Forms data.

## Installation

You can install the development version of ICMbenchmarkR like so:

``` r
devtools::install_github("nickopotamus/icmBenchmarkR")
```

Or clone this repository/download the code as a zip file and the code
directly.

## Example of using all functions

``` r
# Reload package (if using source)
devtools::document()
devtools::load_all()

# Load and clean data
df <- read_clean_form("data/latest_2511.xlsx")
names(df)                    # should show cleaned column names
attr(df, "question_lookup")  # should show original

# Example question map (including "uncategorised" section)
lookup <- attr(df, "question_lookup")
test_map <- list(
  "Education"  = lookup$cleaned[1:5],
  "Curriculum" = lookup$cleaned[6:10]
)

# Generate region summary
reg_summary <- summarise_region(df, category_map = test_map)
names(reg_summary)

# Generate PDF for a single hospital
render_hospital_report(
  hospital_name    = "Queen's Medical Centre",
  df_clean         = df,
  regional_summary = reg_summary,
  output_file      = "hospital_reports/qmc_test.pdf"
)

# Generate PDFs for an entire region
generate_all_reports("data/latest_2511.xlsx", category_map = test_map)

# Run the app in your browser
run_app()
```
