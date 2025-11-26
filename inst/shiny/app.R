# app.R
library(shiny)
library(shinyjs)
library(sortable)
library(dplyr)
library(icmBenchmarkR)

# UI ####
ui <- fluidPage(

  useShinyjs(),

  titlePanel("ICM Benchmarking Report Generator"),

  ## Set sidebar ####
  sidebarLayout(
    sidebarPanel(

      width = 3,

      h4("Step 1: Define categories BEFORE uploading data"),
      helpText("Set the number of categories in the form and their names. These will be used in the PDF reports."),
      numericInput(
        "n_categories",
        "Number of categories:",
        value = 3,
        min = 1,
        max = 10
      ),

      uiOutput("category_name_inputs"),

      tags$hr(),

      h4("Step 2: Upload data"),
      helpText("Upload the Excel/CSV export from your M365 Form."),
      fileInput(
        "data_file",
        "Upload file:",
        accept = c(".xlsx", ".xls", ".csv")
      ),
      uiOutput("data_info"),

      tags$hr(),

      h4("Step 3: Generate reports"),
      helpText("Assign questions to categories, then generate all hospital PDFs."),
      actionButton("generate_reports", "Generate Reports", class = "btn-primary"),
      br(), br(),

      h4("Report generation progress"),
      uiOutput("hospital_status_ui"),

      br(),
      downloadButton("download_zip", "Download ZIP", class = "btn-success"),

      br(), br(),
      verbatimTextOutput("debug")
    ),

    ## Set main panel ####
    mainPanel(
      width = 8,

      h3("Assign questions to categories"),
      helpText("Drag questions from 'Unassigned' into the category boxes below."),
      uiOutput("drag_ui"),

      tags$hr(),

      h4("Current category mapping"),
      tableOutput("mapping_table")
    )
  )
)

# Server ####
server <- function(input, output, session) {

  ## Category names ####
  output$category_name_inputs <- renderUI({
    req(input$n_categories)

    default_names <- c(
      "Effective educational time",
      "Training and the non-clinical curriculum",
      "Safe, effective working environment"
    )

    lapply(seq_len(input$n_categories), function(i) {
      textInput(
        paste0("cat_", i),
        paste("Category", i, "name:"),
        value = if (i <= length(default_names)) default_names[i] else paste("Category", i)
      )
    })
  })

  ## Disable category controls once file uploaded ####
  observeEvent(input$data_file, {

    # Lock categories
    shinyjs::disable("n_categories")
    lapply(seq_len(input$n_categories), function(i) {
      shinyjs::disable(paste0("cat_", i))
    })

    # Initialise hospital statuses
    observeEvent(df_clean(), ignoreInit = TRUE, {
      hosp <- sort(unique(df_clean()$Hospital))
      progress_vals$hospitals <- hosp
      progress_vals$status <- rep("pending", length(hosp))
      })
  })

  # Load and clean data ####
  df_clean <- reactive({
    req(input$data_file)
    read_clean_form(input$data_file$datapath)
  })

  question_cols <- reactive({
    req(df_clean())

    # Convert to "pretty" names
    lookup <- attr(df_clean(), "question_lookup")
    pretty <- lookup$original
    names(pretty) <- lookup$cleaned     # cleaned → pretty mapping

    pretty   # return pretty labels (display), but preserve internal names
  })

  output$data_info <- renderUI({
    req(df_clean())
    tagList(
      strong("Data loaded successfully."),
      br(),
      paste("Rows:", nrow(df_clean())),
      br(),
      paste("Columns:", ncol(df_clean())),
      br(),
      paste("Detected", length(question_cols()), "scored questions.")
    )
  })

  ## Drag and drop UI ####
  output$drag_ui <- renderUI({
    req(df_clean())
    req(question_cols())
    req(input$n_categories)

    n <- input$n_categories

    # Build category buckets as a list of rank_list objects
    cat_buckets <- lapply(seq_len(n), function(i) {
      add_rank_list(
        text = input[[paste0("cat_", i)]],
        labels = character(0),
        input_id = paste0("bucket_", i)
      )
    })

    # Unassigned questions bucket
    unassigned <- add_rank_list(
      text = "Unassigned",
      labels = question_cols(),
      input_id = "unassigned"
    )

    # Now flatten the list explicitly using do.call()
    do.call(
      bucket_list,
      c(
        list(
          header = NULL,
          group_name = "questions",
          orientation = "horizontal",
          unassigned
        ),
        cat_buckets
      )
    )
  })

  ## Build category_map from bucket contents ####

  category_map <- reactive({
    req(input$n_categories)

    out <- list()

    for (i in seq_len(input$n_categories)) {
      cat_name <- input[[paste0("cat_", i)]]
      qs <- input[[paste0("bucket_", i)]]

      if (!is.null(qs) && length(qs) > 0) {
        out[[cat_name]] <- qs
      }
    }

    out
  })

  output$mapping_table <- renderTable({
    cm <- category_map()
    if (length(cm) == 0) return(NULL)

    data.frame(
      Category = rep(names(cm), lengths(cm)),
      Question = unlist(cm),
      check.names = FALSE
    )
  })

  # Report status tracker ####
  hospital_status <- reactiveVal(NULL)

  output$hospital_status_ui <- renderUI({
    st <- hospital_status()
    if (is.null(st)) return(NULL)

    tagList(lapply(seq_len(nrow(st)), function(i) {
      icon <- if (st$status[i] == "pending") "hourglass-half" else "check-circle"
      color <- if (st$status[i] == "pending") "orange" else "green"

      div(
        tags$i(class = paste0("fa fa-", icon), style = paste0("color:", color, ";")),
        paste(" ", st$hospital[i])
      )
    }))
  })

  ## Generate reports ####
  observeEvent(input$generate_reports, {

    req(df_clean())
    df <- df_clean()
    cm <- category_map()
    if (length(cm) == 0) cm <- NULL

    hospitals <- sort(unique(df$Hospital))
    hospital_status(
      data.frame(hospital = hospitals, status = "pending")
    )

    tmp <- file.path(tempdir(), "icm_reports")
    if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
    dir.create(tmp)

    regional_summary <- summarise_region(df, category_map = cm)

    # disable download button until complete
    shinyjs::disable("download_zip")

    withProgress(message = "Generating reports...", value = 0, {
      for (i in seq_along(hospitals)) {
        hosp <- hospitals[i]

        incProgress(1 / length(hospitals),
                    detail = paste("Processing:", hosp))

        safe_name <- gsub("[^A-Za-z0-9_]+", "_", hosp)
        out_pdf <- file.path(tmp, paste0(safe_name, "_report.pdf"))

        render_hospital_report(
          hospital_name    = hosp,
          df_clean         = df,
          regional_summary = regional_summary,
          output_file      = out_pdf
        )

        st <- hospital_status()
        st$status[st$hospital == hosp] <- "done"
        hospital_status(st)
      }
    })

    shinyjs::enable("download_zip")
  })

  # Download zip ####
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("ICM_hospital_reports_", Sys.Date(), ".zip")
    },
    content = function(file) {

      tmp <- file.path(tempdir(), "icm_reports")

      files <- list.files(tmp)
      if (length(files) == 0) stop("No PDFs generated.")

      old <- setwd(tmp)
      on.exit(setwd(old))

      utils::zip(zipfile = file, files = files)
    }
  )
}

shinyApp(ui, server)
