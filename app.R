# Load packages
library(shiny)
library(devtools)
library(bslib)
library(tools)
library(readxl)
library(openxlsx)
library(DT)
library(ragnar)
library(ellmer)
library(shinychat)
library(knitr)
library(quarto)
library(haven)
library(rtables)
library(flextable)
library(tern)
library(commonmark)
library(cimstfl)
library(stat2mw)

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Define UI layout
ui <- page_fillable(
  layout_columns(
    col_widths = c(4, 8),
    
    # Sidebar for spec upload and chat agent
    card(
      card_header(tags$h3("{DataChat}: CDISC data query agent")),
      fileInput("spec_file", "Upload specification file",
                accept = c(".xlsx", ".xls"),
                placeholder = "Accept .xlsx"),
      hr(style = "margin: 0;"),
      chat_ui("chat"),
      style = "overflow-y: auto;",
    ),
    
    # Main panel for data upload and output preview
    card(
      card_header("Local non-AI TFL generation (guarantees data privacy)"),
      fluidRow(
        column(3, fileInput("adam_data", "Connect ADaM data", accept = ".zip",
                            placeholder = "Accept .zip")),
        column(3, offset = 6, br(), actionButton("sample_data", "Try out sample data!", class = "btn-lg btn-primary"))
      ),
      hr(style = "margin: 0;"),
      htmlOutput("tbl_viewer"),
      style = "overflow-y: auto;",
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  chat_append("chat", paste(
    "Hello there! I am {DataChat}, your CDISC data query agent.",
    "You can ask me questions about your data,",
    "and I will respond with a table and a narrative.",
    "Please upload your data above or use the sample data we provided.",
    sep = "\n"
  ))
  # Handle shinychat module input
  rv <- reactiveValues(chat = NULL)
  
  # Store location for vector DB
  store_location <- tempfile("cdisc.ragnar.duckdb")
  unlink(store_location)  # reset on each run
  
  # Set OpenAI key
  Sys.setenv(OPENAI_API_KEY = "")
  
  # Initialize the store
  store <- ragnar::ragnar_store_create(
    store_location,
    embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small",
                                      api_key = Sys.getenv("OPENAI_API_KEY"))
  )
  
  # Use sample specification file and ADSL data
  observeEvent(input$sample_data, {
    # sample spec file path
    spec_path <- system.file("test_doc", "adam_spec.xlsx", package = "shiny.rag")
    # Load specification file
    adsl_spec <- read_excel(spec_path, sheet = "ADSL")
    adae_spec <- read_excel(spec_path, sheet = "ADAE")
    df <- rbind(adsl_spec, adae_spec)
    
    # Process and read document (batch size = 10)
    chunk_size <- floor(nrow(df) / 10)
    indexes <- seq(1, nrow(df), by = chunk_size)
    
    pages <- do.call(rbind, lapply(indexes, function(i) {
      row_df <- df[i:min(i + chunk_size - 1, nrow(df)), ]
      tmp_file <- tempfile(fileext = ".xlsx")
      write.xlsx(row_df, tmp_file)
      ragnar_read(tmp_file)
    }))
    
    ragnar::ragnar_store_insert(store, pages)
    ragnar::ragnar_store_build_index(store)
    
    # Optionally connect again as read-only
    store_ro <- ragnar::ragnar_store_connect(store_location, read_only = TRUE)
    
    system_prompt <- r"--(
    You are an expert in querying and analyzing CDISC ADaM ADSL datasets. You follow CDISC conventions and best practices for exploratory clinical data summaries.

    You have access to fully derived ADSL and ADAE datasets based on a provided specification file.

    Your task is to:
    - Interpret the user's question about the ADSL and/or ADAE datasets.
    - Generate valid, clean R code chosing only from the functions `t_b_dm_01()` or `t_s_ae_03()` to answer the question.
    - Ensure that all input variable names used in the function call (e.g., `arm_var`, `anal_var`, `pt_var`, `filter_var`, `filter_val`, `pop_var`) must exist in the specification file. Do not make up or hallucinate variable names.

    The `t_b_dm_01()` function has the following parameters:
      - `adam`: a list of dataset names, set to fixed variable `adam_data`
      - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
      - `anal_var`: a character vector of analysis variables to summarize, default is `c("AGE", "SEX", "RACE", "ETHNIC")`
      - `pop_var`: a string specifying a population flag variable to subset the population, default is `NULL`
    The `t_b_dm_01()` function produces a table describing population summary statistics for analysis variables and stratified by treatment arm (`arm_var`).
    A sample question to use `t_b_dm_01()` will be: "summarize the age for arm variable TRT01P and population flag SAFFL".

    The `t_s_ae_03()` function has the following parameters:
      - `adam`: a list of dataset names, set to fixed variable `adam_data`
      - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
      - `pt_var`: a string indicating the AE term used for counting occurrences, default is `"AEBODSYS"`. Supported values include:
        - `"AETERM"`: Reported term
        - `"AEDECOD"`: Preferred term
        - `"AEBODSYS"`: Body system organ class
        - `"AESOC"`: Primary system organ class
      - `filter1_var`: a string specifying a variable to apply a filter on, default is `NULL`
      - `filter1_val`: a string specifying the value of `filter_var` to filter by, default is `NULL`
      - `pop_var`: a string specifying a population flag variable to subset the population, default is `NULL`
    The `t_s_ae_03()` function produces a count table of Adverse Events (AE), grouped by the selected AE term (`pt_var`) and stratified by treatment arm (`arm_var`).

    A sample question to use `t_s_ae_03()` will be: "summarize the adverse events by body system for arm variable TRT01P and population flag SAFFL".

    Return only the R code. Do not include explanations or summaries.
)--"
    
    # Create chat object
    chat_obj <- ellmer::chat_openai(system_prompt,
                                    model = "gpt-4o",
                                    params = params(temperature = 0, seed = 1234),
                                    api_key = Sys.getenv("OPENAI_API_KEY"),
                                    echo = "none")
    
    # Retrieve based on embedding
    ragnar::ragnar_register_tool_retrieve(chat_obj, store_ro)
    
    # Save the chat object in a reactive value
    rv$chat <- chat_obj
    
    # Load and save sample ADaM data
    adsl_path <- system.file("test_doc", "adsl.sas7bdat", package = "shiny.rag")
    adae_path <- system.file("test_doc", "adae.sas7bdat", package = "shiny.rag")
    rv$adam_data <- list(adsl = haven::read_sas(adsl_path),
                         adae = haven::read_sas(adae_path))
    
    showNotification("Sample data loaded successfully!", type = "message")
  })
  
  # Store the uploaded specification file
  observeEvent(input$spec_file, {
    req(input$spec_file)
    
    # Load specification file
    adsl_spec <- read_excel(input$spec_file$datapath, sheet = "ADSL")
    adae_spec <- read_excel(input$spec_file$datapath, sheet = "ADAE")
    df <- rbind(adsl_spec, adae_spec)
    
    # Process and read document (batch size = 10)
    chunk_size <- floor(nrow(df) / 10)
    indexes <- seq(1, nrow(df), by = chunk_size)
    
    pages <- do.call(rbind, lapply(indexes, function(i) {
      row_df <- df[i:min(i + chunk_size - 1, nrow(df)), ]
      tmp_file <- tempfile(fileext = ".xlsx")
      write.xlsx(row_df, tmp_file)
      ragnar_read(tmp_file)
    }))
    
    ragnar::ragnar_store_insert(store, pages)
    ragnar::ragnar_store_build_index(store)
    
    # Optionally connect again as read-only
    store_ro <- ragnar::ragnar_store_connect(store_location, read_only = TRUE)
    
    # Set up system prompt and chat
    system_prompt <- r"--(
    You are an expert in querying and analyzing CDISC ADaM ADSL datasets. You follow CDISC conventions and best practices for exploratory clinical data summaries.

    You have access to fully derived ADSL and ADAE datasets based on a provided specification file.

    Your task is to:
    - Interpret the user's question about the ADSL and/or ADAE datasets.
    - Generate valid, clean R code chosing only from the functions `t_b_dm_01()` or `t_s_ae_03()` to answer the question.
    - Ensure that all input variable names used in the function call (e.g., `arm_var`, `anal_var`, `pt_var`, `filter_var`, `filter_val`, `pop_var`) must exist in the specification file. Do not make up or hallucinate variable names.

    The `t_b_dm_01()` function has the following parameters:
      - `adam`: a list of dataset names, set to fixed variable `adam_data`
      - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
      - `anal_var`: a character vector of analysis variables to summarize, default is `c("AGE", "SEX", "RACE", "ETHNIC")`
      - `pop_var`: a string specifying a population flag variable to subset the population, default is `NULL`
    The `t_b_dm_01()` function produces a table describing population summary statistics for analysis variables and stratified by treatment arm (`arm_var`).
    A sample question to use `t_b_dm_01()` will be: "summarize the age for arm variable TRT01P and population flag SAFFL".

    The `t_s_ae_03()` function has the following parameters:
      - `adam`: a list of dataset names, set to fixed variable `adam_data`
      - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
      - `pt_var`: a string indicating the AE term used for counting occurrences, default is `"AEBODSYS"`. Supported values include:
        - `"AETERM"`: Reported term
        - `"AEDECOD"`: Preferred term
        - `"AEBODSYS"`: Body system organ class
        - `"AESOC"`: Primary system organ class
      - `filter1_var`: a string specifying a variable to apply a filter on, default is `NULL`
      - `filter1_val`: a string specifying the value of `filter_var` to filter by, default is `NULL`
      - `pop_var`: a string specifying a population flag variable to subset the population, default is `NULL`
    The `t_s_ae_03()` function produces a count table of Adverse Events (AE), grouped by the selected AE term (`pt_var`) and stratified by treatment arm (`arm_var`).

    A sample question to use `t_s_ae_03()` will be: "summarize the adverse events by body system for arm variable TRT01P and population flag SAFFL".

    Return only the R code. Do not include explanations or summaries.
)--"
    
    # Create chat object
    chat_obj <- ellmer::chat_openai(system_prompt,
                                    model = "gpt-4o",
                                    params = params(temperature = 0, seed = 1234),
                                    api_key = Sys.getenv("OPENAI_API_KEY"),
                                    echo = "none")
    
    # Retrieve based on embedding
    ragnar::ragnar_register_tool_retrieve(chat_obj, store_ro)
    
    # Save the chat object in a reactive value
    rv$chat <- chat_obj
    
    showNotification("Specification file uploaded successfully!", type = "message")
  })
  
  # Store the uploaded ADSL data
  observeEvent(input$adam_data, {
    # Unzip the data
    file_names <- unzip(input$adam_data$datapath, list = TRUE)$Name
    adsl_path <- file_names[grepl("adsl", file_names, ignore.case = TRUE)][1]
    adae_path <- file_names[grepl("adae", file_names, ignore.case = TRUE)][1]
    
    # Save the data in a reactive value
    rv$adam_data <- list(adsl = haven::read_sas(unz(input$adam_data$datapath, adsl_path)),
                         adae = haven::read_sas(unz(input$adam_data$datapath, adae_path)))
    
    showNotification("ADaM data connected successfully!", type = "message")
  })
  
  # Stream the response to chat UI
  observeEvent(input$chat_user_input, {
    req(rv$chat)
    stream <- rv$chat$chat(input$chat_user_input)
    chat_append("chat", stream)
    
    # Store the code as reactive value
    rv$code_res <- stream
  })
  
  output$tbl_viewer <- renderUI({
    req(rv$chat, rv$code_res, rv$adam_data, input$chat_user_input)
    
    tryCatch({
      # Use the chat object to get and clean code from user question
      code <- substr(rv$code_res, 6, nchar(rv$code_res) - 4)
      cat("\nGenerated R code:\n", code, "\n")
      
      # Load the uploaded ADSL data
      adam_data <- rv$adam_data
      
      # Evaluate the code
      res <- eval(parse(text = code))
      
      # Render rtables as HTML output
      if (inherits(res$rs, c("TableTree", "ElementaryTable"))) {
        tbl_html <- rtables::as_html(res$rs)
        if (grepl("t_b_dm_01", code)) {
          nar_ls <- stat2mw::t_b_dm_01_mw(res$ard)
        } else if (grepl("t_s_ae_03", code)) {
          nar_ls <- stat2mw::t_s_ae_03_mw(res$ard)
        }
        nar_md <- stat2mw::ut_glue(nar_ls$var_df, nar_ls$md)
        nar_html <- commonmark::markdown_html(nar_md)
        return(HTML(paste0("<h3>", nar_ls$header, "</h3>", tbl_html, "<br><br>", nar_html)))
      } else {
        div(style = "color: red;", "Error: Result is not a valid rtables object.")
      }
    }, error = function(e) {
      div(style = "color: red;", paste("Error while evaluating code:", e$message))
    })
    
  })
  
}

shinyApp(ui, server)
