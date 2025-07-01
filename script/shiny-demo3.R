# Load packages
library(shiny)
library(bslib)
library(tools)
library(readxl)
library(openxlsx)
library(DT)
library(ragnar)
library(ellmer)
library(shinychat)
library(knitr)
library(haven)
library(rtables)
library(flextable)
library(tern)
library(commonmark)
library(cimstfl)
library(stat2mw)

# Define UI layout
ui <- page_fillable(
  titlePanel("CDISC Data Query Agent"),
  layout_columns(
    col_widths = c(4, 8),

    # Sidebar for spec upload and chat agent
    card(
      fileInput("spec_file", "Upload specification file", accept = c(".xlsx", ".xls")),
      hr(style = "margin: 1px 0;"),
      chat_ui("chat"),
      style = "overflow-y: auto;",
    ),

    # Main panel for data upload and output preview
    card(
      fluidRow(
        column(3, fileInput("adsl_data", "Connect ADSL data", accept = ".sas7bdat")),
        column(3, offset = 6, br(), actionButton("sample_data", "Try it out with sample data!", class = "btn-lg btn-primary"))
      ),
      hr(style = "margin: 1px 0;"),
      htmlOutput("tbl_viewer"),
      style = "overflow-y: auto;",
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Handle shinychat module input
  rv <- reactiveValues(chat = NULL)

  # Use sample specification file and ADSL data
  observeEvent(input$sample_data, {
    # sample spec file path
    # spec_path <- "/mnt/R/Workplace/zwu/shiny.rag/inst/test_doc/adsl_spec.xlsx"
    # spec_path <- file.path(getwd(), "inst/test_doc", "adsl_spec.xlsx")
    spec_path <- system.file("test_doc", "adsl_spec.xlsx", package = "shiny.rag")
    # Load specification file
    df <- read_excel(spec_path, sheet = "ADSL")

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

    You have access to fully derived ADSL dataset based on a provided specification file.

    Your task is to:
    - Interpret the user's question about the ADSL dataset.
    - Generate valid, clean R code using only the function `t_b_dm_01()` to answer the question.
    - The `t_b_dm_01()` function has the following parameters:
        - `adam`: a list of dataset names, set to fixed variable `adam_data`
        - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
        - `anal_var`: a character vector of analysis variables to summarize, default is `c("AGE", "SEX", "RACE", "ETHNIC")`
        - `pop_var`: a string specifying a population flag variable to filter the population, default is `NULL`
    - This function produces a table describing population summary statistics for analysis variables and is split by the specified arm variable.

    A sample question might be: "summarize the age for arm variable TRT01P and population flag SAFFL".

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

    # Load sample adsl data
    # adsl_path <- "/mnt/R/Workplace/zwu/shiny.rag/inst/test_doc/adsl.sas7bdat"
    # adsl_path <- file.path(getwd(), "inst/test_doc", "adsl.sas7bdat")
    adsl_path <- system.file("test_doc", "adsl.sas7bdat", package = "shiny.rag")
    rv$adsl_data <- read_sas(adsl_path)

    showNotification("Sample data loaded successfully!", type = "message")
  })

  # Store the uploaded ADSL data
  observeEvent(input$adsl_data, {
    rv$adsl_data <- read_sas(input$adsl_data$datapath)

    showNotification("ADSL data connected successfully!", type = "message")
  })

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

  observeEvent(input$spec_file, {
    req(input$spec_file)

    # Load specification file
    df <- read_excel(input$spec_file$datapath, sheet = "ADSL")

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

    You have access to fully derived ADSL dataset based on a provided specification file.

    Your task is to:
    - Interpret the user's question about the ADSL dataset.
    - Generate valid, clean R code using only the function `t_b_dm_01()` to answer the question.
    - The `t_b_dm_01()` function has the following parameters:
        - `adam`: a list of dataset names, set to fixed variable `adam_data`
        - `arm_var`: a string specifying the treatment arm variable, default is `"TRT01P"`
        - `anal_var`: a character vector of analysis variables to summarize, default is `c("AGE", "SEX", "RACE", "ETHNIC")`
        - `pop_var`: a string specifying a population flag variable to filter the population, default is `NULL`
    - This function produces a table describing population summary statistics for analysis variables and is split by the specified arm variable.

    A sample question might be: "summarize the age for arm variable TRT01P and population flag SAFFL".

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

  # Stream the response to chat UI
  observeEvent(input$chat_user_input, {
    req(rv$chat)
    stream <- rv$chat$chat(input$chat_user_input)
    chat_append("chat", stream)

    # Store the code as reactive value
    rv$code_res <- stream
  })

  output$tbl_viewer <- renderUI({
    req(rv$chat, rv$code_res, rv$adsl_data, input$chat_user_input)

    tryCatch({
      # Use the chat object to get and clean code from user question
      code <- substr(rv$code_res, 6, nchar(rv$code_res) - 4)
      cat("\nGenerated R code:\n", code, "\n")

      # Load the uploaded ADSL data
      adam_data <- list(adsl = rv$adsl_data)

      # Evaluate the code
      res <- eval(parse(text = code))

      # Render rtables as HTML output
      if (inherits(res$rs, "TableTree")) {
        tbl_html <- rtables::as_html(res$rs)
        nar_ls <- stat2mw::csr_sec_10_2_1(res$ard)
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
