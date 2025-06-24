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

# Define UI layout
ui <- page_fluid(
  titlePanel("SDTM to ADaM Coding Agent"),
  layout_columns(
    col_widths = c(4, 8),
    
    # Sidebar with upload and output code
    card(
      fileInput("doc_file", "Upload Specification file",
                accept = c(".xlsx", ".xls")),
      hr(),
      h4("Generated {admiral} R Code:"),
      verbatimTextOutput("admiral_code"),
      style = "height: 800px; overflow-y: auto;"
    ),
    
    # Main panel for document preview
    card(
      uiOutput("doc_viewer"),
      style = "height: 800px; overflow-y: auto;"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Temp folder for uploaded files
  temp_doc_dir <- tempfile("docs")
  dir.create(temp_doc_dir)
  addResourcePath("docs", temp_doc_dir)
  
  # Handle shinychat module input
  rv <- reactiveValues(chat = NULL)
  
  # Store location for vector DB
  store_location <- tempfile("ragnar_store.duckdb")
  unlink(store_location)  # reset on each run
  
  # Set OpenAI key
  Sys.setenv(OPENAI_API_KEY = "")
  
  # Initialize the store
  store <- ragnar::ragnar_store_create(
    store_location,
    embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small",
                                      api_key = Sys.getenv("OPENAI_API_KEY"))
  )
  
  observeEvent(input$doc_file, {
    req(input$doc_file)
    
    # Save the uploaded file to a web-accessible folder
    ext <- tools::file_ext(input$doc_file$name)
    safe_name <- paste0("uploaded.", ext)
    dest_path <- file.path(temp_doc_dir, safe_name)
    file.copy(input$doc_file$datapath, dest_path, overwrite = TRUE)
    
    # Render the document preview
    df <- readxl::read_excel(dest_path, sheet = "ADSL")[1:50, ]
    output$doc_viewer <- renderUI({
      datatable(df, options = list(scrollX = TRUE, pageLength = 10))
    })
    
    # Vector store the uploaded document
    pages <- data.frame()
    for (i in 1:5) {
      row_df <- df[((i-1)*10+1):(i*10), ]
      tmp_file <- tempfile(fileext = ".xlsx")
      write.xlsx(row_df, tmp_file)
      pages <- rbind(pages, ragnar_read(tmp_file))
    }
    
    ragnar::ragnar_store_insert(store, pages)
    ragnar::ragnar_store_build_index(store)
    
    # Optionally connect again as read-only
    store_ro <- ragnar::ragnar_store_connect(store_location, read_only = TRUE)
    
    # Set up system prompt and chat
    system_prompt <- r"--(
    You are an expert in creating CDISC ADaM datasets using the {admiral} R package. You follow CDISC and {admiral} best practices.

    You have been provided with a specification file containing derivation rules for ADSL dataset.

    The specification file is separated and stored by variable name where each row contains one variable derivation rule.

    Your task is to:
    - Read the ADSL specification sheet.
    - Generate R code using {admiral} to implement the derivation logic (computational method) for all variables.
    - Use functions in {admiral} package like `derive_vars_dtm()`, `derive_vars_merged()`, `derive_vars_duration()`, `derive_vars_extreme_event()`, etc., as appropriate.
    - Follow {admiral} coding style: pipe-based workflows (`%>%`), clean formatting, clear comments.
    - Include relevant library calls (`library(admiral)`, `library(dplyr)`, etc.).

    Start from scratch and assume required input SDTM datasets are available (e.g., DM, EX, DS, etc.).

    For reference, use the {admiral} user guide: https://github.com/pharmaverse/admiral/blob/main/inst/cheatsheet/cheatsheet_image.png。

    For reference, use the {admiral} ADSL creation guide: https://pharmaverse.github.io/admiral/articles/adsl.html.

    Return only the R code. Do not include explanations or summaries.
)--"
    
    chat_obj <- ellmer::chat_openai(system_prompt, model = "gpt-4o",
                                    params = params(temperature = 0, seed = 1234),
                                    api_key = Sys.getenv("OPENAI_API_KEY"))
    ragnar::ragnar_register_tool_retrieve(chat_obj, store_ro)
    
    # Set up user question
    user_prompt <- paste0(
      "Based on the ADSL specification, return {admiral} R code for variables",
      paste0(df$`Variable Name`, collapse = ", ")
    )
    
    # Generate and display code
    rv$output_code <- chat_obj$chat(user_prompt, echo = "none")
    
    output$admiral_code <- renderText({
      req(rv$output_code)
      rv$output_code
    })
  })
}

shinyApp(ui, server)
