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
  titlePanel("Clinical RAG Agent"),
  layout_columns(
    col_widths = c(4, 8),
    
    # Sidebar with upload and chat
    card(
      fileInput("doc_file", "Upload Clinical Document",
                accept = c(".pdf", ".xlsx", ".xls", ".docx")),
      hr(),
      chat_ui("chat"),
      style = "height: 600px; overflow-y: auto;"
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
  Sys.setenv(OPENAI_API_KEY = "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")
  
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
    if (ext %in% c("pdf", "docx")) {
      output$doc_viewer <- renderUI({
        tags$iframe(
          src = file.path("docs", safe_name),
          width = "100%",
          height = "800px",
          style = "border: none;"
        )
      })
      
      # Vector store the uploaded document
      pages <- ragnar::ragnar_read(dest_path)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(dest_path)
      output$doc_viewer <- renderUI({
        datatable(df, options = list(scrollX = TRUE, pageLength = 15))
      })
      
      # Vector store the uploaded document
      pages <- data.frame()
      for (i in 1:5) {
        row_df <- df[((i-1)*10+1):(i*10), ]
        tmp_file <- tempfile(fileext = ".xlsx")
        write.xlsx(row_df, tmp_file)
        pages <- rbind(pages, ragnar_read(tmp_file))
      }
    }
    
    ragnar::ragnar_store_insert(store, pages)
    ragnar::ragnar_store_build_index(store)
    
    # Optionally connect again as read-only
    store_ro <- ragnar::ragnar_store_connect(store_location, read_only = TRUE)
    
    # Set up system prompt and chat
    system_prompt <- stringr::str_squish(r"--(
      You are an experienced clinical data analyst and medical writer.
      Your job is to read a document that contains tables and narratives,
      extract the information from the document,
      and answer the user's question based on the document.
    )--")
    
    chat_obj <- ellmer::chat_openai(system_prompt, model = "gpt-4o",
                                    api_key = Sys.getenv("OPENAI_API_KEY"))
    ragnar::ragnar_register_tool_retrieve(chat_obj, store_ro)
    
    # Save the chat object in a reactive value if needed later
    rv$chat <- chat_obj
  })
  

  
  observeEvent(input$chat_user_input, {
    req(rv$chat)
    stream <- rv$chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)
