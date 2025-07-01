# Load packages
library(ragnar)
library(ellmer)
library(readxl)
library(knitr)
library(openxlsx)

# Load specification file
base_url <- "/mnt/R/Workplace/zwu/shiny.rag/inst/test_doc/adsl_spec.xlsx"
df <- read_excel(base_url, sheet = "ADSL")[1:50, ]

# tmp_file <- tempfile(fileext = ".xlsx")
# write.xlsx(df, tmp_file)
# pages <- ragnar_read(tmp_file)

# Process and read document
pages <- data.frame()
for (i in 1:5) {
  row_df <- df[((i - 1)*10 + 1):(i*10), ]
  tmp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(row_df, tmp_file)
  pages <- rbind(pages, ragnar_read(tmp_file))
}

# Create storage (embedding)
store_location <- "r4ds3.ragnar.duckdb"
unlink(store_location)

Sys.setenv(OPENAI_API_KEY = "")

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small",
                                    api_key = Sys.getenv("OPENAI_API_KEY"))
)

# Store document (embedding)
ragnar_store_insert(store, pages)

ragnar_store_build_index(store)

store <- ragnar_store_connect(store_location, read_only = TRUE)

# system_prompt <- r"--(
#     You are an expert in creating CDISC ADaM datasets using the {admiral} R package. You follow CDISC and {admiral} best practices.
# 
#     You have been provided with a multi-sheet Excel specification file containing derivation rules for several ADaM datasets (e.g., ADSL, ADLB, ADAE, etc.).
# 
#     Focus ONLY on the sheet related to the **ADSL** dataset. Ignore all other sheets.
# 
#     Your task is to:
#     - Read the ADSL specification sheet.
#     - Generate R code using {admiral} to implement the derivation logic (computational method) for all variables in the ADSL dataset.
#     - Generate R code based on variable type and length.
#     - Use functions in {admiral} package like `derive_vars_dtm()`, `derive_vars_merged()`, `derive_vars_duration()`, `derive_vars_extreme_event()`, etc., as appropriate.
#     - Follow {admiral} coding style: pipe-based workflows (`%>%`), clean formatting, clear comments.
#     - Include relevant library calls (`library(admiral)`, `library(dplyr)`, etc.).
#     - Return only the R code. Do not include explanations or summaries.
# 
#     Start from scratch and assume required input SDTM datasets are available (e.g., DM, EX, DS, etc.).
# 
#     For reference, use the {admiral} user guide: https://github.com/pharmaverse/admiral/blob/main/inst/cheatsheet/cheatsheet_image.png。
# 
#     For reference, use the {admiral} ADSL creation guide: https://pharmaverse.github.io/admiral/articles/adsl.html.
# 
#     Use only the content from the ADSL sheet of the uploaded Excel file as your reference.
# )--"

# Define system prompt
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

# Create chat object
chat <- ellmer::chat_openai(system_prompt,
                            model = "gpt-4o",
                            params = params(temperature = 0, seed = 1234),
                            api_key = Sys.getenv("OPENAI_API_KEY"))

# Retrieve based on embedding
ragnar_register_tool_retrieve(chat, store)

# Return the chat result
chat$chat(paste0("Based on the ADSL specification, return {admiral} R code for variables",
                 paste0(df$`Variable Name`, collapse  = ", ")))
