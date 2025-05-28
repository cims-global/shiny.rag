library(ragnar)
library(ellmer)
library(readxl)
library(knitr)
library(openxlsx)

base_url <- "/mnt/R/Workplace/zwu/shiny.rag/inst/test_doc/xxx.xlsx"
df <- read_excel(base_url, sheet = "ADSL")[1:50, ]

tmp_file <- tempfile(fileext = ".xlsx")
write.xlsx(df, tmp_file)
pages <- ragnar_read(tmp_file)

store_location <- "r4ds.ragnar.duckdb"
unlink(store_location)

Sys.setenv(OPENAI_API_KEY = "")

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small",
                                    api_key = Sys.getenv("OPENAI_API_KEY"))
)

ragnar_store_insert(store, pages)

ragnar_store_build_index(store)

store <- ragnar_store_connect(store_location, read_only = TRUE)

system_prompt <- r"--(
    You are an expert in creating CDISC ADaM datasets using the {admiral} R package. You follow CDISC and {admiral} best practices.

    You have been provided with a multi-sheet Excel specification file containing derivation rules for several ADaM datasets (e.g., ADSL, ADLB, ADAE, etc.).

    Focus ONLY on the sheet related to the **ADSL** dataset. Ignore all other sheets.

    Your task is to:
    - Read the ADSL specification sheet.
    - Generate R code using {admiral} to implement the derivation logic (computational method) for all variables in the ADSL dataset.
    - Generate R code based on variable type and length.
    - Use functions in {admiral} package like `derive_vars_dtm()`, `derive_vars_merged()`, `derive_vars_duration()`, `derive_vars_extreme_event()`, etc., as appropriate.
    - Follow {admiral} coding style: pipe-based workflows (`%>%`), clean formatting, clear comments.
    - Include relevant library calls (`library(admiral)`, `library(dplyr)`, etc.).
    - Return only the R code. Do not include explanations or summaries.

    Start from scratch and assume required input SDTM datasets are available (e.g., DM, EX, DS, etc.).

    For reference, use the {admiral} user guide: https://github.com/pharmaverse/admiral/blob/main/inst/cheatsheet/cheatsheet_image.png。

    For reference, use the {admiral} ADSL creation guide: https://pharmaverse.github.io/admiral/articles/adsl.html.

    Use only the content from the ADSL sheet of the uploaded Excel file as your reference.
)--"

chat <- ellmer::chat_openai(system_prompt,
                            model = "gpt-4o",
                            seed = 1234,
                            api_args = list(temperature = 0),
                            api_key = Sys.getenv("OPENAI_API_KEY"))

ragnar_register_tool_retrieve(chat, store)

chat$chat(paste0("Based on the ADSL specification, return {admiral} R code for variables ",
                 paste0(df$`Variable Name`, collapse  = ", ")))
