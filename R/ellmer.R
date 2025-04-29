# install.packages("ellmer")

library(ellmer)

setwd("/mnt/R/Workplace/zwu/stat2mw.rshiny/inst/extdata1")

adam_list = list(
  adae = readRDS("adae.rds"),
  adsl = readRDS("adsl.rds")
)

t_b_ds_01 <- cimstfl::t_b_ds_01(adam_list)$ard

tbl_text <- paste(capture.output(t_b_ds_01), collapse = "\n")

chat <- ellmer::chat_deepseek(system_prompt = paste("You're a clinical data analyst who can analyze dataset and tables.",
                                                    "Here is the table: \n", tbl_text),
                              api_key = "",
                              model = "deepseek-chat")

live_console(chat)
