# pak::pak("tidyverse/ragnar")

library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url)

store_location <- "r4ds.ragnar.duckdb"
unlink(store_location)

Sys.setenv(OPENAI_API_KEY = "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")

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small",
                                    api_key = Sys.getenv("OPENAI_API_KEY"))
)


for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |>
    ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
    dplyr::mutate(link = page) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(text = glue::glue(r"---(
    # Excerpt from the book "R for Data Science (2e)"
    link: {link}
    chapter: {h1}
    section: {h2}
    subsection: {h3}
    content: {text}

    )---"))
  
  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

store <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "How can I subset a dataframe with a logical vector?"


## Retrieving Chunks
# Once the store is set up, retrieve the most relevant text chunks like this

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks

embedding_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks

bm25_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")


system_prompt <- stringr::str_squish(r"--(
    You are an expert R programmer and mentor.
    You often respond by first direct quoting material from book or documentation,
    then adding your own additional context and interpertation.
)--")
chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")
# chat <- ellmer::chat_ollama(system_prompt, model = "llama3.2:1b")

ragnar_register_tool_retrieve(chat, store)

chat$chat("How can I subset a dataframe?")
