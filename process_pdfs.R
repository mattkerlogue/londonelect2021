library(tidyverse)

# Get PDFs

url <- "https://www.londonelects.org.uk/im-voter/election-results/results-2021"

page <- xml2::read_html(url)

pdfs <- page %>% 
  rvest::html_nodes(".media-download a") %>%
  rvest::html_attr("href") %>%
  purrr::map_chr(., ~paste0("https://www.londonelects.org.uk", .))

dir.create("pdfs")

download_pdf <- function(url) {
  
  file <- basename(url)
  file <- gsub("%20", "_", file)
  
  download.file(url, file.path("pdfs", file))
  
}

# Convert PDFs to datasets

extract_const_pdf <- function(pdf) {
  
  x <- pdftools::pdf_data(pdf)
  
  const_y <- x[[1]] %>%
    filter(text == "Constituency") %>%
    pull(y)
  
  constituency <- x[[1]] %>%
    filter(y == const_y, text != "Constituency") %>%
    pull(text) %>%
    paste0(collapse = " ")
  
  blocks <- x[[1]] %>%
    mutate(
      block_start = lag(!space, default = FALSE),
      block_num = 1 + cumsum(block_start)
      ) %>%
    nest_by(block_num) %>%
    mutate(block_text = paste0(data$text, collapse = " "),
           block_x = min(data$x),
           block_y = min(data$y)) %>%
    select(block_num, x = block_x, y = block_y, text = block_text) %>%
    mutate(type = if_else(str_detect(text, "^\\d+$"), "numeric", "character")) %>%
    ungroup()
  
  candidates_head_y <- blocks %>%
    filter(text == "(if any)") %>%
    pull(y)
  
  candidates_head_x <- blocks %>%
    filter(text == "Name of Candidates") %>%
    pull(x)
  
  parties_head_x <- blocks %>%
    filter(text == "Name of Registered Political Party") %>%
    pull(x)
  
  good_votes_y <- blocks %>%
    filter(text == "Total number of good votes") %>%
    pull(y)
  
  candidates_df <- blocks %>%
    filter(y > candidates_head_y & y < good_votes_y) %>%
    mutate(
      var = case_when(
        x == candidates_head_x ~ "candidate",
        x == parties_head_x ~ "party",
        type == "numeric" ~ "votes"
      )) %>%
    select(y, text, var) %>%
    pivot_wider(names_from = var, values_from = text) %>%
    drop_na(votes)
  
  return(df)
  
  
}
