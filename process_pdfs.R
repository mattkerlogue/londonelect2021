library(tidyverse)

# Get PDFs

url <- "https://www.londonelects.org.uk/im-voter/election-results/results-2021"

page <- xml2::read_html(url)

pdf_list <- page %>% 
  rvest::html_nodes(".media-download a") %>%
  rvest::html_attr("href") %>%
  purrr::map_chr(., ~paste0("https://www.londonelects.org.uk", .))

dir.create("pdfs")

download_pdf <- function(url) {
  
  file <- basename(url)
  file <- gsub("%20", "_", file)
  
  download.file(url, file.path("pdfs", file))
  
}


purrr::walk(pdf_list, download_pdf)


filter_y_val <- function(filter_text, df) {
  df %>%
    filter(text == filter_text) %>%
    pull(y)
}

filter_x_val <- function(filter_text, df) {
  df %>%
    filter(text == filter_text) %>%
    pull(x)
}

# Convert PDFs to datasets

extract_constituency_data <- function(pdf_file) {
  
  # convert pdf to tibble
  x <- pdftools::pdf_data(pdf_file)
  
  # get constituency
  constituency <- x[[1]] %>%
    filter(y == filter_y_val("Constituency", df = x[[1]]) & 
             text != "Constituency") %>%
    pull(text) %>%
    paste0(collapse = " ")
  
  ## constituency candidates
  
  # page 1 blocks
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

  # subset tibble to just constituency results
  constituency_df <- blocks %>%
    filter(y > filter_y_val("(if any)", blocks) & 
             y < filter_y_val("Total number of good votes", blocks)) %>%
    mutate(
      var = case_when(
        x == filter_x_val("Name of Candidates", blocks) ~ "candidate",
        x == filter_x_val("Name of Registered Political Party", blocks) ~ "party",
        type == "numeric" ~ "votes"
      )) %>%
    select(y, text, var) %>%
    pivot_wider(names_from = var, values_from = text) %>%
    drop_na(votes) %>%
    mutate(
      constituency = constituency,
      vote_type = "constituency")
  
  # consitutency metadata
  constituency_meta <- blocks %>%
    filter(
      y == filter_y_val("Total number of ballot papers counted", blocks) |
        y == filter_y_val("Total number of good votes", blocks) |
        y == filter_y_val("(a) Unmarked", blocks) |
        y == filter_y_val("(b) Uncertain", blocks) |
        y == filter_y_val("(c) Voting for too many", blocks) |
        y == filter_y_val("(d) Writing identifying voter", blocks) |
        y == filter_y_val("(e) Want of official mark", blocks) |
        y == filter_y_val("Total number of Rejected ballots", blocks) |
        y == filter_y_val("System Logged Electorate", blocks) |
        y == filter_y_val("Turnout", blocks)) %>%
    mutate(
      type = if_else(
        y == filter_y_val("Turnout", blocks) & text != "Turnout",
        "numeric",
        type)) %>%
    select(y, text, type) %>%
    pivot_wider(names_from = type, values_from = text) %>%
    select(metadata = character, value = numeric) %>%
    mutate(
      constituency = constituency,
      vote_type = "constituency")
  
  
  ## get london-wide votes
  
  # page 2 blocks
  blocks <- x[[2]] %>%
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
  
  # subset tibble to just party info
  london_df <- blocks %>%
    filter(y > filter_y_val("Votes", blocks) 
           & y < filter_y_val("Total number of good votes", blocks)) %>%
    mutate(
      var = case_when(
        x == filter_x_val("Name of Registered Political Party or Independent") ~ "party",
        type == "numeric" ~ "votes"
      )) %>%
    select(y, text, var) %>%
    pivot_wider(names_from = var, values_from = text) %>%
    drop_na(votes) %>%
    mutate(
      constituency = constituency,
      vote_type = "londonwide",
      party = str_remove(party, " \\(.*$"))
  
  # london metadata
  london_meta1 <- blocks %>%
    filter(
      y == filter_y_val("Total number of ballot papers counted", blocks) |
        y == filter_y_val("Total number of good votes", blocks) |
        y == filter_y_val("(a) Unmarked", blocks))
    
  # page 3 blocks
  blocks <- x[[3]] %>%
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
  
  # get metadata on page 2
  london_meta2 <- blocks %>%
    filter(
      y == filter_y_val("(b) Uncertain", blocks) |
        y == filter_y_val("(c) Voting for too many", blocks) |
        y == filter_y_val("(d) Writing identifying voter", blocks) |
        y == filter_y_val("(e) Want of official mark", blocks) |
        y == filter_y_val("Total", blocks) |
        y == filter_y_val("System Logged Electorate", blocks) |
        y == filter_y_val("Turnout", blocks))
  
  # join london metadata
  london_meta <- bind_rows(london_meta1, london_meta2) %>%
    mutate(
      type = if_else(
        y == filter_y_val("Turnout", blocks) & text != "Turnout",
        "numeric",
        type)) %>%
    select(y, text, type) %>%
    pivot_wider(names_from = type, values_from = text) %>%
    select(metadata = character, value = numeric) %>%
    mutate(
      constituency = constituency,
      vote_type = "londonwide")
  
  ## get mayor preferences
  
  # page 4 blocks
  blocks <- x[[4]] %>%
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
  
  # subset tibble to just mayor info 
  mayor_df <- blocks %>%
    filter(y > filter_y_val("Votes", blocks) & 
             y < filter_y_val("Page 1 of 2", blocks)) %>%
    mutate(
      var = case_when(
        x == filter_x_val("Name of Candidates", blocks) ~ "candidate",
        x == filter_x_val("Political Party (if any)", blocks) ~ "party",
        type == "numeric" & x < filter_x_val("Second", blocks) ~ "mayor_first",
        type == "numeric" & x > filter_x_val("Second", blocks) ~ "mayor_second"
      )) %>%
    select(y, text, var) %>%
    pivot_wider(names_from = var, values_from = text) %>%
    pivot_longer(cols = c(mayor_first, mayor_second), 
                 names_to = "vote_type", values_to = "votes") %>%
    drop_na(votes) %>%
    mutate(constituency = constituency)
  
  
  mayor_ballots <- blocks %>%
    filter(y < filter_y_val("Votes", blocks) & type == "numeric") %>%
    transmute(
      metadata = "Total number of ballot papers counted",
      value = text,
      constituency = constituency
    )
  
  mayor_meta1 <- bind_rows(mayor_ballots, mayor_ballots) %>%
    mutate(vote_type = c("mayor_first", "mayor_second"))
  
  # page 5 blocks
  blocks <- x[[5]] %>%
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
  
  mayor_good_votes <- blocks %>%
    filter(y == filter_y_val("Total number of good votes", blocks) & type == "numeric") %>%
    transmute(
      metadata = "Total number of good votes",
      value = text,
      constituency = constituency,
      vote_type = c("mayor_first", "mayor_second")
    )
  
  mayor_meta2 <- blocks %>%
    filter(y < filter_y_val("The number of ballot papers second preference unused was as follows: -", blocks) & 
             y > filter_y_val("Total number of good votes", blocks)) %>%
    filter(y == filter_y_val("(a) Unmarked", df = .) |
             y == filter_y_val("(b) Uncertain", df = .) |
             y == filter_y_val("(c) Voting for too many", df = .) |
             y == filter_y_val("(d) Writing identifying voter", df = .) |
             y == filter_y_val("(e) Want of official mark", df = .) |
             y == filter_y_val("Total", df = .)) %>%
    select(y, text, type) %>%
    pivot_wider(names_from = type, values_from = text) %>%
    select(metadata = character, value = numeric) %>%
    mutate(
      constituency = constituency,
      vote_type = "mayor_first")
  
  mayor_meta3 <- blocks %>%
    filter(y > filter_y_val("The number of ballot papers second preference unused was as follows: -", blocks) & 
             y < filter_y_val("System Logged Electorate", blocks)) %>%
    filter(y == filter_y_val("(a) Unmarked", df = .) |
             y == filter_y_val("(a) Uncertain", df = .) |
             y == filter_y_val("(b) Voting for too many", df = .) |
             y == filter_y_val("Total", df = .)|
             y == filter_y_val("The total number of valid ballot papers on which the", df = .)) %>%
    mutate(text = if_else(
      y == max(y) & type == "character",
      "Second preference matched first preference",
      text)) %>%
    select(y, text, type) %>%
    pivot_wider(names_from = type, values_from = text) %>%
    select(metadata = character, value = numeric) %>%
    mutate(
      metadata = case_when(
        metadata == "(a) Uncertain" ~ "(b) Uncertain",
        metadata == "(b) Voting for too many" ~ "(c) Voting for too many",
        TRUE ~ metadata),
      constituency = constituency,
      vote_type = "mayor_second")
  
  mayor_electorate <- blocks %>%
    filter(y >= filter_y_val("System Logged Electorate", blocks) & 
             y < filter_y_val("Signed - Constituency Returning Officer", blocks)) %>%
    mutate(
      type = if_else(
        y == filter_y_val("Turnout", df = .) & text != "Turnout",
        "numeric",
        type
    )) %>%
    select(y, text, type) %>%
    pivot_wider(names_from = type, values_from = text) %>%
    select(metadata = character, value = numeric) %>%
    mutate(constituency = constituency)
  
  mayor_meta4 <- bind_rows(mayor_electorate, mayor_electorate) %>%
    mutate(vote_type = paste("mayor", c(rep("first", 2), rep("second", 2)), sep = "_"))
  
  mayor_meta <- bind_rows(mayor_meta1, mayor_meta2, mayor_meta3, mayor_meta4)
  
  # combine results dfs
  results_df <- bind_rows(constituency_df, london_df, mayor_df)
    
  # combine metadata
  metadata_df <- bind_rows(constituency_meta, london_meta, mayor_meta)
  
  return(list(results = results_df, metadata = metadata_df))
  
}

extract_mayor_preference_data <- function(pdf_file) {
  
  x <- pdftools::pdf_data(pdf_file)
  
  pg5 <- x[[5]] %>%
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
  
  pg6 <- x[[6]] %>%
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
  
  pg7 <- x[[7]] %>%
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
  
  pg8 <- x[[8]] %>%
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
  
  pages <- bind_rows(pg5, pg6, pg7, pg8, .id = "page") %>%
    mutate(page = as.numeric(page) + 4)
  
  candidates <- pages %>%
    filter(page == 5 | page == 6) %>%
    filter(y > filter_y_val("1st Choice votes for Candidate", df = .) & 
             y < filter_y_val("2nd Choice votes for Candidate:", df = .)) %>%
    filter(text != "HEWISON") %>%
    mutate(
      text = if_else(text == "Richard John Howard", "Richard John Howard HEWISON", text),
      cell_type = case_when(
        type == "character" ~ "candidate",
        type == "numeric" & as.numeric(text) <= 20 ~ "id",
        type == "numeric" & as.numeric(text) > 20 ~ "votes"
      )
    ) %>%
    arrange(cell_type, page, x, y) %>%
    group_by(cell_type) %>%
    mutate(shape_id = row_number()) %>%
    ungroup() %>%
    select(cell_type, shape_id, text) %>%
    pivot_wider(names_from = cell_type, values_from = text) %>%
    arrange(shape_id) %>%
    mutate(candidate = as_factor(candidate))
  
  second_prefs <- pages %>%
    mutate(
      second_pref = case_when(
        (page == 5 | page == 6) &
          y > filter_y_val("2nd Choice votes for Candidate:", pg6) &
          y < min(filter_y_val("Generated On: 08/05/2021 23:33:17", .)) ~ TRUE,
        page == 7 &
          y > filter_y_val("Mayor of London Contest Votes (PART 2)", pg7) & 
          y < filter_y_val("2", pg7) ~ TRUE,
        page == 8 &
          y < filter_y_val("Total 1st and 2nd Choice votes for Candidate:", pg8) ~ TRUE,
        TRUE ~ FALSE
      )) %>%
    filter(second_pref) %>%
    separate_rows(text, sep = " ") %>%
    mutate(
      cell_value = as.numeric(text),
      cell_type = case_when(
        cell_value <= 20 ~ "id",
        TRUE ~ "votes"
    )) %>%
    arrange(page, y, cell_type, x) %>%
    group_by(page, y, cell_type) %>%
    mutate(pseudo_column = row_number()) %>%
    ungroup() %>%
    mutate(
      pseudo_column = as.numeric(pseudo_column),
      pseudo_column = if_else(page == 6 | page == 8, pseudo_column + 15, pseudo_column),
      col_head = if_else(
        y == filter(., page == 5 | page == 6) %>% pull(y) %>% min(),
        TRUE,
        FALSE
      )
    ) %>%
    group_by(page, col_head) %>%
    mutate(pseudo_row = dense_rank(y)) %>%
    ungroup() %>%
    mutate(
      pseudo_row = as.numeric(pseudo_row),
      pseudo_row = if_else(page == 7 | page == 8, pseudo_row + 11, pseudo_row)
    )
    
  second_pref_votes <- second_prefs %>%
    filter(cell_type == "votes") %>%
    mutate(
      first_vote = candidates$candidate[pseudo_column],
      second_vote = candidates$candidate[pseudo_row]
    ) %>%
    select(first_vote, second_vote, votes = cell_value
    )
    
    
  
}

