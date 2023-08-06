library(dplyr)
setwd('/home/jtimm/pCloudDrive/free-corpus/raw-scrapes')
pubs <- readRDS('pubs_etc.rds')

subjs <- gutenbergr::gutenberg_subjects |>
  group_by(gutenberg_id) |>
  summarize(subject = paste0(subject, collapse = ' | ')) |> ungroup() |>
  mutate(genre = ifelse(grepl('Fiction|fiction', subject), 'Fiction', 'Non-fiction'),
         genre  = ifelse(grepl('Poetry|poetry', subject), 'Poetry', genre)) |>
  select(gutenberg_id, genre)

META <- gutenbergr::gutenberg_metadata |> 
  left_join(gutenbergr::gutenberg_authors) |>
  filter(has_text == T, !is.na(author), !is.na(birthdate)) |>
  select(-language, -gutenberg_bookshelf, -rights, -has_text, 
         -alias, -aliases, -wikipedia) 

pubs0 <- pubs |> na.omit() |>
  mutate(xauthor = gsub(', [0-9 -.]*$', '', author)) |>
  mutate(xauthor = gsub(' [from old catalog]', '', xauthor)) |>
  select(-author) |>
  
  inner_join(META |> select(gutenberg_id, title, gutenberg_author_id, 
                            author, birthdate, deathdate) |>
               distinct(title, .keep_all = T),
             by = c('in_title' = 'title')) |>
  
  mutate(dist = stringdist::stringdist(xauthor, author, method = 'jw')) |>
  filter(dist < 0.25) |>
  
  select(-out_title, -xauthor, -dist) |>
  
  mutate(deathdate = ifelse(is.na(deathdate), 2023, deathdate)) |>
  mutate(in_life = ifelse(pubdate > birthdate & pubdate <= deathdate, 1, 0)) |>
  group_by(author, in_title) |>
  mutate(n = n(), n99 = sum(in_life)) |> ungroup() |>
  
  mutate(pubdate = pubdate |> as.integer(),
         age = pubdate - birthdate) |>
  
  mutate(deathdate = ifelse(deathdate == 2023, NA, deathdate)) |>
  
  filter(age > 20) |>
  filter(in_life == 1)  |>
  
  group_by(author, in_title) |>
  filter(pubdate == min(pubdate)) |> ungroup() |>
  distinct(author, in_title, .keep_all = T) |>
  
  ## left_join(gbr::pg_tokens) |>
  
  distinct(in_title, .keep_all = T) |>
  
  mutate(decade = gsub('[0-9]$', '0', pubdate) |> as.integer()) |>
  na.omit() |> 
  ## filter(tokens > 1000) |>
  select(-n, -n99, -in_life) |>
  
  left_join(subjs) |>
  filter(!is.na(genre)) |>
  rename(title = in_title) |>
  select(gutenberg_id, title, genre, pubdate, decade, gutenberg_author_id, age)

setwd('/home/jtimm/pCloudDrive/free-corpus/data')
write.csv(pubs0, 'book_details.csv', row.names = F)