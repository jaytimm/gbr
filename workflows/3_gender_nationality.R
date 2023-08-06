
library(dplyr)

setwd('/home/jtimm/pCloudDrive/free-corpus/raw-scrapes')
wiks0 <- readRDS('author-wikis.rds') |> 
  group_by(author) |> mutate(n = row_number()) |> ungroup() |>
  mutate(author_n = paste0(author, '_', n)) |>
  data.table::setDT()


dems <- xml2::read_html('https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations')
dems_df <- dems |> 
  rvest::html_node("table") |>
  rvest::html_table() |>
  janitor::clean_names() |>
  
  mutate(country_entity_name = gsub('England', 
                                    'United Kingdom of Great Britain and Northern Ireland', 
                                    country_entity_name)) |>
  
  mutate(adjectivals = ifelse(adjectivals == 'New Zealand', 'Kiwi', adjectivals)) |>
  
  mutate(adjectivals = gsub('\\[[0-9a-z]*\\]', '_', adjectivals)) |>
  mutate(adjectivals = gsub('([a-z])([A-Z])', '\\1_\\2', adjectivals)) |>
  tidyr::separate_rows(adjectivals, sep = '_') |>
  
  filter(adjectivals != '', nchar(adjectivals) < 100) |>
  distinct(adjectivals, .keep_all = T) |>
  arrange(country_entity_name, adjectivals) |>
  group_by(country_entity_name) |>
  mutate(n = row_number()) |> ungroup() |>
  mutate(dem = ifelse(n == 1, adjectivals, NA)) |>
  tidyr::fill(dem) |>
  rename(country = country_entity_name) |>
  select(country, dem, adjectivals) |>
  filter(!adjectivals %in% c('United States', 'United Kingdom', 'UK', 'U.S.'))

ds <- paste0('\\b', dems_df$adjectivals, '\\b')
ds0 <- paste0(ds, collapse = '|')

wiks0[, text1 := gsub('(\\b[A-Za-z]*-)([A-Za-z]*\\b)', '\\2', text)] # African-American --> American

found <- stringi::stri_locate_all(wiks0$text1, regex = ds0)
names(found) <- paste0(wiks0$author, '_', wiks0$n)
found1 <- lapply(found, data.frame)
df <- data.table::rbindlist(found1, idcol='author_n', use.names = F)

df <- subset(df, !is.na(start))
df <- wiks0[df, on = 'author_n' ]
df[, adjectivals := stringi::stri_sub(text1, start, end)]

df0 <- df |> 
  group_by(author) |>
  slice(1) |> ungroup() |>
  left_join(dems_df)
  

#### gender
prons <- c('he', 'his', 'she', 'her', 'him')
ps <- paste0('\\b', prons, '\\b')
ps0 <- paste0(ps, collapse = '|')

found1 <- stringi::stri_locate_all(wiks0$text, regex = ps0)
names(found1) <- paste0(wiks0$author, '_', wiks0$n)
found2 <- lapply(found1, data.frame)
pdf <- data.table::rbindlist(found2, idcol='author_n', use.names = F)
pdf <- subset(pdf, !is.na(start))

pdf <- wiks0[pdf, on = 'author_n']
pdf[, prons := stringi::stri_sub(text, start, end)]

full <- pdf |> 
  group_by(author) |>
  slice(1) |> ungroup() |>
  mutate(gender = ifelse(prons %in% c('he', 'his', 'him'), 'M', 'F')) |>
  select(author, gender) |>
  inner_join(df0) |> 
  select(author, gender, dem, link) |>
  rename(nationality = dem) |>
  
  
  ######
  inner_join(gutenbergr::gutenberg_authors |> 
               select(gutenberg_author_id, author, birthdate, deathdate) |>
               distinct(.keep_all = T),
             by = c('author')) |>
  
  filter(birthdate > 1700) |>
  
  ## 
  group_by(link) |>
  mutate(n = n()) |> ungroup() |>
  filter(n < 2) |> select(-n) |>
  
  select(gutenberg_author_id, author:nationality, birthdate, deathdate, link)


gens <- read.csv('https://raw.githubusercontent.com/jaytimm/AmericanGenerations/main/data/pew-plus-strauss-generations.csv') |>
  mutate(order = row_number())

full$generation <- gens$generation[
  findInterval(x = full$birthdate, vec = gens$start)]

full0 <- full |>
  mutate(generation = ifelse(nationality != 'American', NA, generation)) 

setwd('/home/jtimm/pCloudDrive/free-corpus/data')
write.csv(full0, 'author_details.csv', row.names = F)
