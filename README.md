# User friendly extensions to Project Gutenberg

> I remembered the glowworms and millipeds.  
> I remembered the spectre.  
> I remembered the dressmaker’s smile!!

## Intro

Working with raw text files from Project Gutenberg. Some simple,
parallelized functions for text extraction, sentence tokenization, and
simple search. Extensions assume corpus has been downloaded, lives
locally.

See [this repository](https://github.com/pgcorpus/gutenberg) for a
quick/easy download of full Project Gutenberg corpus. No need to process
the raw files. Output includes a nice meta file summarizing text
included in download.

## Extraction & sentence tokenization

### Extraction function

Taken from [this R
package](https://github.com/patperry/r-corpus/blob/master/R/gutenberg.R),
and tweaked ever so slightly.

``` r
pg_extract_text <- function(lines){
    # number the lines
    i <- seq_along(lines)

    # find the empty lines
    empty <- (lines == "")

    # find the end of the Project Gutenberg header
    start_pat <- paste("^[*][*][*].*PROJECT GUTENBERG.*[*][*][*]",
                       "END.*SMALL PRINT", sep = "|")
    start_match <- grep(start_pat, lines)
    if (length(start_match) == 0) {
        start_match <- 0
    }
    start <- start_match[[1]] + 1

    # look for the encoding, and convert to UTF-8
    enc_pat <- "Character set encoding:[[:space:]]*(.*)[[:space:]]*$"
    enc_match <- grep(enc_pat, lines[seq_len(start - 1)])
    if (length(enc_match) > 0) {
        enc <- sub(enc_pat, "\\1", lines[enc_match[[1]]])
        if (!enc %in% c("ASCII", "UTF-8")) {

          tryCatch({
            lines <- iconv(lines, enc, "UTF-8")},
            error = function(e) {lines = "REMOVE"})
        }
    }

    start <- min(which(start <= i & !empty))

    # find the start of the Project Gutenberg footer
    end_pat <- paste("^End of .*Project Gutenberg.*",
                     "\\\\*\\\\*\\\\*.*END OF.*PROJECT GUTENBERG", sep = "|")

    end_match <- grep(end_pat, lines)
    if (length(end_match) == 0) {
        end_match <- length(lines) + 1
    }
    end <- end_match[[1]] - 1

    # skip the empty lines at the end
    end <- max(which(i <= end & !empty))

    # skip the production notes at the start of the text
    note_start_pat <- paste("produced by",
                            "prepared by",
                            "transcribed from",
                            "project gutenberg",
                            "^[*][*][*]",
                            "^note: ",
                            "^special thanks",
                            "^this is a retranscription",
                            sep = "|")
    note_start <- grep(note_start_pat, lines, ignore.case = TRUE)
    note_start <- note_start[start <= note_start & note_start <= end]

    ## error happens in here -- 
    while (length(note_start) && note_start[[1]] == start) {
        # the note ends at the first empty line
        note_end <- min(which(note_start[[1]] <= i & empty))

        start <- min(which(note_end + 1 <= i & !empty))
        note_start <- note_start[start <= note_start]
    }

    # concatenate the content lines
    if(!is.finite(start)){'REMOVE'} else{
      paste(lines[start:end], collapse = "\n")
     }
}
```

### Sentence tokenization & output function

``` r
pg_tokenize_sentences <- function(x){
  
  lapply(x, function(y){
    id <- gsub('^.*/', '',  y)
    id <- gsub('_.*$', '', id)
    lines <- readLines(y, 
                       encoding = "UTF-8", 
                       warn = FALSE) 
    
    lines0 <- pg_extract_text(lines)
    
    if(lines[1] == 'REMOVE'){} else{
      
      tryCatch({
        x0 <- data.frame(doc_id = id,
                         text = lines0) |>
          text2df::tif2sentence() |> 
          data.table::setDT()
        
        x0[, text := trimws(gsub('\n', ' ', text))]
        #setwd(out_dir)
        saveRDS(x0, paste0(id, '.rds'))
        },
        
        error = function(e) {})
      }
  })
}
```

### Batches, parallel

``` r
pafs <- list.files(path = path_to_raw_pg_files,
                   full.names = T)

batches <- split(pafs, ceiling(seq_along(pafs)/50))

setwd(path_to_out_folder)

clust <- parallel::makeCluster(7)
parallel::clusterExport(cl = clust,
                        varlist = c('pg_tokenize_sentences',
                                    'pg_extract_text',
                                    'batches'),
                        envir = environment())

pbapply::pblapply(X = batches, 
                  FUN = pg_tokenize_sentences, 
                  cl = clust)

parallel::stopCluster(clust)
```

``` r
setwd(path_to_meta)
pg_meta <- read.csv('metadata.csv')
```

## Package

``` r
devtools::install_github("jaytimm/gbr")
```

### Metadata

``` r
subs <- gbr::gb_subset(x = 10,
                       min_birth_year = 1850,
                       max_birth_year = 1900,
                       subj = 'Science fiction',
                       lang = 'en',
                       filepath = path_to_out_folder) 
```

### Build corpus

``` r
library(dplyr)
mcorp <- subs |> 
  gbr::gb_build_corpus() 

mcorp |> sample_n(5) |> knitr::kable()
```

| doc_id       | text                                                                                                     |
|:--------|:--------------------------------------------------------------|
| PG69257.338  | She snapped her bag shut and stood up.                                                                   |
| PG56338.75   | I came to these hills just to develope the soul.”                                                        |
| PG22357.3127 | Daddy departed upon his mission intending to be gruff, and my word, he can be quite gruff when he likes! |
| PG18172.252  | He paused.                                                                                               |
| PG67121.4247 | Zoar laughed.                                                                                            |

### Simple regex search

``` r
gbr::gb_subset(x = 5000) |>
  gbr::gb_search(pattern = '^I remembered', cores = 12) |>
  gbr::gb_examples(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                  |
|------------------:|:----------------------------------------------------|
|              1799 | I remembered all that, you see,” said the fisherman.  |
|              1806 | I remembered this fact, and resolved to profit by it. |
|              1810 | I remembered it at once.                              |
|              1814 | I remembered my companion’s remark, and felt no fear. |
|              1818 | I remembered I had been a Sioux warrior.              |
|              1819 | I remembered my vow to you.                           |
|              1821 | I remembered him quite well, however.                 |
|              1822 | I remembered her parting words.                       |
|              1824 | I remembered at last.                                 |
|              1828 | I remembered it yesterday, when you had left.         |
|              1838 | I remembered Marny’s warning and kept still.          |
|              1850 | I remembered that I belonged to France.               |
|              1857 | I remembered that day.                                |
|              1860 | I remembered his modesty about Aqua Marine.           |
|              1861 | I remembered the pistol in my box.                    |
|              1862 | I remembered something from “Alice.                   |
|              1863 | I remembered my dream, and hurried back to the house. |
|              1864 | I remembered–ha!                                      |
|              1867 | I remembered that we were alone in the place.         |
|              1869 | I remembered then certain other little things.        |
|              1870 | I remembered seeing barges in Bensersiel harbour.     |
|              1873 | I remembered but one conversation during that wait.   |
|              1875 | I remembered her shy timidity, her innocence.         |
|              1876 | I remembered the niblick shot on the fourteenth.      |
|              1879 | I remembered now.                                     |
|              1880 | I remembered Kennedy’s parting words.                 |
|              1881 | I remembered well the time—I had a sore foot.         |
|              1886 | I remembered all this.                                |
|              1899 | I remembered the anguished screams.                   |
|              1915 | I remembered hearing blasters rip occasionally.       |
