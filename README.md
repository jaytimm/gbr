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
subs |> 
  gbr::gb_build_corpus() |>
  dplyr::sample_n(5) |> 
  knitr::kable()
```

| doc_id       | text                                                                                                                                           |
|:------|:----------------------------------------------------------------|
| PG59774.1493 | There were no matches there, and I suddenly realised that my extremities were cold.                                                            |
| PG21638.617  | And I don’t think, at such a time, I should trust Robins to bring them.”                                                                       |
| PG60374.1175 | “Wallabaloo,” said the Clockwork man, faintly, “Wum–Wum–” “Yes, we know all about that,” said the constable, “but you take my tip and go ’ome. |
| PG59774.5953 | There followed a period of silence.                                                                                                            |
| PG624.1562   | The prophet, who had guessed the meaning of God, must dicker for the price of the revelation, and the poet hawk his visions in printers’ row.  |

### Simple regex search

``` r
gbr::gb_subset(x = 5000) |>
  gbr::gb_search(pattern = '^I remembered', cores = 12) |>
  gbr::gb_examples(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1743 | I remembered seeing some empty casks in the hold.      |
|              1799 | I remembered the dressmaker’s smile!                   |
|              1803 | I remembered the words of the ostler in the mountains. |
|              1814 | I remembered my companion’s remark, and felt no fear.  |
|              1824 | I remembered there were rats in it.                    |
|              1830 | I remembered, aloud and abruptly.                      |
|              1831 | I remembered all that when I opened my eyes again.     |
|              1841 | I remembered so many things–oh, so many!               |
|              1846 | I remembered your face.                                |
|              1850 | I remembered everything now.                           |
|              1852 | I remembered his warning to me and fled.               |
|              1855 | I remembered yesterday’s doings, and groaned.          |
|              1857 | I remembered my vow.                                   |
|              1861 | I remembered the pistol in my box.                     |
|              1863 | I remembered Eloise’s words: “Little Carl is a girl.   |
|              1864 | I remembered you very well–very well, indeed.          |
|              1865 | I remembered it last night.                            |
|              1867 | I remembered that we were alone in the place.          |
|              1869 | I remembered then certain other little things.         |
|              1870 | I remembered him perfectly.                            |
|              1874 | I remembered he lived in Jermyn Street.                |
|              1876 | I remembered the niblick shot on the fourteenth.       |
|              1879 | I remembered it all but the shining cup.               |
|              1882 | I remembered when Dr. Maynard had taken that picture.  |
|              1885 | I remembered you from Monte Carlo, you see.            |
|              1887 | I remembered the gentleness of the man with her.       |
|              1892 | I remembered exactly what movements I had to make.     |
|              1902 | I remembered.                                          |
|              1909 | I remembered suddenly.                                 |
|              1911 | I remembered how he resembled the lanee-assistant.     |
|              1924 | I remembered now.                                      |
