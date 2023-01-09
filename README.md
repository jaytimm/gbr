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
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
mcorp <- subs |> 
  gbr::gb_build_corpus() 

mcorp |> sample_n(5) |> knitr::kable()
```

| doc_id       | text                                                                                                                                                                                                                                                                                                                                                                                                    |
|:---|:-------------------------------------------------------------------|
| PG68815.5617 | “I don’t know,” Kinnison answered the unasked question, “but I can find out.”                                                                                                                                                                                                                                                                                                                           |
| PG8684.1066  | Hostages were taken, as usual, these including Major Henry L. Higginson, President A. Lawrence Lowell of Harvard University, Major James M. Curley, Edward A. Filene, Margaret Deland, William A. Paine, Ellery Sedgwick, Mrs. John L. Gardner, Charles W. Eliot, Louis D. Brandeis, Bishop William Lawrence, Amy Lowell, T. Jefferson Coolidge, Thomas W. Lawson, Guy Murchie, and Cardinal O’Connell. |
| PG68815.716  | It’s quite possible that she was hit hard enough to lay out most of her crew cold–anyway enough of them to put her out of control.                                                                                                                                                                                                                                                                      |
| PG61698.882  | “Don’t worry–I’ll be careful of them.                                                                                                                                                                                                                                                                                                                                                                   |
| PG26637.1331 | “I’ll be perfectly candid with you, Mr. Ewart,” he replied.                                                                                                                                                                                                                                                                                                                                             |

### Simple regex search

``` r
gbr::gb_subset(x = 5000) |>
  gbr::gb_search(pattern = '^I remembered', cores = 12) |>
  gbr::gb_examples(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1745 | I remembered!                                          |
|              1802 | I remembered him well.                                 |
|              1806 | I remembered everything to the smallest incident.      |
|              1814 | I remembered the fire on the prairie.                  |
|              1821 | I remembered thee on the cross.                        |
|              1828 | I remembered it all then.                              |
|              1831 | I remembered that I had salt in the cabin.             |
|              1835 | I remembered Lord Nithisdale’s escape in ’15.          |
|              1838 | I remembered Mr. Yocomb’s helping me to my room.       |
|              1839 | I remembered more after that.                          |
|              1844 | I remembered my fable and flinched.                    |
|              1850 | I remembered that I belonged to France.                |
|              1857 | I remembered that I had left it so when I came to bed. |
|              1859 | I remembered it directly I saw it again.               |
|              1861 | I remembered my experience in the elevated train.      |
|              1862 | I remembered now.                                      |
|              1863 | I remembered.                                          |
|              1864 | I remembered the name at once.                         |
|              1866 | I remembered what you had said.                        |
|              1867 | I remembered not to tell him that.                     |
|              1869 | I remembered a short story I had read once.            |
|              1872 | I remembered, also how I had threatened her.           |
|              1874 | I remembered suddenly my last talk with him.           |
|              1875 | I remembered that I was not yet twenty.                |
|              1880 | I remembered no more.                                  |
|              1881 | I remembered–remembered hard.                          |
|              1887 | I remembered her.                                      |
|              1889 | I remembered that she had left in disgrace.            |
|              1890 | I remembered how you had lost your father.             |
|              1891 | I remembered the wind.                                 |
|              1892 | I remembered without remembering that I remembered.    |
|              1895 | I remembered that I did not know her name.             |
|              1903 | I remembered the strange voice that had instructed me. |
|              1909 | I remembered the dream, the ship with its broken sail. |
|              1933 | I remembered the first Lindbergh.                      |
