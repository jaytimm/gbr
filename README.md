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

| doc_id       | text                                                                                                                                                                    |
|:-----|:-----------------------------------------------------------------|
| PG21094.1100 | Then I set her down, and signified to the king I was ready.                                                                                                             |
| PG21094.3295 | From her head in graceful folds hung a thin scarf of gold.                                                                                                              |
| PG60860.2434 | To make matters worse, the day was nearly spent, my ankle pained me exceedingly and my dread of snakes became a factor which contributed much to my nervous excitement. |
| PG21094.3622 | He spoke only a moment, more with the impartial attitude of one who gives advice than as a witness.                                                                     |
| PG67121.1184 | Cautiously he opened his eyes.                                                                                                                                          |

### Simple regex search

``` r
gbr::gb_subset(x = 5000) |>
  gbr::gb_search(pattern = '^I remembered', cores = 12) |>
  gbr::gb_examples(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1800 | I remembered them well.                                |
|              1814 | I remembered a similar case.                           |
|              1818 | I remembered the red blanket on my saddle.             |
|              1822 | I remembered old Balerma, and felt deeply for them.    |
|              1824 | I remembered there were rats in it.                    |
|              1828 | I remembered and repeated the gipsy burden.            |
|              1830 | I remembered, aloud and abruptly.                      |
|              1831 | I remembered that I had salt in the cabin.             |
|              1839 | I remembered more after that.                          |
|              1843 | I remembered the name Waikatakata.                     |
|              1844 | I remembered my fable and flinched.                    |
|              1846 | I remembered the spot well.                            |
|              1847 | I remembered the boy Madame Tank had told about.       |
|              1850 | I remembered I had talked easily with her.             |
|              1851 | I remembered him very well.                            |
|              1855 | I remembered how rich he was.                          |
|              1856 | I remembered that bag.                                 |
|              1859 | I remembered that there was a war.                     |
|              1860 | I remembered that I was near to the great Alps.        |
|              1863 | I remembered.                                          |
|              1865 | I remembered that you are joining your regiment.       |
|              1866 | I remembered many strange things.                      |
|              1867 | I remembered my oath to Susan at Traxelby.             |
|              1870 | I remembered seeing barges in Bensersiel harbour.      |
|              1871 | I remembered,” she said very low, “that night.         |
|              1872 | I remembered the name, Jaime de Oteros.                |
|              1874 | I remembered that our name means ‘right.’              |
|              1875 | I remembered the Italian perfectly.                    |
|              1876 | I remembered afterward that luck was the word he used. |
|              1878 | I remembered my children.                              |
|              1879 | I remembered what my eyes had seen, my ears heard.     |
|              1885 | I remembered our trifling on that railway platform….   |
|              1890 | I remembered how you had lost your father.             |
|              1909 | I remembered the sequence of Jane’s tiger.             |
|              1910 | I remembered from our history; she was smoking.        |
|              1933 | I remembered the first Lindbergh.                      |
