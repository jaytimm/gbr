# User friendly extensions to Project Gutenberg

> I remember the dressmaker’s smile!!

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

## Simple regex search

``` r
devtools::install_github("jaytimm/gbr")
```

``` r
gbr::gbsearch(pattern = '^I remembered',
              book_sample = 10000,
              x = path_to_out_folder) |>
  
  gbr::gbclean(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1763 | I remembered the glowworms and millipeds.              |
|              1793 | I remembered the spectre.                              |
|              1799 | I remembered the dressmaker’s smile!                   |
|              1805 | I remembered it first.                                 |
|              1806 | I remembered everything to the smallest incident.      |
|              1807 | I remembered it, it was sich an outlandish name like.  |
|              1814 | I remembered my companion’s remark, and felt no fear.  |
|              1819 | I remembered that you had once given me hope.          |
|              1821 | I remembered him quite well, however.                  |
|              1824 | I remembered nothing but the horror of his situation.  |
|              1828 | I remembered it yesterday, when you had left.          |
|              1830 | I remembered, aloud and abruptly.                      |
|              1831 | I remembered our visit to the Merceria.                |
|              1835 | I remembered the circumstance very well.               |
|              1836 | I remembered the man’s face now, but not his name.     |
|              1838 | I remembered what I had seen, and bided my time.       |
|              1839 | I remembered it all perfectly.                         |
|              1841 | I remembered it quite well.                            |
|              1842 | I remembered its former decor.                         |
|              1844 | I remembered “obedience,” and did as I was told.       |
|              1846 | I remembered the whole collection.                     |
|              1848 | I remembered the voyage home far too well for my age.  |
|              1850 | I remembered I had talked easily with her.             |
|              1852 | I remembered one of them well.                         |
|              1853 | I remembered it, and that’s why we are here now.       |
|              1854 | I remembered every single thing that had happened.     |
|              1856 | I remembered my dream and took comfort.                |
|              1857 | I remembered what I should never have forgotten.       |
|              1859 | I remembered my promise to you, and here I am.         |
|              1861 | I remembered these things afterwards.                  |
|              1862 | I remembered the divine halo of his smile.             |
|              1863 | I remembered.                                          |
|              1865 | I remembered that you are joining your regiment.       |
|              1866 | I remembered only those other and greater things.      |
|              1867 | I remembered something.                                |
|              1868 | I remembered how those girls out West bragged.)        |
|              1869 | I remembered a short story I had read once.            |
|              1870 | I remembered him perfectly.                            |
|              1871 | I remembered the lad that was Jimmie Tool.             |
|              1872 | I remembered, also how I had threatened her.           |
|              1874 | I remembered he lived in Jermyn Street.                |
|              1876 | I remembered Lafayette.                                |
|              1877 | I remembered your invitation, Mrs. Aikens.             |
|              1879 | I remembered the public fields of my own age.          |
|              1881 | I remembered well the time—I had a sore foot.          |
|              1884 | I remembered something M. Briavoine had said.          |
|              1885 | I remembered you from Monte Carlo, you see.            |
|              1887 | I remembered the gentleness of the man with her.       |
|              1889 | I remembered the engineer’s statement, but was silent. |
|              1890 | I remembered how you had lost your father.             |
|              1891 | I remembered the wind.                                 |
|              1894 | I remembered your picture.                             |
|              1911 | I remembered my name, but that’s about all….           |
|              1913 | I remembered well your mention of this shop!           |
|              1924 | I remembered now.                                      |
|              1933 | I remembered the first Lindbergh.                      |
