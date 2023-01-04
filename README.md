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
|              1799 | I remembered the dressmaker’s smile!                   |
|              1805 | I remembered it first.                                 |
|              1806 | I remembered her counsels and her warnings.            |
|              1807 | I remembered it, it was sich an outlandish name like.  |
|              1814 | I remembered this, and prayed for strength.            |
|              1818 | I remembered the lazo.                                 |
|              1819 | I remembered it all.                                   |
|              1821 | I remembered my obligations to him.                    |
|              1822 | I remembered only the charm and the opportunity.       |
|              1823 | I remembered your inexhaustible kindness.              |
|              1835 | I remembered Lord Nithisdale’s escape in ’15.          |
|              1836 | I remembered her–and her name.                         |
|              1839 | I remembered what Mrs. Parsley had said.               |
|              1841 | I remembered so many things–oh, so many!               |
|              1842 | I remembered no barrier.                               |
|              1843 | I remembered the name Waikatakata.                     |
|              1847 | I remembered the boy Madame Tank had told about.       |
|              1851 | I remembered him very well.                            |
|              1855 | I remembered that this was Paris.                      |
|              1856 | I remembered the suggestion of the Crooked One.        |
|              1857 | I remembered to have seen the figures somewhere.       |
|              1859 | I remembered your motor-car was waiting at the gates.  |
|              1860 | I remembered what I came for and got down my camera.   |
|              1861 | I remembered that gala day when we christened it.      |
|              1862 | I remembered later that I had not done so.             |
|              1863 | I remembered.                                          |
|              1864 | I remembered them now, and discerned his meaning.      |
|              1865 | I remembered now.                                      |
|              1866 | I remembered that.                                     |
|              1867 | I remembered you kindly, with real gratitude, indeed.  |
|              1868 | I remembered how those girls out West bragged.)        |
|              1870 | I remembered him perfectly.                            |
|              1871 | I remembered the gray stranger’s warning.              |
|              1872 | I remembered the name, Jaime de Oteros.                |
|              1873 | I remembered but one conversation during that wait.    |
|              1874 | I remembered it was your wedding-day.                  |
|              1875 | I remembered, and said so.                             |
|              1876 | I remembered afterward that luck was the word he used. |
|              1878 | I remembered so quick it stopped me.                   |
|              1881 | I remembered Augustus dimly.                           |
|              1882 | I remembered my scarf.                                 |
|              1884 | I remembered. . . .                                    |
|              1885 | I remembered the two larks.                            |
|              1887 | I remembered her.                                      |
|              1888 | I remembered him asking.                               |
|              1890 | I remembered Mrs. Inglethorp’s dying words.            |
|              1892 | I remembered without remembering that I remembered.    |
|              1894 | I remembered it to-day.                                |
|              1918 | I remembered that once I had liked cigarettes.         |
|              1933 | I remembered the first Lindbergh.                      |
