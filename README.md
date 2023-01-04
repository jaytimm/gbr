# User friendly extensions to Project Gutenberg

> I remembered the glowworms and millipeds. I remembered the spectre. I
> remembered the dressmaker’s smile!!

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
|              1802 | I remembered him well.                                 |
|              1804 | I remembered what had occurred the previous evening.   |
|              1805 | I remembered a few of these books.                     |
|              1808 | I remembered its striking features so well.            |
|              1814 | I remembered the voice now.                            |
|              1815 | I remembered the red blanket on my saddle.             |
|              1818 | I remembered the lazo.                                 |
|              1819 | I remembered that Bartleby never left the office.      |
|              1822 | I remembered old Balerma, and felt deeply for them.    |
|              1824 | I remembered at last.                                  |
|              1827 | I remembered that I was thy accepted knight.           |
|              1830 | I remembered, aloud and abruptly.                      |
|              1831 | I remembered what father had said about Polly.         |
|              1835 | I remembered this as I looked at Mrs. Darrell.         |
|              1836 | I remembered what Mother Gaillarde had said.           |
|              1839 | I remembered more after that.                          |
|              1843 | I remembered what she had mentioned to me.             |
|              1844 | I remembered “obedience,” and did as I was told.       |
|              1846 | I remembered his chaff in that hour.                   |
|              1849 | I remembered her.                                      |
|              1853 | I remembered it, and that’s why we are here now.       |
|              1854 | I remembered well.                                     |
|              1857 | I remembered two.                                      |
|              1861 | I remembered the face at once.                         |
|              1863 | I remembered her face.                                 |
|              1864 | I remembered the strange Omen of the Camel’s Hoof!     |
|              1865 | I remembered some of Charlie’s confidences.            |
|              1866 | I remembered something.                                |
|              1867 | I remembered his outburst against Rosetta Rosa.        |
|              1868 | I remembered his drawn face at my question.            |
|              1869 | I remembered of reading about his experience.          |
|              1871 | I remembered,” she said very low, “that night.         |
|              1872 | I remembered.                                          |
|              1873 | I remembered but one conversation during that wait.    |
|              1874 | I remembered Joseph.                                   |
|              1875 | I remembered her shy timidity, her innocence.          |
|              1876 | I remembered the incident well.                        |
|              1877 | I remembered your invitation, Mrs. Aikens.             |
|              1878 | I remembered so quick it stopped me.                   |
|              1879 | I remembered what you’d said, too, Dan.                |
|              1880 | I remembered my first day in the city.                 |
|              1881 | I remembered this with forebodings.                    |
|              1883 | I remembered Alex’s instructions: ’Watch out for gas.  |
|              1894 | I remembered your picture.                             |
|              1903 | I remembered the strange voice that had instructed me. |
|              1904 | I remembered Aarl!                                     |
|              1916 | I remembered the money.                                |
|              1923 | I remembered the library book and almost went back.    |
