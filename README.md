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
|              1805 | I remembered a few of these books.                     |
|              1810 | I remembered how, a little child.                      |
|              1811 | I remembered the dim melodies of the Lady of the Lake. |
|              1817 | I remembered.                                          |
|              1822 | I remembered only the charm and the opportunity.       |
|              1824 | I remembered too well what I had seen to hope for it.  |
|              1827 | I remembered that I was thy accepted knight.           |
|              1830 | I remembered, aloud and abruptly.                      |
|              1835 | I remembered that Oshoria had my steel and flint.      |
|              1836 | I remembered what Mother Gaillarde had said.           |
|              1837 | I remembered our promise to be friends.                |
|              1839 | I remembered what Mrs. Parsley had said.               |
|              1841 | I remembered it quite well.                            |
|              1842 | I remembered no barrier.                               |
|              1846 | I remembered the spot well.                            |
|              1847 | I remembered the boy Madame Tank had told about.       |
|              1850 | I remembered I had talked easily with her.             |
|              1852 | I remembered one of them well.                         |
|              1854 | I remembered that I had rather pained him last night.  |
|              1856 | I remembered my dream and took comfort.                |
|              1857 | I remembered something else.                           |
|              1858 | I remembered it not until now.                         |
|              1859 | I remembered Holmes’s injunction.                      |
|              1860 | I remembered that date.                                |
|              1861 | I remembered these things afterwards.                  |
|              1863 | I remembered Madame Stauwaert in the nick of time.     |
|              1864 | I remembered you very well–very well, indeed.          |
|              1866 | I remembered nothing.                                  |
|              1867 | I remembered their smiles afterwards, and understood.  |
|              1868 | I remembered the saying of the watchman.               |
|              1869 | I remembered when I had first heard it.                |
|              1870 | I remembered the look on her face as I rowed away.     |
|              1874 | I remembered the witches’ Sabbaths.                    |
|              1875 | I remembered the tunnel now and the Kansas journalist. |
|              1876 | I remembered how he growled and grumbled.              |
|              1877 | I remembered that he had been at the wheel.            |
|              1878 | I remembered so quick it stopped me.                   |
|              1879 | I remembered the public fields of my own age.          |
|              1880 | I remembered Kennedy’s parting words.                  |
|              1881 | I remembered that they had left my place together.     |
|              1887 | I remembered that my boat was to sail on Friday.       |
|              1888 | I remembered the whole scene on the moorland.          |
|              1894 | I remembered it to-day.                                |
|              1904 | I remembered that dive of six years before.            |
|              1909 | I remembered songs in the evening.                     |
|              1924 | I remembered now.                                      |
|              1930 | I remembered the trail and remembered the spot.        |
