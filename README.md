# User friendly extensions to Project Gutenberg

> I remembered the dressmaker’s smile!!

## Intro

Working with raw text files from Project Gutenberg. Some simple,
parallelized functions for text extraction, sentence tokenization, and
simple search.

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

clust <- parallel::makeCluster(10)
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

### Search in parallel

``` r
sentences <- list.files(path = path_to_out_folder,
                        full.names = T)


pg_search_bookwise <- function(x, 
                               pattern, 
                               book_sample = NULL,
                               cores = 6){
  
  search_bookwise <- function(x, pattern){ 
    lapply(x, function(q){
      y <- readRDS(q)
      subset(y, grepl(pattern, text)) }) |> 
      data.table::rbindlist() }
  
  if(!is.null(book_sample)){x <- sample(x, book_sample)}
  batches <- split(x, ceiling(seq_along(x)/50))
  
  clust <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = clust,
                          varlist = c('search_bookwise',
                                      'batches'),
                          envir = environment())
  
  out <- pbapply::pblapply(X = batches, 
                    FUN = function(x) search_bookwise(x, 
                                                      pattern = pattern), 
                    cl = clust)
  
  parallel::stopCluster(clust)
  out1 <- out |> data.table::rbindlist()
  out1[, doc_id := gsub('\\..*$', '', doc_id)]
  return(out1)
}
```

### Output

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
remember99 <- sentences |> pg_search_bookwise(pattern = '^I remember',
                                              book_sample = 10000,
                                              cores = 10)

remember99 |>
  left_join(pg_meta, by = c('doc_id' = 'id')) |>
  na.omit() |>
  mutate(text = gsub('"$|”$', '', text)) |>
  distinct(text, .keep_all = T) |>
  mutate(nc = nchar(text)) |>
  filter(nc < 55) |>
  group_by(authoryearofbirth) |>
  slice(1) |> ungroup() |>
  select(authoryearofbirth, text) |>
  arrange(authoryearofbirth) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1564 | I remember his name.                                   |
|              1626 | I remember he came severall times to Trin.\[           |
|              1672 | I remember some Years ago an Instance of this Kind.    |
|              1740 | I remember particularly the Ode *Eheu fugaces*\[551\]. |
|              1759 | I remember it well.                                    |
|              1763 | I remember, and well I may!                            |
|              1767 | I remember you here, a boy; you was always good.       |
|              1778 | I remember this was the case in Ireland, in 1823.      |
|              1788 | I remember a singular instance of this,’ said he.      |
|              1793 | I remember perfectly now.                              |
|              1794 | I remember I was always hungry then–always.            |
|              1797 | I remember dates–I am sure that it was so.             |
|              1799 | I remembered the dressmaker’s smile!                   |
|              1800 | I remember I told him, ‘At home.’                      |
|              1802 | I remember his country house, like a glory in a cloud. |
|              1804 | I remember it well; it would be strange if I did not.  |
|              1805 | I remember it well!                                    |
|              1806 | I remember still less.                                 |
|              1808 | I remember the question.                               |
|              1810 | I remember eating sloes and crabs with a relish.       |
|              1811 | I remember,” says Harry, looking at his bandaged arm.  |
|              1812 | I remember eating muffins at the time, with marmalade. |
|              1814 | I remember!                                            |
|              1815 | I remember there was a Walter when I was at Dunripple. |
|              1817 | I remembered.                                          |
|              1818 | I remember well our conversation on the subject.       |
|              1819 | I remember now the cool feeling of her marble hall.    |
|              1821 | I remember everything up to the last moment.           |
|              1822 | I remember it well, Sueno.                             |
|              1823 | I remember it now.                                     |
|              1824 | I remember that Jenny was very kind to me, too.        |
|              1825 | I remembered what Persida had said to me.              |
|              1826 | I remember a verse or two.                             |
|              1827 | I remember the bitterness with which I heard it.       |
|              1828 | I remember nothing.                                    |
|              1829 | I remember thinking what a skeleton it was.            |
|              1830 | I remember one of you was very pretty.                 |
|              1831 | I remember my uncle’s death.                           |
|              1832 | I remember him now.                                    |
|              1833 | I remember a storm off the River Plate.                |
|              1834 | I remember one garden-party at Buckingham Palace.      |
|              1835 | I remember these women perfectly well.                 |
|              1836 | I remember speaking to her about it some time since.   |
|              1837 | I remember a case where it might be advantageous.      |
|              1838 | I remember–you were here last year.                    |
|              1839 | I remember looking at you this afternoon.              |
|              1840 | I remember distinctly only two of his stories.         |
|              1841 | I remember how my side ached that day.                 |
|              1842 | I remember once making the same reflection at Manilla. |
|              1843 | I remember now.                                        |
|              1844 | I remember him in England.                             |
|              1845 | I remember with pleasure a visit to Fanny Kemble—Mrs.  |
|              1846 | I remember it all.                                     |
|              1847 | I remember your father very well, and yourself, too.   |
|              1848 | I remember it as distinctly as if it were yesterday.   |
|              1849 | I remember it.                                         |
|              1850 | I remembered that I belonged to France.                |
|              1851 | I remember I called her Sòruchna.                      |
|              1852 | I remember now you said so.                            |
|              1853 | I remember one case.                                   |
|              1854 | I remember the young man very well.                    |
|              1855 | I remember you,” he said kindly.                       |
|              1856 | I remember those very words of yours.                  |
|              1857 | I remember how it ended also.                          |
|              1858 | I remember that name.                                  |
|              1859 | I remember Mr. Guthrie’s fearlessness on the scaffold. |
|              1860 | I remember you said so.                                |
|              1861 | I remember I went to the State ball, and cheerfully.   |
|              1862 | I remember you and your father clearly.                |
|              1863 | I remember best that I was pretty sore still.          |
|              1864 | I remember you lost, and I won.                        |
|              1865 | I remember now—-” “*You* did the bullying then.        |
|              1866 | I remember it perfectly….                              |
|              1867 | I remember well.                                       |
|              1868 | I remember little broken snatches of explanation.      |
|              1869 | I remembered the piece of good news I had for Raoul.   |
|              1870 | I remember THAT all right….                            |
|              1871 | I remember when you was born.                          |
|              1872 | I remembered you all.                                  |
|              1873 | I remember now, I lost the leash thong last night.     |
|              1874 | I remember how anxious she looked at him.              |
|              1875 | I remember seeing one of ’em the day we got back.      |
|              1876 | I remember how Hon.                                    |
|              1877 | I remember the first time I ever rode on it.           |
|              1878 | I remember, now.                                       |
|              1879 | I remember now that I ordered Jap to do it.            |
|              1880 | I remember the train goes at nine-fifty-five.          |
|              1881 | I remembered him as having no aptitude for the sea.    |
|              1882 | I remember the night he died.                          |
|              1884 | I remember a conversation I heard at Cleveland.        |
|              1885 | I remember I’d got a little, silky moustache.          |
|              1886 | I remembered something I had forgotten to tell him.    |
|              1887 | I remembered that my boat was to sail on Friday.       |
|              1888 | I remember that….                                      |
|              1889 | I remember he asked Gilbert, “Do you like babies?      |
|              1890 | I remember now,” said Lady Isabel thoughtfully.        |
|              1892 | I remember this.                                       |
|              1893 | I remember one day I met my first and only Socialist.  |
|              1894 | I remember him once in Chicago.                        |
|              1896 | I remember the way from my dream.                      |
|              1898 | I remember it; the silly fellow!                       |
|              1902 | I remember he brushed my shoulder.                     |
|              1904 | I remember distinctly that you were free.              |
|              1909 | I remember his face–so full of smiles.                 |
|              1910 | I remember you used to be shy.                         |
|              1923 | I remember that now, Harry.                            |
|              1925 | I remember there was some trouble….                    |
|              1928 | I remember him very clearly.                           |
|              1931 | I remembered him.                                      |
|              1933 | I remember once, I went to a circus, or a sideshow.    |
