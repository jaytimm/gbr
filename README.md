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
|              -750 | I remember well when thou didst go to Troy.            |
|              1564 | I remember his name.                                   |
|              1631 | I remember, poor Nat.                                  |
|              1740 | I remember particularly the Ode *Eheu fugaces*\[551\]. |
|              1743 | I remembered seeing some empty casks in the hold.      |
|              1764 | I remember them in their splendour!                    |
|              1775 | I remember the astonishment it raised in me.           |
|              1785 | I remember some Life Guardsmen helped me on.           |
|              1787 | I remember seeing him about when he was a little boy.  |
|              1788 | I remember it well—I remember it well!                 |
|              1789 | I remember her right well!                             |
|              1793 | I remember me mein song now.                           |
|              1797 | I remember your logic of old.’                         |
|              1799 | I remembered the dressmaker’s smile!                   |
|              1802 | I remember,” he said, aloud.                           |
|              1803 | I remember too well my first dose of castor-oil.       |
|              1804 | I remember the name well.                              |
|              1805 | I remember the first of them all.                      |
|              1806 | I remember well your kindness to it.                   |
|              1807 | I remember two he told of Dean Mansel.                 |
|              1809 | I remember now.                                        |
|              1810 | I remember my father in powder.                        |
|              1811 | I remember she gave me (poor dear!)                    |
|              1812 | I remember every word–“Charge!                         |
|              1814 | I remember no more.                                    |
|              1815 | I remembered the red blanket on my saddle.             |
|              1817 | I remember gooin’ to a oratory once, at Bury.          |
|              1818 | I remember that I wept all night.                      |
|              1819 | I remember very well–it struck even my childish eyes.  |
|              1820 | I remember also when we had a music-hall in the City.  |
|              1821 | I remember seeing somebody poking the fire last night. |
|              1822 | I remember how she said it directly she saw him.       |
|              1823 | I remember that his regiment was there.                |
|              1824 | I remember among them a Miss Oliphaunt.                |
|              1825 | I remember her–she is an awful old woman.              |
|              1826 | I remember his being at Maroobil.’                     |
|              1828 | I remember being once thought very like your sister.   |
|              1829 | I remember how eloquently you did it.                  |
|              1830 | I remembered, aloud and abruptly.                      |
|              1831 | I remember all,” she said, eagerly.                    |
|              1832 | I remember his face very well.                         |
|              1833 | I remember Mrs. Middlemore—-” “Who’s she?              |
|              1834 | I remember him well.                                   |
|              1835 | I remember I answered him rather rudely, “Ah!          |
|              1836 | I remember that they called themselves the “Oüaits.    |
|              1837 | I remember my grandfather as a stately old gentleman.  |
|              1838 | I remember his mother who died near fifty years ago.   |
|              1839 | I remember my cousins—-” “Great musical talent!        |
|              1840 | I remember that I felt very swollen.                   |
|              1841 | I remember how Rodney felt about you.                  |
|              1843 | I remember now as clearly as if it were this minute.   |
|              1844 | I remembered my fable and flinched.                    |
|              1845 | I remember I always felt responsible for you.          |
|              1846 | I remember, at least, all about that.                  |
|              1847 | I remembered the boy Madame Tank had told about.       |
|              1848 | I remember your picture perfectly.’                    |
|              1849 | I remembered a certain Sunday in the Park.             |
|              1850 | I remember there were ten salt-cellars, no two alike.  |
|              1851 | I remembered him very well.                            |
|              1852 | I remember it was real pretty when we first got it.    |
|              1853 | I remember a grand action I won there.                 |
|              1854 | I remember her distinctly.                             |
|              1855 | I remember a very amusing incident.                    |
|              1856 | I remember!                                            |
|              1857 | I remember it took my fancy immensely.                 |
|              1858 | I remember I heard o’ his goin’ there.                 |
|              1859 | I remember quite well.                                 |
|              1860 | I remember one extraordinary instance. . . .           |
|              1861 | I remember a sister with fluffy hair.                  |
|              1862 | I remember the tune and the words.                     |
|              1863 | I remember it well.                                    |
|              1864 | I remember . . .                                       |
|              1865 | I remember one violinist whose efforts were woeful.    |
|              1866 | I remember that some called him Robin of Locksley.     |
|              1867 | I remember he said he was going to post-date it.       |
|              1868 | I remembered those “few miles of desert.               |
|              1869 | I remember one old road agent named Laughing Sam.      |
|              1870 | I remember it perfectly.                               |
|              1871 | I remember when you was born.                          |
|              1872 | I remember perfectly doing it.                         |
|              1873 | I remember it still.                                   |
|              1874 | I remember the day well.                               |
|              1875 | I remember Travo.’                                     |
|              1876 | I remember yet the first time I saw the coast.         |
|              1877 | I remember what a fine tea my mother had for me.       |
|              1878 | I remember–I read about it in the ‘Nation.’            |
|              1879 | I remember well the day I was leaving.                 |
|              1880 | I remember that Bobby Governeur was enslaved!          |
|              1881 | I remember Augustus mentioning it in his speech.       |
|              1882 | I remember only the refrain.                           |
|              1883 | I remember, sir.                                       |
|              1884 | I remember you, sir.                                   |
|              1885 | I remember you wrote me an amazing letter.             |
|              1886 | I remembered something I had forgotten to tell him.    |
|              1887 | I remember seeing one man writing page after page.     |
|              1888 | I remember one night.                                  |
|              1889 | I remember well.                                       |
|              1890 | I remember now,” said Lady Isabel thoughtfully.        |
|              1891 | I remember being rebuked for my early scoffing.        |
|              1893 | I remember one day I met my first and only Socialist.  |
|              1895 | I remember once, first time I landed in Bordeaux …     |
|              1896 | I remember it when it was a little cup.                |
|              1897 | I remember you,” said Herr Banker.                     |
|              1898 | I remember, of course.                                 |
|              1899 | I remember you quite well.                             |
|              1900 | I remember seeing him on the boat.                     |
|              1904 | I remember distinctly that you were free.              |
|              1906 | I remember–” “Never mind what you remember!            |
|              1908 | I remembered MacGregor’s comment at the time.          |
|              1909 | I remember you as a child, willful and headstrong.     |
|              1911 | I remembered how he resembled the lanee-assistant.     |
|              1915 | I remember so many crazy things–the vacation to Mars.  |
|              1920 | I remember hearing about it when I was a little boy.   |
|              1925 | I remembered my instructions.                          |
|              1926 | I remember–no, never mind.                             |
|              1927 | I remember that nice young man … what was his name?    |
|              1928 | I remember him very clearly.                           |
