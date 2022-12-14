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
sentences <- list.files(path = path_to_out_folder,
                        full.names = T)
```

``` r
library(dplyr)
sentences |> 
  
  pg_search_bookwise(pattern = '^I remember ',
                     book_sample = 20000,
                     cores = 10) |>
  
  left_join(pg_meta, by = c('doc_id' = 'id')) |>
  na.omit() |>
  mutate(text = gsub('"$|”$', '', text)) |>
  distinct(text, .keep_all = T) |>
  filter(nchar(text) < 55) |>
  group_by(authoryearofbirth) |>
  slice(1) |> ungroup() |>
  select(authoryearofbirth, text) |>
  arrange(authoryearofbirth) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              -750 | I remember well when thou didst go to Troy.            |
|              -106 | I remember Albinus the ex-consul and Sp.               |
|              1564 | I remember his name.                                   |
|              1670 | I remember it, and it is true.                         |
|              1672 | I remember some Years ago an Instance of this Kind.    |
|              1720 | I remember a ridiculous instance of this sort.         |
|              1721 | I remember that!                                       |
|              1728 | I remember them both last Voyage.                      |
|              1746 | I remember the veneration in which he was held.        |
|              1759 | I remember but too well!                               |
|              1763 | I remember well that it was owing to Mr. Fox.          |
|              1767 | I remember it as well as if it was this instant.       |
|              1769 | I remember that name.                                  |
|              1771 | I remember feeling just such a pain but once before.   |
|              1772 | I remember well The fatal day.                         |
|              1773 | I remember an interesting passage of history.          |
|              1775 | I remember Miss Wetheral’s taste for reading.          |
|              1779 | I remember meeting him at Earl Grey’s at dinner.       |
|              1780 | I remember the *first* crier of “young lambs to sell!  |
|              1784 | I remember an instance of this.                        |
|              1785 | I remember some Life Guardsmen helped me on.           |
|              1788 | I remember an instance of his skill in the small way.  |
|              1790 | I remember a droll proof of this.                      |
|              1792 | I remember this was the case in Ireland, in 1823.      |
|              1793 | I remember the first day with perfect distinctness.    |
|              1794 | I remember praying, wrestling, and striving with God.  |
|              1797 | I remember a story of a labourer and his dying wife.   |
|              1798 | I remember going ashore on Good Friday.                |
|              1800 | I remember nothing that passed during the day.         |
|              1802 | I remember as though a superior will made me do so.    |
|              1803 | I remember too well my first dose of castor-oil.       |
|              1804 | I remember nothing else in this room.                  |
|              1806 | I remember too how I wrote or spoke of such.           |
|              1807 | I remember two he told of Dean Mansel.                 |
|              1809 | I remember it well, being then seven years old.        |
|              1810 | I remember his reading there with Mr. Ness.            |
|              1811 | I remember what it was before the houses were built.   |
|              1812 | I remember that, for I woke in a tremble.              |
|              1814 | I remember Walter Heathfield’s features well.          |
|              1815 | I remember to have read in Fr.                         |
|              1816 | I remember scores of such hours of real agony.         |
|              1817 | I remember the night well.                             |
|              1818 | I remember only the first two lines….                  |
|              1819 | I remember I spoke very feebly.                        |
|              1820 | I remember a case in point.                            |
|              1821 | I remember there was real feeling in me, too.          |
|              1822 | I remember it all well as ef it want but yisterday.    |
|              1823 | I remember crying out with tears.                      |
|              1824 | I remember all that is necessary.                      |
|              1825 | I remember him, but I care not for him.                |
|              1826 | I remember once――‘ ‘Come, papa!’                       |
|              1827 | I remember a few lines.                                |
|              1828 | I remember Charlie so well; may I speak of him to her? |
|              1829 | I remember saying, “Oh, don’t do that.                 |
|              1830 | I remember perfectly where Fabio keeps his provisions. |
|              1831 | I remember my uncle’s death.                           |
|              1832 | I remember clearly the arrival of the news.            |
|              1833 | I remember *him* before the young uns was born.        |
|              1834 | I remember when Kennedy was put out of possession.     |
|              1835 | I remember now.                                        |
|              1836 | I remember going out in ’53 with Hank Boompirater.     |
|              1837 | I remember that evening as if it were only yesterday.  |
|              1838 | I remember that I have a conscience.                   |
|              1839 | I remember my cousins—-” “Great musical talent!        |
|              1840 | I remember feeling very proud of my good landfall.     |
|              1841 | I remember distinctly one thing papa said.             |
|              1842 | I remember Maryan told me something of this.           |
|              1843 | I remember some of those winter mornings.              |
|              1844 | I remember it at night in my bed, and I am afraid.     |
|              1845 | I remember with pleasure a visit to Fanny Kemble—Mrs.  |
|              1846 | I remember Hardin coming himself.                      |
|              1847 | I remember hearing him testify as a witness to a will. |
|              1848 | I remember the very day he had his tail broken off.    |
|              1849 | I remember a little Hindustani.                        |
|              1850 | I remember every apple tree being planted.             |
|              1851 | I remember your telling me so.                         |
|              1852 | I remember it was real pretty when we first got it.    |
|              1853 | I remember it just as if it was yesterday.             |
|              1854 | I remember it.                                         |
|              1855 | I remember the next few days very well.                |
|              1856 | I remember our first night at the new camp.            |
|              1857 | I remember how sick I felt wriggling in that slime.    |
|              1858 | I remember one close call I had, though.               |
|              1859 | I remember her well.                                   |
|              1860 | I remember the shape of the head now.                  |
|              1861 | I remember now, I smelled smoke or thought I did.      |
|              1862 | I remember that fog well.                              |
|              1863 | I remember your name too.                              |
|              1864 | I remember someone exclaiming “He’s bled to death!     |
|              1865 | I remember him well.                                   |
|              1866 | I remember that I thought Edmond Laforce insane.       |
|              1867 | I remember another couple up on the hillside.          |
|              1868 | I remember his christening as if it was yesterday.     |
|              1869 | I remember it as plainly as if it happened yesterday.  |
|              1870 | I remember as clear as crystal!                        |
|              1871 | I remember how long he was reading it.                 |
|              1872 | I remember hearing him say: “Oh!                       |
|              1873 | I remember seeing her once.                            |
|              1874 | I remember Jack’s habits of old.                       |
|              1875 | I remember being stricken with it.                     |
|              1876 | I remember their queer get-ups.                        |
|              1877 | I remember the words very well, and they were these.   |
|              1878 | I remember that Essenes are physicians.                |
|              1879 | I remember well the day I was leaving.                 |
|              1880 | I remember two types of merchants at Constantinople.   |
|              1881 | I remember one tremendous march.                       |
|              1882 | I remember thinking: ’This is awkward.                 |
|              1883 | I remember a Captain in the Royal Marines….            |
|              1884 | I remember that.                                       |
|              1885 | I remember Senhor Dexter.                              |
|              1886 | I remember once, my throat….                           |
|              1887 | I remember that brother o’ yours.                      |
|              1888 | I remember that evening so well.                       |
|              1889 | I remember well.                                       |
|              1890 | I remember parts quite vividly, every now and then….   |
|              1891 | I remember that the entraining was poor.               |
|              1893 | I remember nothing!                                    |
|              1894 | I remember his enthusiasm over the weapon.             |
|              1895 | I remember Dad mentioned a time limit.                 |
|              1896 | I remember I ran away from home once.                  |
|              1897 | I remember that rock, \_perfectly!                     |
|              1898 | I remember a bit of my classics.                       |
|              1899 | I remember hiding from Tatiana behind a drapery.       |
|              1903 | I remember some of the things he used to say.          |
|              1904 | I remember the serials.                                |
|              1905 | I remember now, too.                                   |
|              1906 | I remember our garden just outside our yard.           |
|              1909 | I remember reading about that.                         |
|              1910 | I remember you used to be shy.                         |
|              1913 | I remember an evening in April 1939.                   |
|              1915 | I remember that sermon, now.                           |
|              1917 | I remember getting six pine trees from a nursery.      |
|              1918 | I remember Hungary–its ancient horror.                 |
|              1919 | I remember then.                                       |
|              1920 | I remember the first time I went to the Continent.     |
|              1925 | I remember there was some trouble….                    |
|              1927 | I remember the trip back to earth.                     |
|              1928 | I remember all of us agreeing upon that.               |
|              1933 | I remember once, I went to a circus, or a sideshow.    |
|              1961 | I remember that day very well.                         |
