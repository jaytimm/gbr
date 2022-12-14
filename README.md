# User friendly extensions to Project Gutenberg

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
|              -106 | I remember Albinus the ex-consul and Sp.               |
|              1534 | I remember that Matth.                                 |
|              1564 | I remember thine eyes well enough.                     |
|              1626 | I remember he came severall times to Trin.\[           |
|              1670 | I remember it, and it is true.                         |
|              1672 | I remember some Years ago an Instance of this Kind.    |
|              1707 | I remember Allworthy at college.                       |
|              1731 | I remember I neither spoke to you nor looked at you.   |
|              1740 | I remember but little of our conversation.             |
|              1743 | I remember the case, and took my part in it.           |
|              1745 | I remember an instance or two wherein this happened.   |
|              1747 | I remember when I was in love with thee—— \_Gandelin.  |
|              1759 | I remember that I am still in your debt.               |
|              1767 | I remember you here, a boy; you was always good.       |
|              1769 | I remember that name.                                  |
|              1771 | I remember that Varney is a smooth-tongued varlet.     |
|              1773 | I remember an interesting passage of history.          |
|              1775 | I remember nothing in my life like it.                 |
|              1779 | I remember meeting him at Earl Grey’s at dinner.       |
|              1784 | I remember now what I rang for.                        |
|              1787 | I remember him with grey hair and a smile.             |
|              1788 | I remember this farce from a curious circumstance.     |
|              1789 | I remember when the Earl of–” “Pendennyss!             |
|              1792 | I remember this was the case in Ireland, in 1823.      |
|              1793 | I remember the first day with perfect distinctness.    |
|              1794 | I remember praying, wrestling, and striving with God.  |
|              1797 | I remember your logic of old.’                         |
|              1798 | I remember going ashore on Good Friday.                |
|              1800 | I remember him younger, once!                          |
|              1802 | I remember I have not seen him to-day.                 |
|              1803 | I remember my mother well.                             |
|              1804 | I remember them very well.                             |
|              1805 | I remember it well!                                    |
|              1806 | I remember too how I wrote or spoke of such.           |
|              1807 | I remember when, ‘I’ll tell your Mamma!’               |
|              1809 | I remember observing in South America (27.             |
|              1810 | I remember studying it over and over again.            |
|              1811 | I remember the kind fellow asking.                     |
|              1812 | I remember every word he said that evening.            |
|              1814 | I remember everything about that evening so well.      |
|              1815 | I remember it as though it were yesterday.             |
|              1816 | I remember the word.                                   |
|              1817 | I remember our once meeting one of these boats.        |
|              1818 | I remember now; a blow–but the Death?–the Death?       |
|              1819 | I remember your description.                           |
|              1820 | I remember this time with peculiar pleasure.           |
|              1821 | I remember that I suddenly saw in the darkness a star. |
|              1822 | I remember that.                                       |
|              1823 | I remember that as long as I remember anything.        |
|              1824 | I remember no blow.                                    |
|              1825 | I remember well her answer.                            |
|              1826 | I remember the day as well as if it was yesterday.     |
|              1827 | I remember meeting in France an old Italian refugee.   |
|              1828 | I remember no one else.                                |
|              1829 | I remember well this spring of ’73.                    |
|              1830 | I remember coming, I think, when I was four years old. |
|              1831 | I remember everything now.                             |
|              1832 | I remember him now.                                    |
|              1833 | I remember a storm off the River Plate.                |
|              1834 | I remember seeing something about it in the papers.    |
|              1835 | I remember Mr. Manchester very well.                   |
|              1836 | I remember when Jack put it up.                        |
|              1837 | I remember my grandfather as a stately old gentleman.  |
|              1838 | I remember it!                                         |
|              1839 | I remember that day so well.                           |
|              1840 | I remember no more then.                               |
|              1841 | I remember the kind of evening it was.                 |
|              1842 | I remember once making the same reflection at Manilla. |
|              1843 | I remember that he owes me a certain amount.’          |
|              1844 | I remember she wrote the note that enclosed it.        |
|              1845 | I remember John very well.                             |
|              1846 | I remember thinking, ‘Well, now for some rest nights!’ |
|              1847 | I remember one day at the Cirque d’Été seeing Mdlle.   |
|              1848 | I remember that one.                                   |
|              1849 | I remember just enough to know that I was mad.         |
|              1850 | I remember the night perfectly.                        |
|              1851 | I remember how uncomfortable I felt afterwards.        |
|              1852 | I remember that sand-hill with its hair all a-bristle. |
|              1853 | I remember me now it did seem to threaten rain.        |
|              1854 | I remember that particularly well.’                    |
|              1855 | I remember when Mr. Nash died.                         |
|              1856 | I remember something of it now.                        |
|              1857 | I remember her catching her breath rather pitifully.   |
|              1858 | I remember one thing he said.                          |
|              1859 | I remember the station, but not a man called Menteith. |
|              1860 | I remember her smiling always.                         |
|              1861 | I remember the old King——’ She broke off suddenly.     |
|              1862 | I remember a curious optical delusion overtook me.     |
|              1863 | I remember showing it to your father.                  |
|              1864 | I remember it all as though it were only yesterday.    |
|              1865 | I remember Agemond well.                               |
|              1866 | I remember now.                                        |
|              1867 | I remember that so well.                               |
|              1868 | I remember your saying so.                             |
|              1869 | I remember just how Mrs. Davis does.’                  |
|              1870 | I remember now–a hundred things.                       |
|              1871 | I remember wondering why he said RITCHIE.              |
|              1872 | I remember I did much the same to your mother.         |
|              1873 | I remember my first literary walk down the Avenue.     |
|              1874 | I remember her as a fine, blooming young woman.        |
|              1875 | I remember well an incident in my own youth.           |
|              1876 | I remember that I looked once and then ran away.       |
|              1877 | I remember it all now–nearly all.                      |
|              1878 | I remember I was glad that I had not another.          |
|              1879 | I remember I cried a little.                           |
|              1880 | I remember then she did not chide.                     |
|              1881 | I remember how they was a-shinin’.                     |
|              1882 | I remember neither father nor mother.                  |
|              1883 | I remember it every day and every night….              |
|              1884 | I remember what they used to say about the Yankees.    |
|              1885 | I remember something about it.                         |
|              1886 | I remember her perfectly….                             |
|              1887 | I remember one tree that we had so pinned.             |
|              1888 | I remember that….                                      |
|              1889 | I remember once seeing a little lost child.            |
|              1890 | I remember them perfectly.                             |
|              1893 | I remember you!                                        |
|              1894 | I remember almost nothing of the incident.             |
|              1896 | I remember the way from my dream.                      |
|              1897 | I remember you,” said Herr Banker.                     |
|              1898 | I remember it; the silly fellow!                       |
|              1902 | I remember her a little, just a little.                |
|              1903 | I remember reading about it.                           |
|              1904 | I remember one instance in particular.                 |
|              1905 | I remember once….                                      |
|              1906 | I remember our garden just outside our yard.           |
|              1909 | I remember his face, the candid eyes and lips.         |
|              1911 | I remember what it is like to be super, Doc.           |
|              1913 | I remember an evening in April 1939.                   |
|              1914 | I remember the description the way you do.             |
|              1915 | I remember so many crazy things–the vacation to Mars.  |
|              1918 | I remember Hungary–its ancient horror.                 |
|              1919 | I remember then.                                       |
|              1920 | I remember the first time I went to the Continent.     |
|              1921 | I remember when I first became aware of the movement.  |
|              1925 | I remember a thing or two.                             |
|              1927 | I remember he mentioned that.                          |
|              1928 | I remember everything.                                 |
|              1929 | I remember once I had a nightmare about my old man.    |
|              1930 | I remember being bound to you.                         |
|              1961 | I remember that day very well.                         |
