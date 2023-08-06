

library(dplyr)
pgs <- gbr::pg_meta |>
  
  filter(language == "['en']",
         authoryearofbirth > 1700,
         type == '') |>
  
  mutate(in_gbr = ifelse(author %in% gutenbergr::gutenberg_authors$author, 1, 0))


#######
batches <- split(pgs$title, ceiling(seq_along(pgs$title)/100))

get_that <- function(x){
  
  q99 <- lapply(x, function(y){
    
    uu <- urltools::url_encode(y)
    hathi <- paste0('https://catalog.hathitrust.org/Search/Home?lookfor=', uu, '&searchtype=title&ft=ft&setft=true')
    
    mm <- tryCatch(
      rvest::read_html(httr::GET(hathi)) |> #, httr::timeout(60)
        rvest::html_node(xpath = '//*[@class="container-fluid p-1"]'),
      error = function(e) NA)  
    
    if(is.na(mm)){
      in_title <- y; pubdate <- NA; author <- NA; out_title <- NA
      
    }else{
      
      out_title <- mm |>
        rvest::html_node(xpath = '//*[@class="record-title"]') |>
        rvest::html_text() |> trimws()
      
      pb <- mm |>
        rvest::html_nodes(xpath = '//*[@class="g-col-lg-4 g-col-12"]') |>
        rvest::html_text()
      pb <-subset(pb, !pb %in% c('Author', 'Published'))
      
      nn <- 1:length(pb)
      
      author <- pb[nn[!nn%%2]][1:5]
      dates <- pb |> as.numeric() 
      pubdate <- dates[!is.na(dates)][1:5]
    }
    
    data.frame(in_title = y,
               pubdate = pubdate,
               author = author,
               out_title = out_title)
  }) 
  
  names(q99) <- x
  q99 <- Filter(length, q99)
  q99 |> data.table::rbindlist()
}


clust <- parallel::makeCluster(14)
parallel::clusterExport(cl = clust,
                        varlist = c('batches'),
                        envir = environment())

pubs <- pbapply::pblapply(X = batches,
                          FUN = get_that,
                          cl = clust)

parallel::stopCluster(clust)
pubs <- data.table::rbindlist(pubs)

setwd('/home/jtimm/pCloudDrive/free-corpus')
saveRDS(pubs, 'pubs_etc.rds')