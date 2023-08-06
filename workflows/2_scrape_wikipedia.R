

ats <- gutenbergr::gutenberg_authors |> #21K < 7,258 < 5,798 with scraped wikis -- 
  filter(!is.na(wikipedia)) |>
  
  ## this doesn't work -- some have multiple wiki links--
  filter(grepl('en.wiki', wikipedia))

## batches <- split(ats$wikipedia, ceiling(seq_along(ats$wikipedia)/100))
batches <- split(ats, ceiling(seq_along(ats$wikipedia)/100))


get_site <- function(x) {
  
  dd <- list()
  
  for(y in 1:nrow(x)){
    
    site <- tryCatch(
      xml2::read_html(httr::GET(x$wikipedia[y], httr::timeout(60))),
      error = function(e) NA
    )
    
    if(is.na(site)){
      w1 <- 'p'; w2 <- 'could not find the link; NA'} else{
        
        ntype1 <- 'p,h1,h2,h3'
        w0 <- rvest::html_nodes(site, ntype1)
        
        if(length(w0) == 0) {
          w1 <- 'p'; w2 <- 'could not find the link; length 0' } else{
            w1 <- rvest::html_name(w0)
            w2 <- rvest::html_text(w0)
          }
        if(any(!validUTF8(w2))){
          w1 <- 'p'; w2 <- 'could not find the link'}
        
        dd[[y]] <- data.frame(author = x$author[y],
                              link = x$wikipedia[y], 
                              type = w1, 
                              text = w2) |>
          
          
          subset(type == 'p' & nchar(text) > 20) |>
          
          ## -- need a bit more --
          dplyr::slice(1:2) 
      }
    
  }
  
  dd |> data.table::rbindlist()
  
}

clust <- parallel::makeCluster(6)
parallel::clusterExport(cl = clust,
                        varlist = c('batches'),
                        envir = environment())

wiks <- pbapply::pblapply(X = batches,
                          FUN = get_site,
                          cl = clust)

parallel::stopCluster(clust)
wiks0 <- data.table::rbindlist(wiks) 

setwd('/home/jtimm/pCloudDrive/free-corpus')
saveRDS(wiks0, 'author-wikis.rds')