#' Build corpus/tif.
#'
#' @name gb_build_corpus
#' @param x A character vvector
#' @param as_fulltext boolean
#' @return A data frame
#'
#' @export
#' @rdname gb_build_corpus
#'
gb_build_corpus <- function(x, as_fulltext = F){

  j <- lapply(x, function(q){

    tryCatch(readRDS(q), error=function(e) NULL)
  })

  j0 <- Filter(length, j)
  j1 <- j0 |> data.table::rbindlist()

  if(as_fulltext){
    j1[, doc_id := gsub('\\..*$', '', doc_id)]
    j1 <- j1[, list(text = paste(text, collapse = " ")), by = doc_id]
  }

  return(j1)
}
