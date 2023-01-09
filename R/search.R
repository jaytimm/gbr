
#' Search gutenberg
#'
#' @name gb_search
#' @param x A character vector
#' @param pattern a character string
#' @param cores an integer
#' @return A data frame
#'
#' @export
#' @rdname gb_search
#'


## x <- batches[[1]][1]
gb_search <- function(x,
                      pattern,
                      ignore.case = T,
                      cores = 5){

  search_bookwise <- function(x, pattern){

    j <- lapply(x, function(q){

      tryCatch(readRDS(q) |>
                 dplyr::filter(grepl(pattern,
                                     text,
                                     ignore.case = ignore.case))
               , error=function(e) NULL)
      })

    j0 <- Filter(length, j)
    j0 |> data.table::rbindlist()
    }

  batches <- split(x, ceiling(seq_along(x)/25))
  p0 <- pattern

  clust <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = clust,
                          varlist = c('search_bookwise',
                                      'p0',
                                      'batches'),
                          envir = environment())

  out <- pbapply::pblapply(X = batches,
                           FUN = function(x) search_bookwise(x, pattern = p0),
                           cl = clust)

  parallel::stopCluster(clust)

  out1 <- out |> data.table::rbindlist()

  out1[, doc_id := gsub('\\..*$', '', doc_id)]

  out1 <- merge(out1,
                gbr::pg_meta,
                by.x = 'doc_id',
                by.y = 'id')
  return(out1)
}
