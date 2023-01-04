
#' Search gutenberg
#'
#' @name gbsearch
#' @param pattern a character string
#' @param x a file path
#' @param book_sample An integer
#' @param cores an integer
#' @return A data frame
#'
#' @export
#' @rdname gbsearch
#'
gbsearch <- function(pattern = '^I believe',
                   x = '/home/jtimm/gutenberg/data/sentences/',
                   book_sample = 25000,
                   ignore.case = T,
                   cores = 12){

  ss <- list.files(path = x, full.names = T)

  search_bookwise <- function(x, pattern){

    j <- lapply(x, function(q){

      tryCatch(y <- readRDS(q) |>
                 dplyr::filter(grepl(pattern,
                               text,
                               ignore.case = ignore.case))
               , error=function(e) NULL)
      })

    j0 <- Filter(length, j)
    j0 |> data.table::rbindlist()
    }


  if(!is.null(book_sample)){ss <- sample(ss, book_sample)}
  batches <- split(ss, ceiling(seq_along(ss)/100))


  clust <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = clust,
                          varlist = c('search_bookwise',
                                      'batches'),
                          envir = environment())

  out <- pbapply::pblapply(X = batches,
                           FUN = function(x) search_bookwise(x, pattern = pattern),
                           cl = clust)

  parallel::stopCluster(clust)
  out1 <- out |> data.table::rbindlist()
  out1[, doc_id := gsub('\\..*$', '', doc_id)]

  out1 <- merge(out1,
                gutensearch::pg_meta,
                by.x = 'doc_id',
                by.y = 'id')
  return(out1)
}






