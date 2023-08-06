
#' Search gutenberg quickly
#'
#' @name quickly
#' @param x An integer
#' @param pattern a character string
#' @param ignore.case a Boolean
#' @param cores an integer
#' @return A data frame
#'
#' @export
#' @rdname quickly
#'

quickly <- function(pattern,
                    x = 10000,
                    ignore.case = T,
                    cores = 6,
                    min_birth_year = NULL,
                    max_birth_year = NULL,
                    subj = NULL){

  gbr::gb_subset(x = x,
                 min_birth_year = min_birth_year,
                 max_birth_year = max_birth_year,
                 subj = subj) |>

    gbr::gb_search(pattern = pattern,
                   cores = cores)
}
