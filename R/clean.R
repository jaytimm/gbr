
#' Clean and filter results
#'
#' @name gb_examples
#' @param x df output from search
#' @param n An integer
#'
#' @export
#' @rdname gbclean
#'
gb_examples <- function(x, n = 2){

  x |>
    na.omit() |>
    dplyr::mutate(text = gsub('"$|”$', '', text)) |>
    dplyr::distinct(text, .keep_all = T) |>
    dplyr::filter(nchar(text) < 55) |>
    dplyr::group_by(authoryearofbirth) |>
    dplyr::slice(1:n) |>
    dplyr::ungroup() |>
    dplyr::select(authoryearofbirth, text) |>
    dplyr::arrange(authoryearofbirth)

}
