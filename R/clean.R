
#' Clean and filter results
#'
#' @name gbclean
#' @param x df output from search
#' @param n An integer
#'
#' @export
#' @rdname gbclean
#'
gbclean <- function(x, n = 2){

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
