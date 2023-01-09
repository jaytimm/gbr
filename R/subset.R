
#' Filter gutenberg corpus by meta
#'
#' @name gb_subset
#' @param x NULL, or integer
#' @param min_birth_year NULL, or integer
#' @param max_birth_year max_birth_year
#' @param subj NULL, or char string
#' @param lang NULL, or char string
#' @param filepath A character string
#'
#' @return A character vector
#'
#' @export
#' @rdname gb_subset
#'
gb_subset <- function(x = NULL,
                      min_birth_year = NULL,
                      max_birth_year = NULL,
                      subj = NULL,
                      lang = 'en',
                      filepath = '/home/jtimm/gutenberg/data/sentences/'){

  ## we need to be filtering table/meta here
  ss <- list.files(path = filepath, full.names = T)
  sdf <- data.frame(ss, id = gsub('(^.*/)(PG[0-9]*)(\\..*$)', '\\2', ss))

  f0 <- gbr::pg_subjects
  if(!is.null(min_birth_year)){f0 <- subset(f0, min_birth_year <= authoryearofbirth)}
  if(!is.null(max_birth_year)){f0 <- subset(f0, max_birth_year >= authoryearofbirth)}
  if(!is.null(subj)){f0 <- subset(f0, grepl(subj, subjects, ignore.case = T))}
  if(!is.null(lang)){f0 <- subset(f0, grepl(lang, language, ignore.case = T))}

  sdf1 <- subset(sdf, id %in% unique(f0$id))

  ## maybe want full meta details --
  if(is.null(x)){samp <- sdf1$ss}
  if(is.numeric(x)){samp <- sample(sdf1$ss, min(x, nrow(sdf1)))}

  return(samp)
}
