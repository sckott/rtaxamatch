#' Capitalize the first letter of a character string.
#'
#' @export
#' @param s A character string
#' @param strict Should the algorithm be strict about capitalizing. Defaults to FALSE.
#' @param onlyfirst Capitalize only first word, lowercase all others. Useful for
#'   	taxonomic names.
#' @examples
#' txm_capwords(c("using AIC for model selection"))
#' txm_capwords(c("using AIC for model selection"), strict=TRUE)

txm_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)), { s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
  if(!onlyfirst){
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else
  {
    sapply(s, function(x) paste(toupper(substring(x,1,1)), tolower(substring(x,2)), sep="", collapse=" "), USE.NAMES=F)
  }
}

squeeze <- function(x) gsub("([[:alnum:]])(\\1)+", "\\1", x, perl=TRUE)

#' Replace x with y in a string
#' @export
#' @param str Target string
#' @param src Characters to replace
#' @param repl Replacement characters
#' @examples
#' tr(str='HELIANTHUS ANNUS', src='EOUYKZ', repl='IAIICS')
tr <- function(str, src, repl){
  foo <- function(x,y,z) gsub(x, y, z)

  src <- strsplit(src, '')[[1]]
  repl <- strsplit(repl, '')[[1]]

  for(i in seq_along(src)){
    str <- foo(src[i], repl[i], str)
  }
  str
}
