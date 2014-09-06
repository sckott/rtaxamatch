#' Normalize a name
#'
#' @export
#' @param string A string
#' @examples
#' normalize(string="Helianthus annuüos  ")
normalize <- function(string){
  string <- trm_ends(string)
  string <- toupper(string)
  res <- utf8_to_ascii(string)
  gsub('[^\x20-\x7F]', '?', res)
}

#' Normalize a word
#'
#' @export
#' @param word A word
#' @examples
#' normalize_word(word='Helianthus  ')
#' normalize_word(word='G9-Helianthus  ')
normalize_word <- function(word){
  ss <- gsub('^[A-Z][0-9]-', '', word)
  ss <- normalize(ss)
  trm_ends(ss)
}

#' Normalize an author
#'
#' @export
#' @param string A string of author(s)
#' @examples
#' normalize_author(string='Gunther, Brunkal')
normalize_author <- function(string){
  ss <- normalize(string)
  ss <- gsub('[^A-Z]', ' ', ss)
  ss <- gsub('[\\s]{2,}', ' ', ss)
  trm_ends(ss)
}

#' Normalize a year
#'
#' @export
#' @param year A year string
#' @examples
#' normalize_year(year='2010')
normalize_year <- function(year){
#   year_int <-
#     gsub('[^0-9]', '', year_string)
  year <- as.numeric(year)
  test <- 1757 < year && year < as.numeric(gsub("-[0-9]+", "", Sys.Date()))
  if(!test) NULL else year
}

utf8_to_ascii <- function(string){
  ss <- gsub("×", "x", string)
  ss <- gsub('[ÀÂÅÃÄÁẤẠÁáàâåãäăãắảạậầằá]', "A", ss)
  ss <- gsub('[ÉÈÊËéèêëĕěếệểễềẻ]', "E", ss)
  ss <- gsub('[ÍÌÎÏíìîïǐĭīĩỉï]', "I", ss)
  ss <- gsub('[ÓÒÔØÕÖỚỔóòôøõöŏỏỗộơọỡốơồờớổő]', "O", ss)
  ss <- gsub('[ÚÙÛÜúùûüůưừựủứụű]', "U", ss)
  ss <- gsub('[Ýýÿỹ]', "Y", ss)
  ss <- gsub('[Ææ]', "AE", ss)
  ss <- gsub('[ČÇčćç]', "C", ss)
  ss <- gsub('[ŠŞśšşſ]', "S", ss)
  ss <- gsub('[Đđð]', "D", ss)
  ss <- gsub('Žžź', "Z", ss)
  ss <- gsub('[Ññńň]', "N", ss)
  ss <- gsub('[Œœ]', "OE", ss)
  ss <- gsub('ß', "B", ss)
  ss <- gsub('Ķ', "K", ss)
  ss <- gsub('ğ', "G", ss)
  ss <- gsub('[Řř]', "R", ss)
  gsub('[[:space:]]{2,}', ' ', ss)
}
