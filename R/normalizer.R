#' Normalize a name
#'
#' @export
#' @param string A string
#' @examples
#' normalize(string="Helianthus annuüos  ")
normalize <- function(string){
  string <- str_trim(string, "both")
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
  str_trim(ss, "both")
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
  str_trim(ss, "both")
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

#' UTF-8 to ASCII
#' @export
#' @keywords internal
#' @param string A string to convert any utf8 to ascii
utf8_to_ascii <- function(string){
#   ss <- gsub("×", "x", string)
  ss <- gsub('\u00D7', "x", string)
#   ss <- gsub('[ÀÂÅÃÄÁẤẠÁáàâåãäăãắảạậầằá]', "A", ss)
  ss <- gsub(letter_a, "A", ss)
#   ss <- gsub('[ÉÈÊËéèêëĕěếệểễềẻ]', "E", ss)
  ss <- gsub(letter_e, "E", ss)
#   ss <- gsub('[ÍÌÎÏíìîïǐĭīĩỉï]', "I", ss)
  ss <- gsub(letter_i, "I", ss)
#   ss <- gsub('[ÓÒÔØÕÖỚỔóòôøõöŏỏỗộơọỡốơồờớổő]', "O", ss)
  ss <- gsub(letter_o, "O", ss)
#   ss <- gsub('[ÚÙÛÜúùûüůưừựủứụű]', "U", ss)
  ss <- gsub(letter_u, "U", ss)
#   ss <- gsub('[Ýýÿỹ]', "Y", ss)
  ss <- gsub('[\u00DD\u00FD\u00FF\u1EF9]', "Y", ss)
#   ss <- gsub('[Ææ]', "AE", ss)
  ss <- gsub('[\u00C6\u00E6]', "AE", ss)
#   ss <- gsub('[ČÇčćç]', "C", ss)
  ss <- gsub('[\u010C\u00C7\u010D\u0107\u00E7]', "C", ss)
#   ss <- gsub('[ŠŞśšşſ]', "S", ss)
  ss <- gsub('[\u0160\u015E\u015B\u0161\u015F\u017F]', "S", ss)
#   ss <- gsub('[Đđð]', "D", ss)
  ss <- gsub('[\u0110\u0111\u00F0]', "D", ss)
#   ss <- gsub('Žžź', "Z", ss)
  ss <- gsub('[\u017D\u017E\u017A]', "Z", ss)
#   ss <- gsub('[Ññńň]', "N", ss)
  ss <- gsub('[\u00D1\u00F1\u0144\u0148]', "N", ss)
#   ss <- gsub('[Œœ]', "OE", ss)
  ss <- gsub('[\u0152\u0153]', "OE", ss)
#   ss <- gsub('ß', "B", ss)
  ss <- gsub('\u00DF', "B", ss)
#   ss <- gsub('Ķ', "K", ss)
  ss <- gsub('\u0136', "K", ss)
#   ss <- gsub('ğ', "G", ss)
  ss <- gsub('\u011F', "G", ss)
#   ss <- gsub('[Řř]', "R", ss)
  ss <- gsub('[\u0158\u0159]', "R", ss)
  gsub('[[:space:]]{2,}', ' ', ss)
}

letter_a <- '[\u00C0\u00C2\u00C5\u00C3\u00C4\u00C1\u1EA4\u1EA0\u00E1\u00E0\u00E2\u00E5\u00E3\u00E4\u0103\u00E3\u1EAF\u1EA3\u1EA1\u1EAD\u1EA7\u1EB1\u00E1]'
letter_e <- '[\u00C9\u00C8\u00CA\u00CB\u00E9\u00E8\u00EA\u00EB\u0115\u011B\u1EBF\u1EC7\u1EC3\u1EC5\u1EC1\u1EBB]'

letter_i <- '[\u00CD\u00CC\u00CE\u00CF\u00ED\u00EC\u00EF\u01D0\u012D\u012B\u0129\u1Ec9\u00EF]'
letter_o <- '[\u00D3\u00D2\u00D4\u00D8\u00D5\u00D6\u1EDA\u1ED4\u00F3\u00F2\u00F4\u00F8\u00F5\u00F6\u014F\u1ECF\u1ED7\u1ED9\u01A1\u1ECD\u1EE1\u1ED1\u01A1\u1ED3\u1EDD\u1EDB\u1ED5\u0151]'
letter_u <- '[\u00DA\u00D9\u00DB\u00DC\u00FA\u00F9\u00FB\u00FC\u016F\u01B0\u1EEB\u1EF1\u1EE7\u1EE9\u1EE5\u0171]'











