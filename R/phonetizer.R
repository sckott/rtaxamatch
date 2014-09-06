#' Phonetize
#'
#' @importFrom stringr str_sub str_trim
#' @export
#' @template phenotize
phonetize <- function(a_word, normalize_ending = FALSE){
  near_match(a_word, normalize_ending)
}

#' Near match
#'
#' @export
#' @template phenotize
#' @examples
#' near_match(a_word='Helianthus   annuus')
near_match <- function(a_word, normalize_ending = FALSE){
  a_word <- str_trim(a_word, "both")
  # FIXME, seems that rescue recovers from a failure, not sure what to replace with
  # a_word = a_word.strip rescue ''
  if(a_word == '') return('')
  a_word <- normalize(a_word)
  a_word <- match_beginning(a_word)
  first_char <- strsplit(a_word, "")[[1]][1]
  rest_chars <- paste0(strsplit(a_word, "")[[1]][-1], collapse = "")
  rest_chars <- gsub('AE', 'I', rest_chars)
  rest_chars <- gsub('IA', 'A', rest_chars)
  rest_chars <- gsub('OE', 'I', rest_chars)
  rest_chars <- gsub('OI', 'A', rest_chars)
  rest_chars <- gsub('SC', 'S', rest_chars)
  rest_chars <- gsub('H', '', rest_chars)
  rest_chars <- tr(str=rest_chars, src='EOUYKZ', repl='IAIICS')
  a_word <- squeeze(paste0(first_char, rest_chars))

  if(normalize_ending && nchar(a_word) > 4)
    normalize_ending(a_word)
  else
    a_word
}

ifmatch <- function(target, pattern, replacement){
  if(grepl(pattern, target))
    return(paste0(replacement, str_sub(target, 3, nchar(target))))
  else
    return(target)
}

match_beginning <- function(x){
  ifmatch(x, '^AE', 'E')
  ifmatch(x, '^CN', 'N')
  ifmatch(x, '^CT', 'T')
  ifmatch(x, '^CZ', 'C')
  ifmatch(x, '^DJ', 'J')
  ifmatch(x, '^EA', 'E')
  ifmatch(x, '^EU', 'U')
  ifmatch(x, '^GN', 'N')
  ifmatch(x, '^KN', 'N')
  ifmatch(x, '^MC', 'MAC')
  ifmatch(x, '^MN', 'N')
  ifmatch(x, '^OE', 'E')
  ifmatch(x, '^QU', 'Q')
  ifmatch(x, '^PS', 'S')
  ifmatch(x, '^PT', 'T')
  ifmatch(x, '^TS', 'S')
  ifmatch(x, '^WR', 'R')
  ifmatch(x, '^X', 'Z')
}

#' Noramlize endings.
#'
#' @export
#' @param a_word (character) A character string. Required.
#' @examples
#' normalize_ending(a_word='HELIANTHIS')
#' normalize_ending(a_word='TIM')
#' normalize_ending(a_word='RAXTAS')
normalize_ending <- function(a_word){
  # -- deal with variant endings
  # -is (includes -us, -ys, -es), -im (was -um), -as (-os)
  # -- at the end of a string translate all to -a
  gsub('IS$|IM$|AS$', 'A', a_word)
}
