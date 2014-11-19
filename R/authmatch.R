#' Authmatch
#'
#' @name authmatch
#' @export
#' @examples
#' authmatch(authors1='Linnaeus', authors2=c('L','Muller'), years1=1786, years2=1787)
#' remove_duplicate_authors(authors1='Linnaeus', authors2=c('L','Muller'))
#' fuzzy_match_authors("Linnaeus", "linnaeus")
#' fuzzy_match_authors("Linnaeus", "linnaeuS")
#' authmatch(c('Linnaeus', 'Muller'), 'L', 1789, 1788)

#' @export
#' @rdname authmatch
authmatch <- function(authors1, authors2, years1, years2){
  tmp <- remove_duplicate_authors(authors1, authors2)
  unique_authors1 <- tmp[1]
  unique_authors2 <- tmp[2]
  year_diff <- compare_years(years1, years2)
  get_score(authors1, unique_authors1, authors2, unique_authors2, year_diff)
}

#' @export
#' @rdname authmatch
get_score <- function(authors1, unique_authors1, authors2, unique_authors2, year_diff=NULL){
  count_before <- length(authors1) + length(authors2)
  count_after <- length(unique_authors1) + length(unique_authors2)
  score <- 0
  if( count_after == 0 ){
    if( !is.null(year_diff) ){
      if( year_diff == 0 ){
        score <- 100
      } else if( year_diff == 1 ){
        score <- 54
      }
    } else { score <- 94 }
  } else if( length(unique_authors1) == 0 || length(unique_authors2) == 0 ){
    if( !is.null(year_diff) ){
      if( year_diff == 0 ){
        score <- 91
      } else if( year_diff == 1 ){
        score <- 51
      }
    } else {
      score <- 90
    }
  } else {
    score <- round((1 - count_after/count_before) * 100)
#     score <- if( is.null(year_diff) || year_diff && year_diff == 0 ) 0 else score
  }
  if( score > 50 ) score else 0
}

#' @export
#' @rdname authmatch
remove_duplicate_authors <- function(authors1, authors2){
  unique_authors1 <- unique(authors1)
  unique_authors2 <- unique(authors2)

  for(i in seq_along(authors1)){
    for(j in seq_along(authors2)){
      au1_match = au2_match = FALSE
      if( authors1[i] == authors2[j] ){
        au1_match = au2_match = TRUE
      } else if( authors1[i] == substring(authors2[j], 1, nchar(authors1[i])) ){
        au1_match = TRUE
      } else if( substring(authors1[i], 1, nchar(authors2[j])) == authors2[j] ){
        au2_match = TRUE
      }
      if( nchar(authors1[i]) >= 3 && au1_match || nchar(authors2[j]) >= 3 && au2_match || au1_match && au2_match ){
        unique_authors1 <- unique_authors1[ unique_authors1 != authors1[i] ]
        unique_authors2 <- unique_authors2[ unique_authors2 != authors2[j] ]
      } else if( au1_match ){
        unique_authors1 <- unique_authors1[ unique_authors1 != authors1[i] ]
      } else if( au2_match ){
        unique_authors2 <- unique_authors2[ unique_authors2 != authors2[j] ]
      } else {
        if( nchar(authors1[i]) > 1 && nchar(authors2[j]) > 1 && fuzzy_match_authors(authors1[i], authors2[j]) ){
          unique_authors1 <- unique_authors1[ unique_authors1 != authors1[i] ]
          unique_authors2 <- unique_authors2[ unique_authors2 != authors2[j] ]
        }
      }
    }
  }
  c(unique_authors1, unique_authors2)
}

#' @export
#' @rdname authmatch
fuzzy_match_authors <- function(author1, author2){
  au1_length <- nchar(author1)
  au2_length <- nchar(author2)
  ed <- stringdist(author1, author2, weight = c(1,1,1,1), maxDist = 3)
  ed <= 3 && min(c(au1_length, au2_length)) > (ed * 2) && (ed < 2 || author1[1] == author2[1])
}

#' @export
#' @rdname authmatch
compare_years <- function(years1=NULL, years2=NULL){
  if( is.null(years1) && is.null(years2) ) {
    return( 0 )
  } else if( length(years1) == 1 && length(years2) == 1 ) {
    return( abs(years1[1] - years2[1]) )
  } else { NULL }
}
