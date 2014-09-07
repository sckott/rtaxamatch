#' unicode_letter(letter='Â')
#' unicode_letter(unicode='0xC2')
unicode_letter <- function(letter=NULL, unicode=NULL){
  dat <- data.frame(
    letter=c('À','Â','Å','Ã','Ä','Á','Ấ','Ạ','á','à','â','å','ã','ä','ă','ã','ắ','ả','ạ','ậ','ầ','ằ','á'),
    unicode=c('\u00C0','\u00C2','\u00C5','\u00C3','\u00C4','\u00C1','\u1EA4','\u1EA0','\u00E1','\u00E0','\u00E2','\u00E5','\u00E3','\u00E4','\u0103','\u00E3','\u1EAF','\u1EA3','\u1EA1','\u1EAD','\u1EA7','\u1EB1','\u00E1'),
    code=c('00C0','00C2','00C5','00C3','00C4','00C1','1EA4','1EA0','00E1','00E0','00E2','00E5','00E3','00E4','0103','00E3','1EAF','1EA3','1EA1','1EAD','1EA7','1EB1','00E1'),
    stringsAsFactors = FALSE
  )
  if(is.null(unicode)){
    if(any(grepl(letter, dat$letter)))
      dat[grep(letter, dat$letter), "code"]
    else
      "no match"
  } else {
    intToUtf8(unicode)
  }
}
