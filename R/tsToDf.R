# Author: Julia Knoebl
#' Convert TS to dataframe
#'
#' @param x A time series dataframe without colums indicating the time period
#' @param lang Language german ("de") or englisch "en" colnames (YEAR, QUARTER). Defaukts to "en"
#'
#' @return a data. frame
#' @export

tsToDf <- function(x,
                   lang = "en") {
  tempdf <- data.frame(YEAR  = as.numeric(floor(stats::time(x))),
                       QUARTER = as.numeric(stats::cycle(x)),
                       x)
  colnames(tempdf) <- gsub("^X(?=[0-9])", "", colnames(tempdf), perl = TRUE)
  if (lang == "de"){
    colnames(tempdf)[1:2] <- c("JAHR", "QUARTAL")
  }
  if (stats::frequency(x) == 4){
    return(tempdf)
  } else if(stats::frequency(x) == 1){
    return(tempdf[, -2])
  } else if(stats::frequency(x) == 12) {
    if(lang == "en"){
      colnames(tempdf)[2] <- "MONTH"
    } else if (lang == "de") {
      colnames(tempdf)[2] <- "MONAT"
    }
    return(tempdf)
  } else {
    warning("ACHTUNG: die Zeitreihe scheint weder Quartals, noch monats , noch Jahresdaten zu enthalten, die Spalte QUARTER/QUARTAL ist daher evtl nicht aussagekraeftig.")
    return(tempdf)
  }
}
