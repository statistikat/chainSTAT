# Author: Julia Knoebl
#' Convert time series to dataframe
#'
#' @param x A time series dataframe without colums indicating the time period
#' @param lang Language. Possible values: "de" for German or  "en" for englisch. Defaults to "en"
#'
#' @return a data. frame with (one or) two additional columns to indicate YEAR and QUARTER/MONTH if frequency of input tiome series is 4 or 12 respectively.
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
    warning("The time series appers to be neither quarterly, monthly or yearly data. Column QUARTER/QUARTAL may be missleading.")
    return(tempdf)
  }
}
