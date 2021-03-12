# Author Julia Knoebl
#' Growth Rates
#'
#' @param x A time series (uni- or multivariate).
#' @param mode additive or multiplicative growth rates. Defaults to additive.
#' @param ref_lag lag to reference period. Defaults to frequency of the series, i.e. yoy growth rates. To get qoq growth rates change to 1.
#'
#' @return time series with growth rates
#' @export

growthRate <- function(x, mode = "additive", ref_lag = NULL){
  if(is.null(ref_lag)){
    ref_lag <- stats::frequency(x)
  }
  g <- exp(diff(log(x), ref_lag))
  if(grepl("mult", mode)){
    return(g)
  } else if (grepl("add", mode)) {
    return(g * 100 - 100)
  } else {
    stop("Mode should be either additive or multiplicative")
  }
}


