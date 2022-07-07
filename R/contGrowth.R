# Author Julia Schieber-Knoebl
#' Contributions to Growth
#'
#' Compile contribution to year-on-year growth rates from a list of Time Series.
#'
#' @param x A list of equally structured Time Series Data Frames. For nom contribution to growth the list is not nessecary it can also be a time series data frame.
#' @param prices selects nominal growth rates ("nom") or chain-linked volumes ("vol"), Defaults to "nom".
#' @param baseline_var Name of the variable of which growth rates should add up to. Usually "BIP". If NULL it looks for variables that are called BIP or GDP, otherwise it takes the first variable.
#' @param ref_lag lag to reference period. defaults to frequency of the series, i.e. yoy growth rates. To get qoq growth rates change to 1.
#'
#' @return A Time series object w/ the same dimensions as the input list elements
#' @details As chain-linked volumes are not additive, values have to be adapted to be additive.
#'          The Additive Volume Data (AVD) Method, where weights are derived from deflators is recommended by EUROSTAT
#'          \deqn{C.AVD(x_t, X)^{c, y-1 \rightarrow c, y} = \frac{P_{x_t}^{y-1}}{P_{X}^{y-1}}\frac{(x_{CVt}^{c,y}- x_{CVt}^{c,y-1})}{X_{CV}^{c,y-1}} +   (\frac{x_{CVt}^{c,y-1}}{X_{CV}^{c,y-1}}-\frac{x_{CVt}^{y-1}}{X_{CV}^{y-1}})*(\frac{P_{x_t}^{y-1}}{P_{X}^{y-1}}-\frac{P_{x_t}^{y-2}}{P_{X}^{y-2}})}
#' @export

contGrowth <- function(x,
                       prices = "nom",
                       baseline_var = NULL,
                       ref_lag = NULL) {
  frq <- NULL
  wb_simple <- NULL
  wb_exact <- NULL
  aDef <- NULL
  pi_ratioA <- NULL
  pi_ratio <- NULL
  pi_ratioA2 <- NULL
  pi_ratio2 <- NULL
  q_share <- NULL
  y_shareA <- NULL
  y_share <- NULL

  if(!(prices %in% c("nom", "vol"))){
    warning( "Possible values are nom an vol")
  }

  if(!is.list(x)){
    if(prices == "vol"){
      stop("IF Contributions to growth are in chain linked columes, the input has to be a list with at least currenat prices (cup/nom) and previous year prices (pyp/vjp).")
    } else if (prices == "nom"){
      x <- list(nom = x)
    }
  }
  if(!("nom" %in% names(x))) {
    if ("V" %in% names(x)) {
      names(x)[names(x) == "V"] <- "nom"
    } else {
      if ("cup" %in% names(x)) {
        names(x)[names(x) == "cup"] <- "nom"
      } else {
      warning(paste("Input is a list, but no elemt is named cup, nom or V.",
                    "The first element in the list, named",
                    names(x)[1],
                    "is assumed to represent values at current prices"))
      names(x)[1] <- "nom"
      }
    }
  }
  if(!("vjp" %in% names(x)) &
     prices == "vol") {
    if ("Y" %in% names(x)) {
      names(x)[names(x) == "Y"] <- "vjp"
    } else {
      if ("Y" %in% names(x)) {
        names(x)[names(x) == "pyp"] <- "vjp"
      } else {
        if (length(x) >= 2) {
          warning(
            paste(
              "Input is a list, but no elemt is named pyp, vjp or Y.",
              "The second element in the list, named",
              names(x)[2],
              "is assumed to represent values at previous years prices "
            )
          )
          names(x)[2] <- "vjp"
        } else{
          stop(
            "To compute contribution to growth at constant prices the input needs to be at least current prices and pyp- values in seperate list elements."
          )
        }
      }
    }
  }
  if(!("vol" %in% names(x)) & prices == "vol"){
    if("L" %in% names(x)){
      names(x)[names(x) == "L"] <- "vol"
    } else {
      warning(paste("The input is a list, but no list element is named vol or L.",
                    "chain linked values have been computed from cup and pyp values."))
      x$vol <- chainSTAT::chainlinkDF(x)
    }
  }
  # BIP name sagt wie die Spalte heisst an der der WB gemessen wird
  if (is.null(baseline_var)){
    baseline_var <- grep("GDP|BIP", colnames(x$nom), value = T)[1]
    if (is.na(baseline_var)){
      baseline_var <- colnames(x$nom)[1]
      warning(paste("baseline_var has not been specified. ",
                    "There is no vairiable named GDP or BIP",
                    "The first column with the variable ",
                    baseline_var,
                    "is used as reference for contribution to growth"))
    }
  }
  frq <- stats::frequency(x[[prices]])
  if(is.null(ref_lag)){
    ref_lag <- frq
  }
  wb_simple <-`colnames<-`(diff(x[[prices]], ref_lag)/stats::lag(x[[prices]][, baseline_var], -ref_lag) * 100,
                           colnames(x[[prices]]))

  if(prices == "vol"){
    aDef <- (stats::aggregate(x$nom)/stats::aggregate(x$vjp))  %>%
      `colnames<-`(colnames(x$nom))

    pi_ratioA <- (stats::lag(aDef, -1)/stats::lag(aDef[, baseline_var] , -1))%>%
      `colnames<-`(colnames(x$nom))
    pi_ratio <- pi_ratioA[rep(1:nrow(pi_ratioA), each = frq),] %>%
      stats::ts(start = stats::start(pi_ratioA),
                frequency = frq)
    pi_ratioA2 <- (stats::lag(aDef, -2)/stats::lag(aDef[, baseline_var] , -2))%>%
      `colnames<-`(colnames(x$nom))
    pi_ratio2 <- pi_ratioA2[rep(1:nrow(pi_ratioA2), each = frq),] %>%
      stats::ts(start = stats::start(pi_ratioA2),
                frequency = frq)

    q_share <- stats::lag(x$vol / x$vol[, baseline_var], -frq)%>%
      `colnames<-`(colnames(x$vol))

    y_shareA <- stats::lag(stats::aggregate(x$vol) / stats::aggregate(x$vol[, baseline_var]), -1) %>%
      `colnames<-`(colnames(x$vol))

    y_share <- y_shareA[rep(1:nrow(y_shareA), each = frq),] %>%
      stats::ts(start = stats::start(y_shareA),
                frequency = frq)

    wb_exact <- (pi_ratio * wb_simple + (q_share- y_share) * (pi_ratio - pi_ratio2) * 100) %>%
      `colnames<-`(colnames(x$vol))
    return(wb_exact)
  } else {
    return(wb_simple)
  }

}
