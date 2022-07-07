# Author Julia Knoebl
#' Creates chainlinked time series with annual overlap
#'
#' Using the annual overlap approach this function chainlinks univariate time series with respect to the given base year.
#' @param cup univariate time series at current prices (cup)
#' @param pyp univariate time series at previous year prices (pyp)
#' @param ref_year Base year. Defaults to 2010
#' @param index If TRUE it returns an index series, where the base year is 100, if FALSE returns volume series. Defaults to FALSE.
#'
#' @return a univariate time series with volume or index values
#' @details For more information on Annual Overlap consult the IMF Manual for QNA (p.185, §71) or Eurostats Handbook on QNA (p. 184, §6.47)
#' @export

chainlinkAO <- function(cup,
                        pyp,
                        ref_year = 2015,
                        index = F) {
  # delete NA values to avoid resulting errors
  cup <- stats::na.omit(cup)
  pyp <- stats::na.omit(pyp)

  if (stats::time(cup)[1] > stats::time(pyp)[1]){
    pyp <- stats::window(pyp, start = stats::start(cup))
  } else if (stats::time(cup)[1] < stats::time(pyp)[1]){
    cup <- stats::window(cup, start = stats::start(pyp))
  }
  if (stats::time(cup)[length(cup)] < stats::time(pyp)[length(pyp)]){
    pyp <- stats::window(pyp, end = stats::end(cup))
  } else if (stats::time(cup)[length(cup)] > stats::time(pyp)[length(pyp)]){
    cup <- stats::window(cup, end = stats::end(pyp))
  }

  # the following errors should only ocur when the truncation of series failes
  if (length(cup) != length(pyp)) {
    stop("Values at current (cup) and previous year prices (pyp) need to have the same length")
  }
  if (stats::frequency(cup) != stats::frequency(pyp)) {
    stop("Values at current (cup) and previous year prices (pyp) need to have the same frequency")
  }
  if (all(stats::start(cup) == stats::start(pyp)) == F) {
    warning("Values at current (cup) and previous year prices (pyp) should start at the same time")
  }

  vol <- NULL

  if (stats::frequency(cup) == 1) {
    growthRate <- log(pyp / stats::lag (cup, k = -1))
    vol <-
      stats::ts(NA,
                start = stats::start(cup),
                end = stats::end(cup))
    vol[stats::time(vol) == ref_year] <- cup[stats::time(cup) == ref_year]
    vol[stats::time(vol) > ref_year] <-
      cup[stats::time(cup) == ref_year] * exp(cumsum(growthRate[stats::time(growthRate) > ref_year]))
    vol[stats::time(vol) < ref_year] <-
      cup[stats::time(cup) == ref_year] / rev(exp(cumsum(rev(growthRate[stats::time(growthRate) <= ref_year]))))
  } else {
    # which is the last quarter?
    lastq <- stats::cycle(cup)[length(cup)]

    # Yearly averages (of the available points in time)
    av <- lapply(c("cup", "pyp"), function(x){
      input <- get(x)
      zr <- rep(stats::aggregate(input, FUN = mean),
                each = stats::frequency(input))
      if(lastq < 4){
        zr <- c(zr,
                rep(mean(stats::window(input, start = stats::end(input)[1])),
                    lastq))
      }
      stats::ts(zr,
                start = stats::start(input),
                frequency = stats::frequency(input))
    }) %>%
      `names<-`(c("cup", "pyp"))

    # at yearly averaged prices
    jdp <- cup *
      (av$cup / av$pyp) /
      (cup / pyp)

    vol <-
      stats::ts(
        NA,
        start = stats::start(jdp),
        end = stats::end(jdp),
        frequency = stats::frequency(jdp)
      )

    # in the base year it equals the yearly averages
    vol[stats::time(vol) >= ref_year &
          stats::time(vol) < (ref_year + 1)] <-
      jdp[stats::time(vol) >= ref_year &
            stats::time(vol) < (ref_year + 1)]

    # calculate growth rates before and after
    # only if there are observations before base year
    if(stats::time(vol)[1] < ref_year){
      before <- stats::window(jdp / stats::lag(jdp),
                              end = c(ref_year, 0))
      # To ensure continious TS over the years we correct
      #  - the 4th quarter BEFORE the base year (Q4 at yearly average/Q1 at pyp (same price!)  -> growth rate)
      #  - the 1st quarter AFTER the base year (Q1 at pyp/Q4 at yearly average (same price!)  -> growth rate)
      q4_cor <- jdp / stats::lag(pyp)

      before[(stats::cycle(before) == 4)] <- q4_cor[(stats::cycle(q4_cor) == 4) & (stats::time(q4_cor) < ref_year)]
      # Cumulative products of growth rates on the base year
      vol[stats::time(vol) < ref_year] <-
        exp(rev(cumsum(rev(log(before))))) * vol[stats::time(vol) == ref_year]
    }
    # only if there are observations after the base year
    if(stats::time(vol)[length(vol)]>= ref_year + 1){
      after <- stats::window(jdp / stats::lag(jdp, k = -1),
                             start = ref_year + 1)
      # To ensure continious TS over the years we correct
      #  - the 4th quarter BEFORE the base year (Q4 at yearly average/Q1 at pyp (same price!)  -> growth rate)
      #  - the 1st quarter AFTER the base year (Q1 at pyp/Q4 at yearly average (same price!)  -> growth rate)
      q1_cor <- pyp / stats::lag(jdp, k = -1)

      after[(stats::cycle(after) == 1)] <- q1_cor[(stats::cycle(q1_cor) == 1) & (stats::time(q1_cor) >= (ref_year + 1))]


      # Cumulative products of growth rates on the base year
      vol[stats::time(vol) >= (ref_year + 1)] <-
        exp(cumsum(log(after))) * vol[stats::time(vol) == ref_year + 1 - 1/stats::frequency(vol)]
    }
  }
  if (index) {
    vol <- vol / stats::aggregate(cup, FUN = mean)[stats::time(stats::aggregate(cup)) == ref_year] * 100
  }
  return(vol)
}
