# Author Julia Knoebl
#' Creates chainlinked time series with annual overlap
#'
#' Using the annual overlap approach this function chainlinks univariate time series with respect to the given base year. Frequency of time series can also be greater than 1.
#' @param nom univariate time series at current prices
#' @param vjp univariate time series at previous year prices
#' @param basis Base year. Defaults to 2010
#' @param index If TRUE it returns an index series, where the base year is 100, if FALSE returns volume series. Defaults to FALSE.
#'
#' @return a univariate time series with volume oder index values
#' @export

chainlinkAO <- function(nom,
                        vjp,
                        basis = 2015,
                        index = F) {
  # NA Werte Löschen, damit ein fehlendes quartal bei den vjp zB keine Fehler im Programm verursacht
  nom <- stats::na.omit(nom)
  vjp <- stats::na.omit(vjp)

  if (stats::time(nom)[1] > stats::time(vjp)[1]){
    vjp <- stats::window(vjp, start = stats::start(nom))
  } else if (stats::time(nom)[1] < stats::time(vjp)[1]){
    nom <- stats::window(nom, start = stats::start(vjp))
  }
  if (stats::time(nom)[length(nom)] < stats::time(vjp)[length(vjp)]){
    vjp <- stats::window(vjp, end = stats::end(nom))
  } else if (stats::time(nom)[length(nom)] > stats::time(vjp)[length(vjp)]){
    nom <- stats::window(nom, end = stats::end(vjp))
  }

  # folgende Fehler sollten nicht auftauchen, nur falls das kuerzen nicht funktioniert.
  if (length(nom) != length(vjp)) {
    stop("Nominelle und Werte zu VJ Basis m\u00FCssen die gleiche L\u00E4nge haben")
  }
  if (stats::frequency(nom) != stats::frequency(vjp)) {
    stop("Nominelle und Werte zu VJ Basis m\u00FCssen die gleiche Frequenz haben")
  }
  if (all(stats::start(nom) == stats::start(vjp)) == F) {
    warning("Nominelle und Werte zu VJ Basis sollten zum gleichen Zeitpunkt anfangen")
  }

  vol <- NULL

  if (stats::frequency(nom) == 1) {
    growthRate <- log(vjp / stats::lag (nom, k = -1))
    vol <-
      stats::ts(NA,
                start = stats::start(nom),
                end = stats::end(nom))
    vol[stats::time(vol) == basis] <- nom[stats::time(nom) == basis]
    vol[stats::time(vol) > basis] <-
      nom[stats::time(nom) == basis] * exp(cumsum(growthRate[stats::time(growthRate) > basis]))
    vol[stats::time(vol) < basis] <-
      nom[stats::time(nom) == basis] / rev(exp(cumsum(rev(growthRate[stats::time(growthRate) <= basis]))))
  } else {
    # welches ist das letzte quartal?
    lastq <- stats::cycle(nom)[length(nom)]

    # Jahresdurchschnitte (fuer das angefangene Jahr den Durchschnitt vom verfuegbaren Zeitraum)
    av <- lapply(c("nom", "vjp"), function(x){
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
      `names<-`(c("nom", "vjp"))

    # zu Jahresdurchschnittspreisen
    jdp <- nom *
      (av$nom / av$vjp) /
      (nom / vjp)

    vol <-
      stats::ts(
        NA,
        start = stats::start(jdp),
        end = stats::end(jdp),
        frequency = stats::frequency(jdp)
      )

    # im Basisjahr gleich zu JDP
    vol[stats::time(vol) >= basis &
          stats::time(vol) < (basis + 1)] <-
      jdp[stats::time(vol) >= basis &
            stats::time(vol) < (basis + 1)]

    # Wachstumsraten davor und danach ausrechnen
    before <- stats::window(jdp / stats::lag(jdp),
                            end = c(basis, 0))
    after <- stats::window(jdp / stats::lag(jdp, k = -1),
                           start = basis + 1)

    # um den Sprung zwischen den Jahren zu korrigieren,
    # wird fuer den Zeitraum VOR dem Basisjahr im 4. Quartal korrigiert ( Wachstum 4. Q JDP / 1. VJDP (gleicher Preis!)
    # NACH dem Basisjahr wird im 1. quartal korrigiert: 1.Q VJDP / 4. Q JDP--> gleicher Preis
    q4_cor <- jdp / stats::lag(vjp)
    q1_cor <- vjp / stats::lag(jdp, k = -1)

    after[(stats::cycle(after) == 1)] <- q1_cor[(stats::cycle(q1_cor) == 1) & (stats::time(q1_cor) >= (basis + 1))]
    before[(stats::cycle(before) == 4)] <- q4_cor[(stats::cycle(q4_cor) == 4) & (stats::time(q4_cor) < basis)]

    # Das kumulierte Produkt der Wachstumsraten wird an das Basisjahr angelegt
    vol[stats::time(vol) >= (basis + 1)] <-
      exp(cumsum(log(after))) * vol[stats::time(vol) == basis + 1 - 1/stats::frequency(vol)]

    vol[stats::time(vol) < basis] <-
      exp(rev(cumsum(rev(log(before))))) * vol[stats::time(vol) == basis]

  }
  if (index) {
    vol <- vol / stats::aggregate(nom, FUN = mean)[stats::time(stats::aggregate(nom)) == basis] * 100
  }
  return(vol)
}
