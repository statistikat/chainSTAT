# Author: Julia Knoebl
#' Erstellt einen Data Frame mit veketten Volumen
#'
#' @param df_list Eine Liste, bei der die Elemente jeweils ein Data Frame sind mit nominellen Werten bzw. Werten zu VJP.
#' @param basis Referenzjahr für Verkettung. Default ist 2015
#' @param ts_output logical. Soll der Output eine Zeitreihe sein, oder nicht. Defaults to FALSE
#'
#' @return einen data.table, die Werte sind verkette Volumen
#' @export

chainlinkDF <- function(df_list,
                        ts_output = F,
                        basis = 2015) {
  nom_name <- NULL
  vjp_name <- NULL
  vol_name <- NULL

  if(!is.list(df_list)){
    stop("Der Input sollte eine Liste von gleich aufgebauten Data Frames sein, wobei nominelle und vjp Werte in seperaten Elementen der Liste sind")
  }
  if ("nom" %in% names(df_list)){
    nom_name <- "nom"
    vol_name <- "vol"
  } else if ("V" %in% names(df_list)){
    nom_name <- "V"
    vol_name <- "L"
  } else {
    stop("Die Listenelemente sollten entsprechend benannt sein: nom oder V fuer nominielle Werte, vjp oder Y fuer Werte zu VJP")
  }

  if ("vjp" %in% names(df_list)){
    vjp_name <- "vjp"
  } else if ("Y" %in% names(df_list)){
    vjp_name <- "Y"
  } else {
    stop("Die Listenelemente sollten entsprechend benannt sein: nom oder V fuer nominielle Werte, vjp oder Y fuer Werte zu VJP")
  }

  # da alle gleich aufgebaut sein  sollte, reicht es, wenn ich nom checke. fuer spaeter kann man andenken eine bessere year/quarter erkennung zu machen
  if(stats::is.ts(df_list[[nom_name]])){
    listTS <- df_list
  } else {
    listTS <- lapply(df_list, function(x){
      x [, -c("YEAR", "QUARTER")] %>%
        stats::ts(start = c(x$YEAR[1], x$QUARTER[1]),
                  frequency = 4)
    })
  }

  listTS[[vol_name]] <- do.call(cbind, lapply(colnames(listTS[[nom_name]]), function(x){
    if(sum(stats::na.omit(listTS[[nom_name]][, x]) < 0) == 0){
      chainlinkAO(nom = listTS[[nom_name]][, x],
                            vjp = listTS[[vjp_name]][, x],
                            basis = basis)
    } else {
      NA
    }
  })) %>%
    `colnames<-`(colnames(listTS[[nom_name]]))

  if(ts_output){
    return(listTS[[vol_name]])
  } else {
    df_list[[vol_name]] <-
      tsToDf(listTS[[vol_name]])
    return(df_list[[vol_name]])
  }
}
