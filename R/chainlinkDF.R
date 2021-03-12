# Author: Julia Knoebl
#' Get a data frame of chained volumes
#'
#' @param df_list A list, where the list elements are each a data frame consisting of current price (cup) and previous year price (pyp) values.
#' @param ref_year Reference year for Annual Overlap. Defaults to 2015.
#'
#' @return a data.frame of chained volumes.
#' @export

chainlinkDF <- function(df_list,
                        ref_year = 2015) {
  nom_name <- NULL
  vjp_name <- NULL
  vol_name <- NULL
  freq <- NULL
  freq_idx <- NULL
  year_idx <- NULL
  output <- NULL


  if(!is.list(df_list)){
    stop("The input needs to be a list of equally constructed data sets, where cup and pyp values are each in a seperate list element.")
  }
  if ("nom" %in% names(df_list)){
    nom_name <- "nom"
    vol_name <- "vol"
  } else if ("cup" %in% names(df_list)){
    nom_name <- "cup"
    vol_name <- "vol"
  } else if ("V" %in% names(df_list)){
    nom_name <- "V"
    vol_name <- "L"
  }else {
    stop("The elements of the list need to be named accordingly: cup, nom or V fuer current price values, pyp, vjp or Y for values at pyp")
  }

  if ("vjp" %in% names(df_list)){
    vjp_name <- "vjp"
  } else if ("pyp" %in% names(df_list)){
    vjp_name <- "pyp"
  } else if ("Y" %in% names(df_list)){
    vjp_name <- "Y"
  } else {
    stop("The elements of the list need to be named accordingly: cup, nom or V fuer current price values, pyp, vjp or Y for values at pyp")
  }

  # as all data sets should be constructed equally we only check for cup. Can be extended to check for year/quarter in all elements

  if(stats::is.ts(df_list[[nom_name]])){
    listTS <- df_list
    output <- "TS"
  } else {
    year_idx <- which(toupper(colnames(df_list[[nom_name]])) %in% c("JAHR", "YEAR"))
    if ("QUARTAL" %in% toupper(colnames(df_list[[nom_name]]))| "QUARTER" %in% toupper(colnames(df_list[[nom_name]]))){
      freq <- 4
      freq_idx <- which(toupper(colnames(df_list[[nom_name]])) %in% c("QUARTER", "QUARTAL"))
    } else if ("MONAT" %in% toupper(colnames(df_list[[nom_name]]))| "MONTH" %in% toupper(colnames(df_list[[nom_name]]))){
      freq <- 12
      freq_idx <- which(toupper(colnames(df_list[[nom_name]])) %in% c("MONAT", "MONTH"))
    } else {
      freq <- 1
      freq_idx <- integer(0)
    }
    listTS <- lapply(df_list, function(x){
      x <- as.data.frame(x)
      x[, c(-year_idx, -freq_idx)] %>%
        stats::ts(start = c(x[1, year_idx], x[1, freq_idx[2]]),
                  frequency = freq)
    })
    if (is.data.frame(df_list[[nom_name]])){
      output <- "DF"
    } else if (data.table::is.data.table(df_list[[nom_name]])){
      output <- "DT"
    } else {
      output <- "DF"
    }
  }

  listTS[[vol_name]] <- do.call(cbind, lapply(colnames(listTS[[nom_name]]), function(x){
    if(sum(stats::na.omit(listTS[[nom_name]][, x]) < 0) == 0){
      chainlinkAO(cup = listTS[[nom_name]][, x],
                  pyp = listTS[[vjp_name]][, x],
                  ref_year = ref_year)
    } else {
      NA
    }
  })) %>%
    `colnames<-`(colnames(listTS[[nom_name]]))

  if(output == "TS"){
    return(listTS[[vol_name]])
  } else {
    df_list[[vol_name]] <-
      tsToDf(listTS[[vol_name]])
    if(output == "DF"){
      return(df_list[[vol_name]])
    } else {
      return(data.table::as.data.table(df_list[[vol_name]]))
    }
  }
}
