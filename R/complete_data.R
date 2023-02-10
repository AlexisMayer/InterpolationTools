#' @title Complete Data
#'
#' @description Ajouter les dates manquantes à un ensemble de données en détectant la fréquence des données
#'
#' @param data data.frame contenant les colonnes "time" et "value"
#'
#' @import data.table
#'
#' @examples
#' complete_data(my_data)
#'
#' @return Un data.frame avec les dates manquantes ajoutées et les valeurs associées définies comme NA
#'
#' @export
complete_data <- function(data) {
  frequency <- NULL
  time_difference <- difftime(data$time[2], data$time[1], units = "days")
  if (time_difference %in% c(365, 365.25, 365.2425)) frequency <- "year"
  if (time_difference %in% c(90, 91, 92, 93)) frequency <- "quarter"
  if (time_difference %in% c(30, 28, 31)) frequency <- "month"
  if (time_difference %in% c(7)) frequency <- "week"
  if (time_difference %in% c(1)) frequency <- "day"
  
  if (is.null(frequency)) stop("Could not detect frequency. Please specify frequency manually.")
  
  full_time <- data.table(time = seq(min(data$time), max(data$time), by = frequency))
  complete_data <- merge(full_time, data, all.x = TRUE)
  complete_data[is.na(complete_data$value), "value"] <- NA
  return(complete_data)
}
