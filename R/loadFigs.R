#' loadFigs
#'
#' Tool for Loading Calculations and Pretty Figure Production
#'
#' Shiny application that uses user-uploaded pollutant and flow datasets to calculate observed and TMDL loadings and make it easy to produce publication-ready dot plots, bar plots, and load duration curves.
#'
#' @import shiny
#' @import shinyjs
#' @import shinyBS
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import sortable
#' @import RColorBrewer
#' @importFrom magrittr %>%

#' @export
loadFigs = function(){
  shiny::runApp(system.file('loadFigs', package='dwqInsights'))
  }
