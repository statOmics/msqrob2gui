#' Launches the msqrob2 Shiny App
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @export launchMsqrob2PreprocessingApp
#'
#' @return shiny application object
#'
#' @example
#' \dontrun{launchMsqrob2ModelingApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy
#'


# wrapper for shiny::shinyApp()
launchMsqrob2PreprocessingApp <- function(maxSize=500) {
  shinyjs::useShinyjs()
  options(shiny.maxRequestSize=maxSize*1024^2)
  shinyApp(ui = msqrob2PreprocessingUI, server = msqrob2PreprocessingServer)
}
