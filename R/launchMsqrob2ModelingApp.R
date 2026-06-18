#' Launches the msqrob2 Shiny App
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @export launchMsqrob2ModelingApp
#'
#' @return shiny application object
#'
#' @examples
#' \dontrun{launchMsqrob2ModelingApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy
#'


# wrapper for shiny::shinyApp()
launchMsqrob2ModelingApp <- function(maxSize=500) {
  shinyjs::useShinyjs()
  options(shiny.maxRequestSize=maxSize*1024^2)
  shinyApp(ui = msqrob2ModelingUI, server = msqrob2ModelingServer)
}
