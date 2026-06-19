#' Launches the msqrob2 Data Processing Shiny App
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @export launchMsqrob2DataProcessingApp
#'
#' @return shiny application object
#'
#' @examples
#' \dontrun{launchMsqrob2DataProcessingApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy
#'

# wrapper for shiny::shinyApp()
launchMsqrob2DataProcessingApp <- function(maxSize = 500) {
  shinyjs::useShinyjs()
  options(shiny.maxRequestSize = maxSize * 1024^2)
  shinyApp(ui = msqrob2DataProcessingUI, server = msqrob2DataProcessingServer)
}
