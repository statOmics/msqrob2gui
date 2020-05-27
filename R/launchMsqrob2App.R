#' Launches the msqrob2 Shiny App
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @export launchMsqrob2App
#'
#' @return shiny application object
#'
#' @example
#' \dontrun{launchMsqrob2App()}
#'
#' @import shiny shinyjs
#'


# wrapper for shiny::shinyApp()
launchMsqrob2App <- function(maxSize=500) {
  shinyjs::useShinyjs()
  options(shiny.maxRequestSize=maxSize*1024^2)
  shinyApp(ui = msqrob2UI, server = msqrob2Server)
}
