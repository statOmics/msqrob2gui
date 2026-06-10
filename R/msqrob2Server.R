#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny reactiveValues


# Define server
msqrob2Server <- function(input, output, session) {
  shinyjs::useShinyjs()

  ### servers
   ### input
  shinyjs::useShinyjs()

  global_variables <- reactiveValues(pe = QFeatures(),
                                     selectedAssay = NULL,
                                     selectedLowLevelAssay = NULL) #Has to be done globally

  importServerInput <- importServer(variables = global_variables)

  modelServerInput <- modelServer(variables = global_variables)

  inferenceServerInput <- inferenceServer(variables = global_variables,
                                          inputServerInput = inputServerInput)

  reportServer <- reportServer(variables = global_variables,
                               importServerInput = importServerInput,
                               modelServerInput = modelServerInput,
                               inferenceServerInput = inferenceServerInput)

  ############################################################
  #Stop the App when closing the browser or ending the session
  ############################################################
  session$onSessionEnded(stopApp)
}
