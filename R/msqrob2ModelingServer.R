#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny reactiveValues


# Define server
msqrob2ModelingServer <- function(input, output, session) {
  shinyjs::useShinyjs()

  ### servers
   ### input
  shinyjs::useShinyjs()

  global_variables <- reactiveValues(pe = NULL,
                                     selectedAssay = NULL,
                                     selectedLowLevelAssay = NULL) #Has to be done globally

  qcServerInput <- qcServer(variables = global_variables)
  
  modelServerInput <- modelServer(variables = global_variables)

  inferenceServerInput <- inferenceServer(variables = global_variables
                                          )

  reportServerInput <- reportServer(variables = global_variables,
                               importServerInput = NULL,
                               preprocessingServerInput = NULL,
                               qcServerInput =qcServerInput,
                               modelServerInput = modelServerInput,
                               inferenceServerInput = inferenceServerInput)

  ############################################################
  #Stop the App when closing the browser or ending the session
  ############################################################
  session$onSessionEnded(stopApp)
}
