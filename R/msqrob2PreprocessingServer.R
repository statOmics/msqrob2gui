#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny reactiveValues


# Define server
msqrob2PreprocessingServer <- function(input, output, session) {
  shinyjs::useShinyjs()

  ### servers
   ### input
  shinyjs::useShinyjs()

  global_variables <- reactiveValues(pe = NULL,
                                     selectedAssay = NULL,
                                     selectedLowLevelAssay = NULL) #Has to be done globally

  importServerInput <- importServer(variables = global_variables)
  
  preprocessingServerInput <- preprocessingServer(variables = global_variables)
  
  qcServerInput <- qcServer(variables = global_variables)

  reportServerInput <- reportPreprocessingServer(variables = global_variables,
                                                 importServerInput = importServerInput,
                                                 preprocessingServerInput = preprocessingServerInput)

  ############################################################
  #Stop the App when closing the browser or ending the session
  ############################################################
  session$onSessionEnded(stopApp)
}
