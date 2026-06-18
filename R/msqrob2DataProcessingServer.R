#' Shiny app server function for the data processing app
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny reactiveValues

# Define server
msqrob2DataProcessingServer <- function(input, output, session) {
  shinyjs::useShinyjs()

  global_variables <- reactiveValues(
    pe                    = NULL,
    qfeatures             = NULL,
    selectedAssay         = NULL,
    selectedLowLevelAssay = NULL,
    rawFilePath           = NULL,
    rawFileName           = NULL,
    annotFilePath         = NULL,
    annotFileName         = NULL
  )

  importServerInput <- importServer(variables = global_variables)

  preprocessingServerInput <- preprocessingServer(variables = global_variables)

  qcServerInput <- qcServer(variables = global_variables)

  reportDataProcessingServer(
    variables                = global_variables,
    importServerInput        = importServerInput,
    preprocessingServerInput = preprocessingServerInput
  )

  session$onSessionEnded(stopApp)
}
