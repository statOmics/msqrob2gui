#' Shiny app server function for the msqrob2 Explore app
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny reactiveValues

msqrob2ExploreServer <- function(input, output, session) {
  shinyjs::useShinyjs()

  global_variables <- reactiveValues(
    pe                    = NULL,
    qfeatures             = NULL,
    selectedAssay         = NULL,
    selectedLowLevelAssay = NULL,
    parameterNames        = NULL
  )

  qcServerInput        <- qcServer(variables = global_variables)
  modelServerInput     <- modelServer(variables = global_variables)
  inferenceServerInput <- inferenceServer(variables = global_variables)
  reportServerInput    <- reportServer(
    variables                = global_variables,
    importServerInput        = NULL,
    preprocessingServerInput = NULL,
    qcServerInput            = qcServerInput,
    modelServerInput         = modelServerInput,
    inferenceServerInput     = inferenceServerInput
  )
  altSEEExploreServer(variables = global_variables)

  session$onSessionEnded(stopApp)
}
