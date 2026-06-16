#' report UI
#'
#' @return A shiny tagList object that contains the report UI components
#' @rdname INTERNAL_reportUI
#' @keywords internal
#'
reportUI <- function(id="report")
{
  fluidRow(
    column(width=12,
    div(
      list(
        div(
          list(
               textInput(NS(id,"project_name"), NULL, value = "project", width = '100%', placeholder = NULL),
               #hidden(
                 helpText(
                   id="tooltip_project_name",
                   "Give your project a meaningful name.
                            This name will be given to your results files.
                           A time stamp will be automatically appended to name.")
               #) #close hidden
          )
        )
      )),
    downloadButton(NS(id,"report"), "Generate report")
  ) # end column
) # end fluidRow
}


#' Server for report tab
#'
#' @param id module id
#' @param variables global reactive values object to share objects across modules
#' @param inputServerInput list with reactive values containing the input of the inputServer module
#' @param modelServerInput list with reactive values containing the input of the modelServer module
#' @param inferenceServerInput list with reactive values containing the input of the inferenceServer module
#' @return list of reactive inputs
#' @rdname INTERNAL_inferenceServer
#' @keywords internal
#'
reportServer <- function(id="report", variables, importServerInput,preprocessingServerInput,qcServerInput, modelServerInput, inferenceServerInput){
  moduleServer(
    id,
    function(input,output,session){
      #make input variables for the report
      selectedAssay <- metaReactive({..(qcServerInput$selectedAssay())}, varname = "selectedAssay")
      form <- metaReactive({..(modelServerInput$designFormula())}, varname = "form")
      doRidge <- metaReactive({..(modelServerInput$doRidge())}, varname = "doRidge")
      doRobust <- metaReactive({..(modelServerInput$doRobust())}, varname = "doRobust")
      contrast <- metaReactive({..(inferenceServerInput$contrast())}, , varname = "contrast")
      sigLevel <- metaReactive({..(inferenceServerInput$alpha())}, varname = "sigLevel")

      output$report <- downloadHandler(filename = function() {
        paste0(
          input$project_name,"-report-inference-", gsub(" |:","-",Sys.time()),".zip")
        },
        content = function(file) {
          peOut <- variables$qfeatures
          saveRDS(peOut,"qfeaturesFile.rds")

          # Copy raw input and annotation files so they are bundled in the zip
          include_files <- "qfeaturesFile.rds"

          if (!is.null(variables$rawFilePath) && file.exists(variables$rawFilePath)) {
            file.copy(variables$rawFilePath, variables$rawFileName, overwrite = TRUE)
            include_files <- c(include_files, variables$rawFileName)
          }

          if (!is.null(variables$annotFilePath) && file.exists(variables$annotFilePath)) {
            file.copy(variables$annotFilePath, variables$annotFileName, overwrite = TRUE)
            include_files <- c(include_files, variables$annotFileName)
          }

          input <- expandChain(
            quote({
              qfeaturesFile <- "qfeaturesFile.rds"
              })
            )
          model <- gsub(paste0(id,"_"),"\n",
            expandChain(
            invisible(selectedAssay()),
            invisible(form()),
            invisible(doRidge()),
            invisible(doRobust())
            )
          )
          inference <- gsub(paste0(id,"_"),"\n",
            expandChain(
            invisible(contrast()),
            invisible(sigLevel())
            )
          )
          buildRmdBundle(
            system.file("data/report-inference.Rmd",package="msqrob2gui"),
            file,
            list(
              input = input,
              model = model,
              inference = inference
              ),
            render=FALSE,
            include_files = include_files
            )
          })
    })
  }
