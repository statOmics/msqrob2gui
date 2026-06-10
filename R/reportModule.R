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
          list(#tags$label("Project name", `for`="project_name"),
               #tags$button("button_project_name", tags$sup("[?]")),
               textInput(NS(id,"project_name"), NULL, value = "project", width = '100%', placeholder = NULL),
               #hidden(
                 helpText(
                   id="tooltip_project_name",
                   "Give your project a meaningful name.
                            This name will be given to your results files.
                           A time stamp will be automatically appended to name.")
               #) #close hidden
          )
        ),
        tags$label("Number of significant features for which you want to have detail plots", `for`="maxPlot"),
        #tags$button(id="button_maxPlot", tags$sup("[?]")),
        numericInput(NS(id,"maxPlot"), label = NULL, value = 10, min = 1, max = NA, step = 1, width = '100%'),
        #hidden(
          helpText(id="tooltip_maxPlot","Number of significant features for which you want to have detail plots in the generated report
     	                  ")
        #) #close hidden
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
reportServer <- function(id="report", variables, importServerInput, modelServerInput, inferenceServerInput){
  moduleServer(
    id,
    function(input,output,session){
      #make input variables for the report
      selectedAssay <- metaReactive({..(inputServerInput$selectedAssay())}, varname = "selectedAssay")
      form <- metaReactive({..(modelServerInput$designFormula())}, varname = "form")
      doRidge <- metaReactive({..(modelServerInput$doRidge())}, varname = "doRidge")
      doRobust <- metaReactive({..(modelServerInput$doRobust())}, varname = "doRobust")
      contrast <- metaReactive({..(inferenceServerInput$contrast())}, , varname = "contrast")
      sigLevel <- metaReactive({..(inferenceServerInput$alpha())}, varname = "sigLevel")
      selectedLowLevelAssay <- metaReactive({..(inferenceServerInput$selectedLowLevelAssay())}, varname = "selectedLowLevelAssay")
      selHorPlot <- metaReactive({..(inferenceServerInput$selHorDetailPlot2())}, varname = "selHorPlot")
      selVertPlot <- metaReactive({..(inferenceServerInput$selVertDetailPlot2())}, varname ="selVertPlot")
      selColPlot <- metaReactive({..(inferenceServerInput$selColDetailPlot2())}, varname = "selColPlot")
      maxPlot <- metaReactive({..(input$maxPlot)}, varname = "maxPlot")

      output$report <- downloadHandler(filename = function() {
        paste0(
          input$project_name,"-report-", gsub(" |:","-",Sys.time()),".zip")
        },
        content = function(file) {
          peOut <- variables$qfeatures
          saveRDS(peOut,"qfeaturesFile.rds")
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
          report <- gsub(paste0(id,"_"),"\n",
                    expandChain(
                     invisible(maxPlot()),
                     invisible(selectedLowLevelAssay()),
                     invisible(selHorPlot()),
                     invisible(selVertPlot()),
                     invisible(selColPlot())
                     )
                   )
          buildRmdBundle(
            system.file("data/report.Rmd",package="msqrob2gui"),
            file,
            list(
              input = input,
              model = model,
              inference = inference,
              report = report
              ),
            render=FALSE,
            include_files = c("qfeaturesFile.rds")
            )
          })
    })
  }
