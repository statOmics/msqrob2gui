#' Data processing report UI
#'
#' @return A shiny tagList object that contains the data processing report UI components
#' @rdname INTERNAL_reportDataProcessingUI
#' @keywords internal
#'
reportDataProcessingUI <- function(id = "reportDataProcessing")
{
  fluidRow(
    column(width = 12,
      div(
        list(
          div(
            list(
              textInput(NS(id, "project_name"), NULL, value = "project", width = '100%', placeholder = NULL),
              helpText(
                id = "tooltip_project_name_dataprocessing",
                "Give your project a meaningful name.
                         This name will be given to your results files.
                         A time stamp will be automatically appended to name.")
            )
          )
        )
      ),
      downloadButton(NS(id, "report"), "Generate data processing report")
    )
  )
}


#' Server for data processing report
#'
#' @param id module id
#' @param variables global reactive values object to share objects across modules
#' @param importServerInput list with reactive values from importServer
#' @param preprocessingServerInput list with reactive values from preprocessingServer
#' @return nothing
#' @rdname INTERNAL_reportDataProcessingServer
#' @keywords internal
#'
reportDataProcessingServer <- function(id = "reportDataProcessing", variables, importServerInput, preprocessingServerInput) {
  moduleServer(
    id,
    function(input, output, session) {

      # ---- Import metaReactives ----
      software  <- metaReactive({..(importServerInput$software())},  varname = "software")
      fnames    <- metaReactive({..(importServerInput$fnames())},    varname = "fnames")
      runCol    <- metaReactive({..(importServerInput$runCol())},    varname = "runCol")
      quantCol  <- metaReactive({..(importServerInput$quantCol())},  varname = "quantCol")
      quantCols <- metaReactive({..(importServerInput$quantCols())}, varname = "quantCols")
      assayName <- metaReactive({..(importServerInput$name())},      varname = "assayName")

      # ---- Preprocessing metaReactives ----
      filterList         <- metaReactive({..(preprocessingServerInput$filterList())},         varname = "filterList")
      doZeroToNA         <- metaReactive({..(preprocessingServerInput$doZeroToNA())},         varname = "doZeroToNA")
      doLog              <- metaReactive({..(preprocessingServerInput$doLog())},              varname = "doLog")
      fCol               <- metaReactive({..(preprocessingServerInput$fCol())},               varname = "fCol")
      nameAssay          <- metaReactive({..(preprocessingServerInput$nameAssay())},          varname = "nameAssay")
      threshold          <- metaReactive({..(preprocessingServerInput$threshold())},          varname = "threshold")
      nameFilterNAAssay  <- metaReactive({..(preprocessingServerInput$nameFilterNAAssay())},  varname = "nameFilterNAAssay")
      nameLogAssay       <- metaReactive({..(preprocessingServerInput$nameLogAssay())},       varname = "nameLogAssay")
      normMethod         <- metaReactive({..(preprocessingServerInput$normMethod())},         varname = "normMethod")
      nameNormAssay      <- metaReactive({..(preprocessingServerInput$nameNormAssay())},      varname = "nameNormAssay")
      aggrMethod         <- metaReactive({..(preprocessingServerInput$aggrMethod())},         varname = "aggrMethod")
      aggrCol            <- metaReactive({..(preprocessingServerInput$aggrCol())},            varname = "aggrCol")
      nameAggrAssay      <- metaReactive({..(preprocessingServerInput$nameAggrAssay())},      varname = "nameAggrAssay")
      nprecFilter        <- metaReactive({..(preprocessingServerInput$nprecFilter())},        varname = "nprecFilter")
      threshold2         <- metaReactive({..(preprocessingServerInput$threshold2())},         varname = "threshold2")
      nameFilterNA2Assay <- metaReactive({..(preprocessingServerInput$nameFilterNA2Assay())}, varname = "nameFilterNA2Assay")

      output$report <- downloadHandler(
        filename = function() {
          paste0(input$project_name, "-report-dataprocessing-", gsub(" |:", "-", Sys.time()), ".zip")
        },
        content = function(file) {

          peOut <- variables$qfeatures
          saveRDS(peOut, "qfeaturesFile.rds")
          include_files <- "qfeaturesFile.rds"
          on.exit(unlink(include_files[file.exists(include_files)]), add = TRUE)

          if (!is.null(variables$rawFilePath) && file.exists(variables$rawFilePath)) {
            file.copy(variables$rawFilePath, variables$rawFileName, overwrite = TRUE)
            include_files <- c(include_files, variables$rawFileName)
          }
          if (!is.null(variables$annotFilePath) && file.exists(variables$annotFilePath)) {
            file.copy(variables$annotFilePath, variables$annotFileName, overwrite = TRUE)
            include_files <- c(include_files, variables$annotFileName)
          }

          # ---- Build import code string ----
          importCode <- gsub(paste0(id, "_"), "\n",
            expandChain(
              if (!is.null(importServerInput$software()))  invisible(software()),
              if (!is.null(importServerInput$fnames()))    invisible(fnames()),
              if (!is.null(importServerInput$name()))      invisible(assayName()),
              if (!is.null(importServerInput$runCol()))    invisible(runCol()),
              if (!is.null(importServerInput$quantCol()))  invisible(quantCol()),
              if (!is.null(importServerInput$quantCols())) invisible(quantCols())
            ))

          # ---- Build preprocessing code string ----
          preprocessingCode <- gsub(paste0(id, "_"), "\n",
            expandChain(
              if (!is.null(preprocessingServerInput$filterList()))         invisible(filterList()),
              if (!is.null(preprocessingServerInput$doZeroToNA()))         invisible(doZeroToNA()),
              if (!is.null(preprocessingServerInput$doLog()))              invisible(doLog()),
              if (!is.null(preprocessingServerInput$fCol()))               invisible(fCol()),
              if (!is.null(preprocessingServerInput$nameAssay()))          invisible(nameAssay()),
              if (!is.null(preprocessingServerInput$threshold()))          invisible(threshold()),
              if (!is.null(preprocessingServerInput$nameFilterNAAssay()))  invisible(nameFilterNAAssay()),
              if (!is.null(preprocessingServerInput$nameLogAssay()))       invisible(nameLogAssay()),
              if (!is.null(preprocessingServerInput$normMethod()))         invisible(normMethod()),
              if (!is.null(preprocessingServerInput$nameNormAssay()))      invisible(nameNormAssay()),
              if (!is.null(preprocessingServerInput$aggrMethod()))         invisible(aggrMethod()),
              if (!is.null(preprocessingServerInput$aggrCol()))            invisible(aggrCol()),
              if (!is.null(preprocessingServerInput$nameAggrAssay()))      invisible(nameAggrAssay()),
              if (!is.null(preprocessingServerInput$nprecFilter()))        invisible(nprecFilter()),
              if (!is.null(preprocessingServerInput$threshold2()))         invisible(threshold2()),
              if (!is.null(preprocessingServerInput$nameFilterNA2Assay())) invisible(nameFilterNA2Assay())
            ))

          buildRmdBundle(
            system.file("data/report-dataprocessing.Rmd", package = "msqrob2gui"),
            file,
            list(importCode = importCode, preprocessing = preprocessingCode,
                 rawFileName = deparse(variables$rawFileName),
                 annotFileName = deparse(variables$annotFileName)),
            render = FALSE,
            include_files = include_files
          )
        }
      )
    }
  )
}
