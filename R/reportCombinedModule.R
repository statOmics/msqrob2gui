#' Combined report UI
#'
#' @return A shiny tagList object that contains the combined report UI components
#' @rdname INTERNAL_reportCombinedUI
#' @keywords internal
#'
reportCombinedUI <- function(id = "report") {
  fluidRow(
    column(width = 12,
      div(
        list(
          div(
            list(
              textInput(NS(id, "project_name"), NULL, value = "project", width = "100%"),
              helpText(
                id = "tooltip_project_name_combined",
                "Give your project a meaningful name.
                 This name will be given to your results files.
                 A time stamp will be automatically appended.")
            )
          )
        )
      ),
      downloadButton(NS(id, "report"), "Generate full report bundle", class = "btn-success")
    )
  )
}


#' Combined report server
#'
#' Generates a single zip bundle containing the raw input file, annotation
#' file, the final QFeatures RDS (after modelling), an inference TSV, and
#' both report-preprocessing.Rmd and report-inference.Rmd filled with the
#' options selected in the app.
#'
#' @param id module id
#' @param variables global reactive values
#' @param importServerInput reactives from importServer
#' @param preprocessingServerInput reactives from preprocessingServer
#' @param qcServerInput reactives from qcServer
#' @param modelServerInput reactives from modelServer
#' @param inferenceServerInput reactives from inferenceServer
#' @return nothing
#' @rdname INTERNAL_reportCombinedServer
#' @keywords internal
#' @importFrom shinymeta metaReactive expandChain buildRmdBundle
#' @importFrom msqrob2 makeContrast msqrobCollect
#' @importFrom knitr knit_expand
#'
reportCombinedServer <- function(id = "report", variables,
                                  importServerInput, preprocessingServerInput,
                                  qcServerInput, modelServerInput,
                                  inferenceServerInput) {
  moduleServer(id, function(input, output, session) {

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
    minObs             <- metaReactive({..(preprocessingServerInput$minObs())},             varname = "minObs")
    nameFilterNAAssay  <- metaReactive({..(preprocessingServerInput$nameFilterNAAssay())},  varname = "nameFilterNAAssay")
    nameLogAssay       <- metaReactive({..(preprocessingServerInput$nameLogAssay())},       varname = "nameLogAssay")
    normMethod         <- metaReactive({..(preprocessingServerInput$normMethod())},         varname = "normMethod")
    nameNormAssay      <- metaReactive({..(preprocessingServerInput$nameNormAssay())},      varname = "nameNormAssay")
    aggrMethod         <- metaReactive({..(preprocessingServerInput$aggrMethod())},         varname = "aggrMethod")
    aggrCol            <- metaReactive({..(preprocessingServerInput$aggrCol())},            varname = "aggrCol")
    nameAggrAssay      <- metaReactive({..(preprocessingServerInput$nameAggrAssay())},      varname = "nameAggrAssay")
    nprecFilter        <- metaReactive({..(preprocessingServerInput$nprecFilter())},        varname = "nprecFilter")
    minObs2            <- metaReactive({..(preprocessingServerInput$minObs2())},            varname = "minObs2")
    nameFilterNA2Assay <- metaReactive({..(preprocessingServerInput$nameFilterNA2Assay())}, varname = "nameFilterNA2Assay")

    # ---- Modelling metaReactives ----
    selectedSet <- metaReactive({..(qcServerInput$selectedAssay())}, varname = "selectedSet")
    form        <- metaReactive({..(modelServerInput$designFormula())}, varname = "form")
    doRidge     <- metaReactive({..(modelServerInput$doRidge())},    varname = "doRidge")
    doRobust    <- metaReactive({..(modelServerInput$doRobust())},   varname = "doRobust")
    contrast    <- metaReactive({..(inferenceServerInput$contrast())}, varname = "contrast")
    sigLevel    <- metaReactive({..(inferenceServerInput$alpha())},  varname = "sigLevel")

    output$report <- downloadHandler(
      filename = function() {
        paste0(input$project_name, "-report-", gsub(" |:", "-", Sys.time()), ".zip")
      },
      content = function(file) {

        # 1. Save final QFeatures RDS
        saveRDS(variables$qfeatures, "qfeaturesFile.rds")
        include_files <- "qfeaturesFile.rds"
        on.exit(unlink(include_files[file.exists(include_files)]), add = TRUE)

        # 2. Copy raw input and annotation files
        if (!is.null(variables$rawFilePath) && file.exists(variables$rawFilePath)) {
          file.copy(variables$rawFilePath, variables$rawFileName, overwrite = TRUE)
          include_files <- c(include_files, variables$rawFileName)
        }
        if (!is.null(variables$annotFilePath) && file.exists(variables$annotFilePath)) {
          file.copy(variables$annotFilePath, variables$annotFileName, overwrite = TRUE)
          include_files <- c(include_files, variables$annotFileName)
        }

        # 3. Generate inference.tsv
        if (length(contrast()) > 0 && !is.null(variables$parameterNames)) {
          L <- try(
            makeContrast(contrast(), parameterNames = variables$parameterNames),
            silent = TRUE
          )
          if (!inherits(L, "try-error")) {
            inferenceDf <- try(
              msqrobCollect(variables$qfeatures[[selectedSet()]], L),
              silent = TRUE
            )
            if (!inherits(inferenceDf, "try-error")) {
              write.table(inferenceDf, "inference.tsv",
                          sep = "\t", row.names = FALSE, quote = FALSE)
              include_files <- c(include_files, "inference.tsv")
            }
          }
        }

        # 4. Build import code string
        importCode <- gsub(paste0(id, "_"), "\n",
          expandChain(
            if (!is.null(importServerInput$software()))  invisible(software()),
            if (!is.null(importServerInput$fnames()))    invisible(fnames()),
            if (!is.null(importServerInput$name()))      invisible(assayName()),
            if (!is.null(importServerInput$runCol()))    invisible(runCol()),
            if (!is.null(importServerInput$quantCol()))  invisible(quantCol()),
            if (!is.null(importServerInput$quantCols())) invisible(quantCols())
          ))

        # 5. Build preprocessing code string
        preprocessingCode <- gsub(paste0(id, "_"), "\n",
          expandChain(
            if (!is.null(preprocessingServerInput$filterList()))         invisible(filterList()),
            if (!is.null(preprocessingServerInput$doZeroToNA()))         invisible(doZeroToNA()),
            if (!is.null(preprocessingServerInput$doLog()))              invisible(doLog()),
            if (!is.null(preprocessingServerInput$fCol()))               invisible(fCol()),
            if (!is.null(preprocessingServerInput$nameAssay()))          invisible(nameAssay()),
            if (!is.null(preprocessingServerInput$minObs()))             invisible(minObs()),
            if (!is.null(preprocessingServerInput$nameFilterNAAssay()))  invisible(nameFilterNAAssay()),
            if (!is.null(preprocessingServerInput$nameLogAssay()))       invisible(nameLogAssay()),
            if (!is.null(preprocessingServerInput$normMethod()))         invisible(normMethod()),
            if (!is.null(preprocessingServerInput$nameNormAssay()))      invisible(nameNormAssay()),
            if (!is.null(preprocessingServerInput$aggrMethod()))         invisible(aggrMethod()),
            if (!is.null(preprocessingServerInput$aggrCol()))            invisible(aggrCol()),
            if (!is.null(preprocessingServerInput$nameAggrAssay()))      invisible(nameAggrAssay()),
            if (!is.null(preprocessingServerInput$nprecFilter()))        invisible(nprecFilter()),
            if (!is.null(preprocessingServerInput$minObs2()))            invisible(minObs2()),
            if (!is.null(preprocessingServerInput$nameFilterNA2Assay())) invisible(nameFilterNA2Assay())
          ))

        # 6. Expand and write the preprocessing RMD (added as an extra include file)
        prepExpanded <- do.call(
          knitr::knit_expand,
          c(
            list(file = system.file("data/report-dataprocessing.Rmd", package = "msqrob2gui")),
            list(
              importCode    = importCode,
              preprocessing = preprocessingCode,
              rawFileName   = deparse(variables$rawFileName),
              annotFileName = deparse(variables$annotFileName)
            )
          )
        )
        writeLines(prepExpanded, "report-dataprocessing.Rmd")
        include_files <- c(include_files, "report-dataprocessing.Rmd")

        # 7. Build modelling code strings; use buildRmdBundle for inference RMD
        #    (it also creates the final zip containing all include_files)
        qfFile <- expandChain(
          quote({ qfeaturesFile <- "qfeaturesFile.rds" })
        )
        model <- gsub(paste0(id, "_"), "\n",
          expandChain(
            invisible(selectedSet()),
            invisible(form()),
            invisible(doRidge()),
            invisible(doRobust())
          ))
        inference <- gsub(paste0(id, "_"), "\n",
          expandChain(
            invisible(contrast()),
            invisible(sigLevel())
          ))

        buildRmdBundle(
          system.file("data/report-inference.Rmd", package = "msqrob2gui"),
          file,
          list(qfFile = qfFile, model = model, inference = inference),
          render        = FALSE,
          include_files = include_files
        )
      }
    )
  })
}
