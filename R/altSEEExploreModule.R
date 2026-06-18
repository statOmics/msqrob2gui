#' altSEE explore tab UI
#' @keywords internal
#' @importFrom shiny NS fluidRow column div tags h4 br uiOutput helpText actionButton icon selectInput selectizeInput renderUI tagList

altSEEExploreUI <- function(id = "explore") {
  ns <- NS(id)
  fluidRow(
    column(
      width = 3,
      h4("iSEE configuration"),
      div(
        tags$label("Main experiment"),
        uiOutput(ns("selectAssay")),
        helpText("Defaults to the assay selected in the QC tab.")
      ),
      div(
        tags$label("Alt experiment"),
        uiOutput(ns("selectAltExp")),
        helpText("Assay shown in the alt experiment panels.")
      ),
      div(
        tags$label("colData variable for X axis"),
        uiOutput(ns("selectVariable")),
        helpText("Optional — used on the X axis of feature-level intensity plots.")
      ),
      div(
        tags$label("rowData column for feature linking"),
        uiOutput(ns("selectMappingKey")),
        helpText("Optional — LookupColumn / MapColumn in LinkedFeaturesAssayPlot.")
      ),
      br(),
      actionButton(ns("launchISEE"), "Launch iSEE", class = "btn-primary",
                   icon = icon("rocket"), width = "100%"),
      br(), br(),
      uiOutput(ns("iseeStatus"))
    ),
    column(
      width = 9,
      h4("QFeatures overview"),
      DT::dataTableOutput(ns("qfTable")),
      br(),
      h4("Selected assay preview"),
      DT::dataTableOutput(ns("assayPreview"))
    )
  )
}


#' altSEE explore tab server
#' @keywords internal
#' @importFrom shiny moduleServer observeEvent renderUI req showNotification selectInput
#'   selectizeInput reactiveValues observe reactiveTimer tagList tags
#' @importFrom SummarizedExperiment colData rowData assay
#' @importFrom DT datatable renderDT
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom shinyjs runjs

altSEEExploreServer <- function(id = "explore", variables) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(isee_proc = NULL, isee_port = NULL, isee_url = NULL)

    output$selectAssay <- renderUI({
      req(variables$qfeatures)
      selectInput(session$ns("selectedAssay"), label = NULL,
                  choices  = names(variables$qfeatures),
                  selected = variables$selectedAssay)
    })

    output$selectAltExp <- renderUI({
      req(variables$qfeatures, input$selectedAssay)
      choices <- setdiff(names(variables$qfeatures), input$selectedAssay)
      selectInput(session$ns("selectedAltExp"), label = NULL, choices = choices)
    })

    output$selectVariable <- renderUI({
      req(variables$qfeatures)
      selectizeInput(session$ns("selectedVariable"), label = NULL,
                     choices  = c("-- none --" = "", names(colData(variables$qfeatures))),
                     selected = "",
                     options  = list(allowEmptyOption = TRUE))
    })

    output$selectMappingKey <- renderUI({
      req(variables$qfeatures, input$selectedAssay)
      selectizeInput(session$ns("selectedMappingKey"), label = NULL,
                     choices  = c("-- none --" = "",
                                  names(rowData(variables$qfeatures[[input$selectedAssay]]))),
                     selected = "",
                     options  = list(allowEmptyOption = TRUE))
    })

    output$qfTable <- DT::renderDT({
      req(variables$qfeatures)
      DT::datatable(
        as.data.frame(colData(variables$qfeatures)),
        options   = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
        selection = "none"
      )
    })

    output$assayPreview <- DT::renderDT({
      req(variables$qfeatures, input$selectedAssay)
      DT::datatable(
        as.data.frame(assay(variables$qfeatures[[input$selectedAssay]])),
        options   = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
        selection = "none"
      )
    })

    observeEvent(input$launchISEE, {
      req(variables$qfeatures, input$selectedAssay)

      required     <- c("scater", "iSEE", "iSEEu", "altSEE", "callr")
      missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing_pkgs) > 0) {
        showNotification(
          paste0("Missing packages: ", paste(missing_pkgs, collapse = ", "),
                 ". Install them and try again."),
          type = "error", duration = NULL
        )
        return()
      }

      shinybusy::show_modal_spinner(spin = "cube-grid", color = "#112446",
                                    text = "Building SingleCellExperiment...")
      sce <- try(.qf_to_sce(variables$qfeatures, input$selectedAssay), silent = TRUE)
      shinybusy::remove_modal_spinner()

      if (inherits(sce, "try-error")) {
        showNotification(
          paste("Failed to build SCE:", conditionMessage(attr(sce, "condition"))),
          type = "error"
        )
        return()
      }

      xAxisVar   <- if (!is.null(input$selectedVariable)   && nzchar(input$selectedVariable))   input$selectedVariable   else NULL
      mappingKey <- if (!is.null(input$selectedMappingKey) && nzchar(input$selectedMappingKey)) input$selectedMappingKey else NULL
      panels     <- .altSEE_panels(input$selectedAssay, input$selectedAltExp, xAxisVar, mappingKey)

      tmp  <- tempfile(fileext = ".rds")
      saveRDS(list(sce = sce, panels = panels), tmp)
      port <- httpuv::randomPort()

      # Kill any existing iSEE background process before launching a new one
      if (!is.null(rv$isee_proc)) {
        try(rv$isee_proc$kill(), silent = TRUE)
        rv$isee_proc <- NULL
      }

      proc <- callr::r_bg(
        function(tmp, port) {
          dat <- readRDS(tmp)
          suppressPackageStartupMessages({
            library(iSEE); library(iSEEu); library(altSEE)
          })
          app <- iSEE::iSEE(dat$sce, initial = dat$panels)
          shiny::runApp(app, port = port, host = "127.0.0.1", launch.browser = FALSE)
        },
        args      = list(tmp = tmp, port = port),
        supervise = FALSE
      )

      rv$isee_proc <- proc
      rv$isee_port <- port
      rv$isee_url  <- sprintf("http://127.0.0.1:%d", port)

      showNotification(
        "iSEE is starting — it will open in a new browser tab shortly.",
        type = "message", duration = 6
      )
    })

    # Poll every 600 ms until iSEE's port accepts connections, then open a new browser tab.
    isee_timer <- reactiveTimer(600)
    observe({
      req(rv$isee_port)
      isee_timer()
      port <- rv$isee_port
      ok <- tryCatch({
        con <- socketConnection("127.0.0.1", port = port,
                                blocking = TRUE, timeout = 0.5, open = "rb")
        close(con)
        TRUE
      }, error = function(e) FALSE)
      if (ok) {
        url          <- rv$isee_url
        rv$isee_port <- NULL   # stop the polling loop
        shinyjs::runjs(sprintf('window.open("%s", "_blank")', url))
      }
    })

    output$iseeStatus <- renderUI({
      if (!is.null(rv$isee_url)) {
        tagList(
          helpText("iSEE is running. If it did not open automatically:"),
          tags$a(href = rv$isee_url, target = "_blank", rv$isee_url)
        )
      }
    })
  })
}


# Build a SingleCellExperiment from a QFeatures object.
# The selected assay becomes the main experiment; all other assays are altExps.
#' @importFrom MultiAssayExperiment getWithColData
#' @importFrom methods as
#' @importFrom SingleCellExperiment altExp
.qf_to_sce <- function(qf, selectedSet) {
  sce <- MultiAssayExperiment::getWithColData(qf, i = selectedSet) |>
    methods::as("SingleCellExperiment") |>
    scater::runMDS(exprs_values = 1)
  SummarizedExperiment::rowData(sce) <- SummarizedExperiment::rowData(sce) |>
    as.data.frame() |>
    dplyr::select(where(is.atomic))

  for (i in setdiff(names(qf), selectedSet)) {
    altsce <- qf[[i]] |>
      methods::as("SingleCellExperiment") |>
      scater::runMDS(exprs_values = 1)
    SummarizedExperiment::rowData(altsce) <- SummarizedExperiment::rowData(altsce) |>
      as.data.frame() |>
      dplyr::select(where(is.atomic))
    SingleCellExperiment::altExp(sce, i) <- altsce
  }
  sce
}


# Build the initial iSEE panel layout.
# altExp is the alt experiment name; xAxisVar and mappingKey may be NULL.
.altSEE_panels <- function(selectedSet, altExp, xAxisVar, mappingKey) {
  lfap_base <- list(
    YAxisFeatureSource = "RowDataTable1",
    PlotType = if (!is.null(xAxisVar)) "Scatter + lines" else "Auto",
    PanelWidth = 6L
  )
  if (!is.null(xAxisVar)) {
    lfap_base$XAxis <- "Column data"
    lfap_base$XAxisColumnData <- xAxisVar
  }
  if (!is.null(mappingKey)) {
    lfap_base$LookupColumn <- mappingKey
    lfap_base$MapColumn    <- mappingKey
  }

  lfap_main <- do.call(
    altSEE::LinkedFeaturesAssayPlot,
    c(list(SelectionExperiment = "(Main)", Experiment = "(Main)"), lfap_base)
  )
  lfap_alt <- do.call(
    altSEE::LinkedFeaturesAssayPlot,
    c(list(SelectionExperiment = "(Main)", Experiment = altExp), lfap_base)
  )

  list(
    iSEE::ReducedDimensionPlot(PanelWidth = 6L),
    altSEE::AltReducedDimensionPlot(PanelWidth = 6L, Experiment = altExp),
    altSEE::AltVolcanoPlot(PanelWidth = 6L, Experiment = "(Main)"),
    altSEE::AltVolcanoPlot(PanelWidth = 6L, Experiment = altExp),
    iSEE::RowDataTable(RowSelectionSource = "AltVolcanoPlot1", PanelWidth = 6L),
    altSEE::AltRowDataTable(RowSelectionSource = "AltVolcanoPlot2", PanelWidth = 6L,
                            Experiment = altExp),
    lfap_main,
    lfap_alt
  )
}
