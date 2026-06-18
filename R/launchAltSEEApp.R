#' Launch the altSEE interactive visualisation app
#'
#' Opens a Shiny interface to upload a QFeatures RDS file and select an assay,
#' then builds a SingleCellExperiment and launches an iSEE app for interactive
#' exploration via altSEE.
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @return Invisibly returns the SingleCellExperiment built from the selected
#'   QFeatures assay.
#'
#' @export launchAltSEEApp
#'
#' @examples
#' \dontrun{launchAltSEEApp()}
#'
launchAltSEEApp <- function(maxSize = 500) {
  required <- c("scater", "iSEE", "iSEEu", "altSEE")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed: ",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Install with:\n",
      "  BiocManager::install(c('scater', 'iSEE', 'iSEEu'))\n",
      "  remotes::install_github('statomics/altSEE')"
    )
  }

  # Attach S4 packages so their class definitions are registered on the search
  # path before iSEE() tries to use them. requireNamespace() alone loads the
  # namespace but does not attach it, which leaves S4 class validators
  # (.SigLength etc.) unresolved when the packages have not been library()-ed
  # beforehand.
  suppressPackageStartupMessages({
    library(scater)
    library(iSEE)
    library(iSEEu)
    library(altSEE)
  })

  options(shiny.maxRequestSize = maxSize * 1024^2)

  result <- new.env(parent = emptyenv())
  result$sce        <- NULL
  result$selectedSet <- NULL
  result$altExp      <- NULL
  result$xAxisVar   <- NULL
  result$mappingKey  <- NULL

  ui <- fluidPage(
    titlePanel("Launch altSEE interactive visualisation"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          tags$label("QFeatures input RDS file"),
          fileInput("pe", label = NULL, multiple = FALSE),
          helpText("Upload the RDS file containing your QFeatures object.")
        ),
        div(
          tags$label("Select assay"),
          uiOutput("selectAssay"),
          helpText("Select the QFeatures assay to use as the main experiment in iSEE.")
        ),
        div(
          tags$label("Select alt experiment"),
          uiOutput("selectAltExp"),
          helpText("Select the assay to use as the alt experiment in iSEE. Only assays other than the main experiment are available.")
        ),
        div(
          tags$label("Select colData variable for X axis"),
          uiOutput("selectVariable"),
          helpText("This variable is used on the X axis of feature-level intensity plots.")
        ),
        div(
          tags$label("Select rowData column for feature linking"),
          uiOutput("selectMappingKey"),
          helpText("Column from the main assay's rowData used as LookupColumn and MapColumn in LinkedFeaturesAssayPlot to link features across experiments.")
        ),
        br(),
        actionButton("launchISEE", "Launch iSEE", class = "btn-primary",
                     icon = icon("rocket"), width = "100%")
      ),
      mainPanel(
        width = 9,
        h4("QFeatures sample overview"),
        DT::dataTableOutput("qfeaturesTable"),
        br(),
        h4("Selected assay preview"),
        DT::dataTableOutput("assayTable")
      )
    )
  )

  server <- function(input, output, session) {
    qf <- reactiveVal(NULL)

    observeEvent(input$pe, {
      req(input$pe$datapath)
      obj <- try(readRDS(input$pe$datapath), silent = TRUE)
      if (inherits(obj, "try-error") || !is(obj, "QFeatures")) {
        showNotification("Please upload a valid QFeatures RDS file.",
                         type = "error", duration = NULL)
        return()
      }
      qf(obj)
    })

    output$selectAssay <- renderUI({
      req(qf())
      selectInput("selectedAssay", label = NULL, choices = names(qf()))
    })

    output$selectAltExp <- renderUI({
      req(qf(), input$selectedAssay)
      altChoices <- setdiff(names(qf()), input$selectedAssay)
      selectInput("selectedAltExp", label = NULL, choices = altChoices)
    })

    output$selectVariable <- renderUI({
      req(qf())
      selectizeInput("selectedVariable", label = NULL,
                     choices  = c("-- none --" = "", names(SummarizedExperiment::colData(qf()))),
                     selected = "",
                     options  = list(allowEmptyOption = TRUE))
    })

    output$selectMappingKey <- renderUI({
      req(qf(), input$selectedAssay)
      selectizeInput("selectedMappingKey", label = NULL,
                     choices  = c("-- none --" = "", names(SummarizedExperiment::rowData(qf()[[input$selectedAssay]]))),
                     selected = "",
                     options  = list(allowEmptyOption = TRUE))
    })

    output$qfeaturesTable <- DT::renderDT({
      req(qf())
      DT::datatable(
        as.data.frame(SummarizedExperiment::colData(qf())),
        options = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
        selection = "none"
      )
    })

    output$assayTable <- DT::renderDT({
      req(qf(), input$selectedAssay)
      DT::datatable(
        as.data.frame(SummarizedExperiment::assay(qf()[[input$selectedAssay]])),
        options = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
        selection = "none"
      )
    })

    observeEvent(input$launchISEE, {
      req(qf(), input$selectedAssay)
      shinybusy::show_modal_spinner(spin = "cube-grid", color = "#112446",
                                    text = "Building SingleCellExperiment...")
      sce <- try(.qf_to_sce(qf(), input$selectedAssay), silent = TRUE)
      shinybusy::remove_modal_spinner()
      if (inherits(sce, "try-error")) {
        showNotification(
          paste("Failed to build SCE:", conditionMessage(attr(sce, "condition"))),
          type = "error"
        )
        return()
      }
      result$sce        <- sce
      result$selectedSet <- input$selectedAssay
      result$altExp      <- input$selectedAltExp
      result$xAxisVar  <- if (!is.null(input$selectedVariable) && nzchar(input$selectedVariable))
        input$selectedVariable else NULL
      result$mappingKey <- if (!is.null(input$selectedMappingKey) && nzchar(input$selectedMappingKey))
        input$selectedMappingKey else NULL
      shiny::stopApp()
    })
  }

  shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)

  if (!is.null(result$sce)) {
    initial_panels <- .altSEE_panels(result$selectedSet, result$altExp, result$xAxisVar, result$mappingKey)
    app <- iSEE::iSEE(result$sce, initial = initial_panels)
    shiny::runApp(app, launch.browser = TRUE)
    invisible(result$sce)
  }
}


# Build a SingleCellExperiment from a QFeatures object.
# The selected assay becomes the main experiment; all other assays are altExps.
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


# Build the initial iSEE panel layout matching the runAltSEE.R script.
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
    altSEE::AltRowDataTable(RowSelectionSource = "AltVolcanoPlot1", PanelWidth = 6L,
                            Experiment = altExp),
    lfap_main,
    lfap_alt
  )
}
