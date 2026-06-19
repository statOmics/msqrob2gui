#' preprocessing UI
#'
#' @return A shiny tagList object that contains the preprocessing UI components
#' @rdname INTERNAL_preprocessingUI
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput helpText tags div
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip

preprocessingUI <- function(id = "preprocessing") {
  fluidRow(
    column(width = 12,

      # --- Restore ---
      div(style = "margin-bottom: 15px;",
        tags$label("Restore"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(4, actionButton(NS(id, "restore_qf"), "Restore from import"))
        ),
        helpText("Resets the QFeatures object to the state at end of import.")
      ),

      # --- Zero to NA ---
      div(style = "margin-bottom: 15px;",
        tags$label("Convert zeros to NA"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, checkboxInput(NS(id, "doZeroToNA"), "Apply zero to NA", value = TRUE)),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_zero_to_na"), "Test", class = "btn-primary", style = "display: block;"))
        ),
        helpText("Converts all zero values to NA.")
      ),

      # --- Filters ---
      uiOutput(NS(id, "filtering")),

      # --- Join assays (only rendered when > 1 assay) ---
      uiOutput(NS(id, "join")),

      # --- Filter NA ---
      div(style = "margin-bottom: 15px;",
        tags$label("Filter missing values"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(4, sliderInput(NS(id, "threshold"), "Threshold", value = 1, min = 0, max = 1, step = 0.01)),
          column(3, textInput(NS(id, "nameFilterNAAssay"), "Name", value = "quants_filter_na")),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_filter_na"), "Test", class = "btn-primary", style = "display: block;"))
        ),
        helpText("Removes features with proportion of missing values above threshold."),
        uiOutput(NS(id, "missingValUI"))
      ),

      # --- Log transform ---
      div(style = "margin-bottom: 15px;",
        tags$label("Log2-transform"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, checkboxInput(NS(id, "doLog"), "Apply log2 transform", value = TRUE)),
          column(3, textInput(NS(id, "nameLogAssay"), "Name", value = "quants_log")),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_log"), "Test", class = "btn-primary", style = "display: block;"))
        )
      ),

      # --- Normalisation ---
      uiOutput(NS(id, "normalisation")),

      # --- Aggregation ---
      uiOutput(NS(id, "aggregation")),

      # --- Filter NA post-aggregation ---
      div(style = "margin-bottom: 15px;",
        tags$label("Filter missing values (post-aggregation)"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(4, sliderInput(NS(id, "threshold2"), "Threshold", value = 1, min = 0, max = 1, step = 0.01)),
          column(3, textInput(NS(id, "nameFilterNA2Assay"), "Name", value = "proteins_filter_na")),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_filter_na2"), "Test", class = "btn-primary", style = "display: block;"))
        ),
        helpText("Removes proteins with proportion of missing values above threshold."),
        uiOutput(NS(id, "missingValUI2"))
      ),

      # --- Run all ---
      div(style = "margin-top: 20px; margin-bottom: 15px;",
        actionButton(NS(id, "run_all"), "Run all steps", class = "btn-success btn-lg")
      ),

      # --- Summary ---
      div(style = "margin-bottom: 15px;",
        tags$label("QFeatures summary"),
        verbatimTextOutput(NS(id, "qfeaturesSummary"))
      ),

      # --- Export ---
      div(style = "margin-bottom: 15px;",
        tags$label("Export QFeatures object"),
        downloadButton(NS(id, "rds_file"), "Export QFeatures object"),
        helpText("Exports the preprocessed QFeatures object.")
      ),

      div(
        br(),
        h4("How do I cite MSqRob?"),
        h4("msqrob2 is free to use and open source. When making use of msqrob2, we would appreciate it if you could cite our two papers."),
        h4("1. Sticker A, Goeminne L, Martens L, Clement L (2020). Robust Summarization and Inference in Proteome-wide Label-free Quantification. Molecular & Cellular Proteomics, 19(7), 1209-1219. doi: 10.1074/mcp.ra119.001624"),
        h4("2. Goeminne L, Gevaert K, Clement L (2016). Peptide-level Robust Ridge Regression Improves Estimation, Sensitivity, and Specificity in Data-dependent Quantitative Label-free Shotgun Proteomics. Molecular & Cellular Proteomics, 15(2), 657-668. doi: 10.1074/mcp.m115.055897")
      )
    )
  )
}


#' Server for preprocessing tab
#'
#' @param id module id
#' @param variables global reactive values object to share objects across modules
#' @return list of reactive inputs
#' @rdname INTERNAL_inputServer
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive showNotification removeNotification renderUI
#' @importFrom shinymeta metaReactive
#' @importFrom MultiAssayExperiment getWithColData
#' @importFrom DT datatable
#'
preprocessingServer <- function(id = "preprocessing", variables) {
  moduleServer(id, function(input, output, session) {

    # Flag set to TRUE whenever preprocessing itself writes variables$qfeatures,
    # so the snapshot observer below knows not to treat that write as a new import.
    preprocessingModifiedQF <- reactiveVal(FALSE)

    # Ensure qf_tmp is always initialised before any test runs
    ensureQfTmp <- function() {
      if (is.null(variables$qf_tmp) && !is.null(variables$qfeatures)) {
        variables$qf_tmp <- variables$qfeatures
      }
    }

    # ---- Snapshot for restore ----
    # Fires whenever variables$qfeatures changes. If the change came from
    # outside this module (i.e. a new import), reset the baseline and qf_tmp.
    # If this module itself wrote variables$qfeatures (run_all / restore),
    # the flag is TRUE and we skip the reset.
    observeEvent(variables$qfeatures, {
      if (preprocessingModifiedQF()) {
        preprocessingModifiedQF(FALSE)
        return()
      }
      variables$qfeatures_import <- variables$qfeatures
      variables$qf_tmp           <- variables$qfeatures
    })

    observeEvent(input$restore_qf, {
      req(variables$qfeatures_import)
      preprocessingModifiedQF(TRUE)
      variables$qfeatures <- variables$qfeatures_import
      variables$qf_tmp <- variables$qfeatures_import
      setDefaultFilters()
      updateTextInput(session, "nameLogAssay", value = variables$nameLogAssayDefault)
      showNotification("QFeatures restored from import", type = "message")
    })

    # ---- Helper: summarise dims ----
    showDims <- function(qf) {
      paste(sapply(names(qf), function(n) paste0(n, ": ", nrow(qf[[n]]), " features")), collapse = " | ")
    }

    # ---- Default filters ----
    filterList <- reactiveVal(list())

    setDefaultFilters <- function() {
      filterList(switch(variables$software,
        "diann" = list(
          "Precursor.Id != ''",
          "Decoy == 0",
          "Proteotypic == 1",
          "Q.Value <= 0.01",
          "PG.Q.Value <= 0.01",
          "Lib.Q.Value <= 0.01",
          "Lib.PG.Q.Value <= 0.01"
        ),
        "spectronaut" = list(
          "EG_IsDecoy %in% c(FALSE, 'False', '0')",
          "PEP_IsProteotypic %in% c(TRUE, 'True', '1')",
          "EG_Qvalue <= 0.01",
          "PG_Qvalue <= 0.01",
          "EG_IsImputed %in% c(FALSE, 'False', '0')"
        ),
        "maxquant" = {
          mq_filters <- list("Proteins != ''", "Reverse != '+'")
          if (!is.null(variables$qfeatures)) {
            rd_cols    <- colnames(SummarizedExperiment::rowData(variables$qfeatures[[1]]))
            contam_col <- rd_cols[grepl("contaminant", rd_cols, ignore.case = TRUE)][1]
            if (!is.na(contam_col)) mq_filters <- c(mq_filters, list(paste0(contam_col, " != '+'")))
          }
          mq_filters
        },
        "other"       = list(),
        list()
      ))

      switch(variables$software,
        "diann" = {
          variables$fColDefault              <- "Precursor.Id"
          variables$nameAssayDefault         <- "quants"
          variables$nameFilterNAAssayDefault <- "quants_filter_na"
          variables$nameLogAssayDefault      <- "quants_log"
          variables$normMethodDefault        <- "Median of Ratios"
          variables$nameNormAssayDefault     <- "quants_norm"
          variables$aggrMethodDefault        <- "maxLFQ"
          variables$aggrColDefault           <- "Protein.Group"
          variables$nameAggrAssayDefault     <- "proteins"
          variables$nameFilterNA2AssayDefault <- "proteins_filter_na"
          variables$nprecDefault             <- 1
        },
        "spectronaut" = {
          variables$fColDefault              <- "EG_PrecursorId"
          variables$nameAssayDefault         <- "quants"
          variables$nameFilterNAAssayDefault <- "quants_filter_na"
          variables$nameLogAssayDefault      <- "quants_log"
          variables$normMethodDefault        <- "Median of Ratios"
          variables$nameNormAssayDefault     <- "quants_norm"
          variables$aggrMethodDefault        <- "maxLFQ"
          variables$aggrColDefault           <- "PG_ProteinGroups"
          variables$nameAggrAssayDefault     <- "proteins"
          variables$nameFilterNA2AssayDefault <- "proteins_filter_na"
          variables$nprecDefault             <- 1
        },
        "maxquant" = {
          variables$fColDefault              <- NULL
          variables$nameAssayDefault         <- "quants"
          variables$nameFilterNAAssayDefault <- "quants_filter_na"
          variables$nameLogAssayDefault      <- "quants_log"
          variables$normMethodDefault        <- "Median of Ratios"
          variables$nameNormAssayDefault     <- "quants_norm"
          variables$aggrMethodDefault        <- "maxLFQ"
          variables$aggrColDefault           <- "Proteins"
          variables$nameAggrAssayDefault     <- "proteins"
          variables$nameFilterNA2AssayDefault <- "proteins_filter_na"
          variables$nprecDefault             <- 1
        },
        "other" = {
          variables$fColDefault              <- NULL
          variables$nameAssayDefault         <- "quants"
          variables$nameFilterNAAssayDefault <- "quants_filter_na"
          variables$nameLogAssayDefault      <- "quants_log"
          variables$normMethodDefault        <- "Median of Ratios"
          variables$nameNormAssayDefault     <- "quants_norm"
          variables$aggrMethodDefault        <- "maxLFQ"
          variables$aggrColDefault           <- NULL
          variables$nameAggrAssayDefault     <- "proteins"
          variables$nameFilterNA2AssayDefault <- "proteins_filter_na"
          variables$nprecDefault             <- 1
        }
      )
    }

    observeEvent(variables$software, {
      setDefaultFilters()
      updateTextInput(session, "nameFilterNAAssay", value = variables$nameFilterNAAssayDefault)
      updateTextInput(session, "nameLogAssay",       value = variables$nameLogAssayDefault)
      updateTextInput(session, "nameFilterNA2Assay", value = variables$nameFilterNA2AssayDefault)
    })

    # ---- Test: Zero to NA ----
    observeEvent(input$test_zero_to_na, {
      ensureQfTmp(); req(variables$qf_tmp)
      if (!isTRUE(input$doZeroToNA)) {
        showNotification("Zero to NA skipped", type = "message", duration = 3)
        return()
      }
      variables$qf_tmp <- try(QFeatures::zeroIsNA(variables$qf_tmp, i = names(variables$qf_tmp)))
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Filtering UI ----
    output$filtering <- renderUI({
      req(variables$qfeatures)
      rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
      rdCols <- colnames(rd)
      list(
        tags$label("Add filter"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, selectizeInput(NS(id, "filterCol"), "Column", choices = rdCols)),
          column(2, selectInput(NS(id, "filterOp"), "Operator",
                                choices = c("==", "!=", "<", ">", "<=", ">=", "%in%"))),
          column(2, textInput(NS(id, "filterVal"), "Value")),
          column(1, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_filter"), "Test", class = "btn-primary", style = "display: block;"))
        ),
        uiOutput(NS(id, "FilterList")),
        fluidRow(style = "margin-top: 5px;",
          column(2, actionButton(NS(id, "addFilter"), "Add")),
          column(3, actionButton(NS(id, "clearFilters"), "Clear all filters"))
        )
      )
    })

    observeEvent(input$addFilter, {
      req(input$filterCol, input$filterOp, input$filterVal)
      newFilter <- paste(input$filterCol, input$filterOp, input$filterVal)
      filterList(c(filterList(), newFilter))
    })

    output$FilterList <- renderUI({
      filters <- filterList()
      if (length(filters) == 0) return(NULL)
      tags$ul(lapply(filters, function(f) tags$li(f)))
    })

    observeEvent(input$clearFilters, { filterList(list()) })

    # ---- Test: Filter ----
    observeEvent(input$test_filter, {
      ensureQfTmp(); req(variables$qf_tmp)
      if (length(filterList()) == 0) {
        showNotification("No filters applied", type = "message", duration = 3)
        return()
      }
      variables$qf_tmp <- try({
        filter_formula <- as.formula(paste("~", paste(unlist(filterList()), collapse = " & ")))
        QFeatures::filterFeatures(variables$qf_tmp, filter_formula)
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Join assays (only when > 1 assay) ----
    output$join <- renderUI({
      req(variables$qfeatures)
      if (length(names(variables$qfeatures)) <= 1) return(NULL)
      rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
      rdCols <- colnames(rd)
      list(
        tags$label("Join assays"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, selectizeInput(NS(id, "fCol"), "Column", choices = rdCols, selected = variables$fColDefault)),
          column(3, textInput(NS(id, "nameAssay"), "Name", value = variables$nameAssayDefault)),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_join"), "Test", class = "btn-primary", style = "display: block;"))
        ),
        helpText("Joins multiple assays into a single assay.")
      )
    })

    # ---- Test: Join ----
    observeEvent(input$test_join, {
      ensureQfTmp(); req(variables$qf_tmp, input$fCol, input$nameAssay)
      if (length(names(variables$qf_tmp)) <= 1) return(NULL)
      variables$qf_tmp <- try({
        qf <- QFeatures::joinAssays(variables$qf_tmp, i = names(variables$qf_tmp), fcol = input$fCol, name = input$nameAssay)
        qf[, , input$nameAssay]
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Missing values plot ----
    missingValAssay <- reactive({
      req(variables$qf_tmp)
      nms <- names(variables$qf_tmp)
      if (!is.null(input$nameFilterNAAssay) && input$nameFilterNAAssay %in% nms) {
        input$nameFilterNAAssay
      } else if (!is.null(input$nameAssay) && input$nameAssay %in% nms) {
        input$nameAssay
      } else {
        nms[1]
      }
    })

    output$missingValUI <- renderUI({
      req(missingValAssay())
      fluidRow(column(6, plotOutput(NS(id, "missingValPlot"))))
    })

    output$missingValPlot <- renderPlot({
      req(variables$qf_tmp, missingValAssay(), input$threshold)
      PlotMissingValues(variables$qf_tmp, missingValAssay(), input$threshold)
    })

    # ---- Test: Filter NA ----
    observeEvent(input$test_filter_na, {
      ensureQfTmp()
      if (isTRUE(input$threshold >= 1)) {
        showNotification("Filter NA skipped (threshold = 1, no features removed)", type = "message", duration = 3)
        return()
      }
      req(variables$qf_tmp, input$threshold, input$nameFilterNAAssay)
      filterNA_i <- if (!is.null(input$nameAssay) && input$nameAssay %in% names(variables$qf_tmp)) {
        input$nameAssay
      } else {
        names(variables$qf_tmp)[1]
      }
      variables$qf_tmp <- try({
        se <- QFeatures::filterNA(variables$qf_tmp[, , filterNA_i], i = filterNA_i, pNA = input$threshold)[[filterNA_i]]
        QFeatures::addAssay(variables$qf_tmp, se, input$nameFilterNAAssay)
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "error", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Test: Log transform ----
    observeEvent(input$test_log, {
      ensureQfTmp()
      if (!isTRUE(input$doLog)) {
        showNotification("Log transform skipped (None)", type = "message", duration = 3)
        return()
      }
      req(variables$qf_tmp, input$nameLogAssay)
      # Fall back through the chain if earlier steps were skipped
      logInput <- if (!is.null(input$nameFilterNAAssay) && input$nameFilterNAAssay %in% names(variables$qf_tmp)) {
        input$nameFilterNAAssay
      } else if (!is.null(input$nameAssay) && input$nameAssay %in% names(variables$qf_tmp)) {
        input$nameAssay
      } else {
        names(variables$qf_tmp)[1]
      }
      variables$qf_tmp <- try(QFeatures::logTransform(variables$qf_tmp, base = 2, i = logInput, name = input$nameLogAssay))
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "error", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Normalisation UI ----
    output$normalisation <- renderUI({
      list(
        tags$label("Normalisation"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, selectInput(NS(id, "normMethod"), "Method",
                                choices = c("None", "sum", "max", "center.mean", "center.median",
                                            "div.mean", "div.median", "diff.median",
                                            "quantiles", "quantiles.robust", "Median of Ratios"),
                                selected = variables$normMethodDefault)),
          column(3, textInput(NS(id, "nameNormAssay"), "Name", value = if (!is.null(variables$nameNormAssayDefault)) variables$nameNormAssayDefault else "quants_norm")),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_norm"), "Test", class = "btn-primary", style = "display: block;"))
        )
      )
    })

    # ---- Test: Normalisation ----
    observeEvent(input$test_norm, {
      ensureQfTmp()
      if (input$normMethod == "None") {
        showNotification("Normalisation skipped (None)", type = "message", duration = 3)
        return()
      }
      normInput <- if (!isTRUE(input$doLog)) input$nameFilterNAAssay else input$nameLogAssay
      req(variables$qf_tmp, normInput, input$nameNormAssay)
      variables$qf_tmp <- try({
        if (input$normMethod == "Median of Ratios") {
          QFeatures::sweep(variables$qf_tmp, MARGIN = 2,
                           STATS = msqrob2::nfLogMedianOfRatios(variables$qf_tmp, normInput),
                           i = normInput, name = input$nameNormAssay)
        } else {
          QFeatures::normalize(variables$qf_tmp, method = input$normMethod, i = normInput, name = input$nameNormAssay)
        }
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Aggregation UI ----
    output$aggregation <- renderUI({
      req(variables$qfeatures)
      rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
      rdCols <- colnames(rd)
      list(
        tags$label("Aggregation"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, selectInput(NS(id, "aggrMethod"), "Method",
                                choices = c("None", "medianPolish", "robustSummary", "colMeans",
                                            "colMedians", "colSums", "maxLFQ"),
                                selected = variables$aggrMethodDefault)),
          column(3, textInput(NS(id, "nameAggrAssay"), "Name", value = variables$nameAggrAssayDefault))
        ),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(3, selectizeInput(NS(id, "aggrCol"), "Aggregation column", choices = rdCols, selected = variables$aggrColDefault)),
          column(2, numericInput(NS(id, "nprecFilter"), "N-peptides rule", value = if (!is.null(variables$nprecDefault)) variables$nprecDefault else 1, min = 1)),
          column(2, tags$label(HTML("&nbsp;")), actionButton(NS(id, "test_aggr"), "Test", class = "btn-primary", style = "display: block;"))
        )
      )
    })

    # ---- Test: Aggregation ----
    observeEvent(input$test_aggr, {
      ensureQfTmp()
      if (input$aggrMethod == "None") {
        showNotification("Aggregation skipped (None)", type = "message", duration = 3)
        return()
      }
      normInput <- if (!isTRUE(input$doLog))          input$nameFilterNAAssay else input$nameLogAssay
      aggrInput <- if (input$normMethod == "None")     normInput               else input$nameNormAssay
      req(variables$qf_tmp, aggrInput, input$aggrCol, input$nameAggrAssay, input$nprecFilter)
      variables$qf_tmp <- try({
        aggFun <- if (input$aggrMethod == "maxLFQ") {
          function(X) iq::maxLFQ(X)$estimate
        } else {
          switch(input$aggrMethod,
            "medianPolish"  = function(X) MsCoreUtils::medianPolish(X, na.rm = TRUE),
            "robustSummary" = function(X) MsCoreUtils::robustSummary(X, na.rm = TRUE),
            "colMeans"      = function(X) base::colMeans(X, na.rm = TRUE),
            "colMedians"    = function(X) matrixStats::colMedians(X, na.rm = TRUE),
            "colSums"       = function(X) base::colSums(X, na.rm = TRUE)
          )
        }
        qf <- QFeatures::aggregateFeatures(variables$qf_tmp, i = aggrInput, name = input$nameAggrAssay,
                                            fcol = input$aggrCol, fun = aggFun)
        counts <- SummarizedExperiment::assay(qf[[input$nameAggrAssay]], "aggcounts")
        a      <- SummarizedExperiment::assay(qf[[input$nameAggrAssay]])
        a[counts < input$nprecFilter] <- NA
        SummarizedExperiment::assay(qf[[input$nameAggrAssay]]) <- a
        qf
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Missing values plot (post-aggregation) ----
    missingValAssay2 <- reactive({
      req(variables$qf_tmp, input$nameAggrAssay)
      if (input$nameAggrAssay %in% names(variables$qf_tmp)) input$nameAggrAssay else NULL
    })

    output$missingValUI2 <- renderUI({
      req(missingValAssay2())
      fluidRow(column(6, plotOutput(NS(id, "missingValPlot2"))))
    })

    output$missingValPlot2 <- renderPlot({
      req(variables$qf_tmp, missingValAssay2(), input$threshold2)
      PlotMissingValues(variables$qf_tmp, missingValAssay2(), input$threshold2)
    })

    # ---- Test: Filter NA post-aggregation ----
    observeEvent(input$test_filter_na2, {
      ensureQfTmp()
      if (is.null(input$aggrMethod) || input$aggrMethod == "None") {
        showNotification("Requires aggregation first", type = "warning", duration = 4)
        return()
      }
      if (isTRUE(input$threshold2 >= 1)) {
        showNotification("Post-aggregation filter NA skipped (threshold = 1, no features removed)", type = "message", duration = 3)
        return()
      }
      req(variables$qf_tmp, input$nameAggrAssay, input$threshold2, input$nameFilterNA2Assay)
      variables$qf_tmp <- try({
        se <- QFeatures::filterNA(variables$qf_tmp[, , input$nameAggrAssay], i = input$nameAggrAssay, pNA = input$threshold2)[[input$nameAggrAssay]]
        QFeatures::addAssay(variables$qf_tmp, se, input$nameFilterNA2Assay)
      })
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Run all ----
    observeEvent(input$run_all, {
      req(variables$qfeatures)
      show_modal_spinner(spin = "cube-grid", color = "#112446", text = "Running all preprocessing steps...")

      qf <- variables$qfeatures

      # Step 1: zero to NA (in-place)
      if (isTRUE(input$doZeroToNA)) {
        qf <- try(QFeatures::zeroIsNA(qf, i = names(qf)))
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Zero to NA", type = "error"); return() }
      }

      # Step 2: filters
      if (length(filterList()) > 0) {
        qf <- try({
          filter_formula <- as.formula(paste("~", paste(unlist(filterList()), collapse = " & ")))
          QFeatures::filterFeatures(qf, filter_formula)
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Filtering", type = "error"); return() }
      }

      # Step 3: join if multiple assays
      if (length(names(qf)) > 1) {
        req(input$fCol, input$nameAssay)
        qf <- try({
          qf <- QFeatures::joinAssays(qf, i = names(qf), fcol = input$fCol, name = input$nameAssay)
          qf[, , input$nameAssay]
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Join assays", type = "error"); return() }
      }

      # Step 4: filter NA
      filterNA_i <- if (!is.null(input$nameAssay) && input$nameAssay %in% names(qf)) {
        input$nameAssay
      } else {
        names(qf)[1]
      }
      filterNA_out <- if (isTRUE(input$threshold >= 1)) {
        filterNA_i  # skip — pass through the current assay name
      } else {
        req(input$nameFilterNAAssay)
        qf <- try({
          se <- QFeatures::filterNA(qf[, , filterNA_i], i = filterNA_i, pNA = input$threshold)[[filterNA_i]]
          QFeatures::addAssay(qf, se, input$nameFilterNAAssay)
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Filter NA", type = "error"); return() }
        input$nameFilterNAAssay
      }

      # Step 5: log transform
      if (isTRUE(input$doLog)) {
        req(input$nameLogAssay)
        qf <- try(QFeatures::logTransform(qf, base = 2, i = filterNA_out, name = input$nameLogAssay))
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Log transform", type = "error"); return() }
      }
      normInput <- if (!isTRUE(input$doLog)) filterNA_out else input$nameLogAssay

      # Step 6: normalisation
      req(input$normMethod)
      if (input$normMethod != "None") {
        req(input$nameNormAssay)
        qf <- try({
          if (input$normMethod == "Median of Ratios") {
            QFeatures::sweep(qf, MARGIN = 2,
                             STATS = msqrob2::nfLogMedianOfRatios(qf, normInput),
                             i = normInput, name = input$nameNormAssay)
          } else {
            QFeatures::normalize(qf, method = input$normMethod, i = normInput, name = input$nameNormAssay)
          }
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Normalisation", type = "error"); return() }
      }
      aggrInput <- if (input$normMethod == "None") normInput else input$nameNormAssay

      # Step 7: aggregation
      req(input$aggrMethod)
      if (input$aggrMethod != "None") {
        req(input$aggrCol, input$nameAggrAssay, input$nprecFilter)
        qf <- try({
          aggFun <- if (input$aggrMethod == "maxLFQ") {
            function(X) iq::maxLFQ(X)$estimate
          } else {
            switch(input$aggrMethod,
              "medianPolish"  = function(X) MsCoreUtils::medianPolish(X, na.rm = TRUE),
              "robustSummary" = function(X) MsCoreUtils::robustSummary(X, na.rm = TRUE),
              "colMeans"      = function(X) base::colMeans(X, na.rm = TRUE),
              "colMedians"    = function(X) matrixStats::colMedians(X, na.rm = TRUE),
              "colSums"       = function(X) base::colSums(X, na.rm = TRUE)
            )
          }
          qf <- QFeatures::aggregateFeatures(qf, i = aggrInput, name = input$nameAggrAssay,
                                              fcol = input$aggrCol, fun = aggFun)
          counts <- SummarizedExperiment::assay(qf[[input$nameAggrAssay]], "aggcounts")
          a      <- SummarizedExperiment::assay(qf[[input$nameAggrAssay]])
          a[counts < input$nprecFilter] <- NA
          SummarizedExperiment::assay(qf[[input$nameAggrAssay]]) <- a
          qf
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Aggregation", type = "error"); return() }
      }
      # Step 8: filter NA post-aggregation (only when aggregation was performed and threshold < 1)
      if (input$aggrMethod != "None" && !isTRUE(input$threshold2 >= 1)) {
        req(input$nameFilterNA2Assay)
        qf <- try({
          se <- QFeatures::filterNA(qf[, , input$nameAggrAssay], i = input$nameAggrAssay, pNA = input$threshold2)[[input$nameAggrAssay]]
          QFeatures::addAssay(qf, se, input$nameFilterNA2Assay)
        })
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Post-aggregation filter NA", type = "error"); return() }
      }

      preprocessingModifiedQF(TRUE)
      variables$qfeatures <- qf
      remove_modal_spinner()
      showNotification("All preprocessing steps completed successfully", type = "message")
    })

    # ---- Summary ----
    output$qfeaturesSummary <- renderPrint({
      req(variables$qfeatures)
      variables$qfeatures
    })

    # ---- Export ----
    output$rds_file <- downloadHandler(
      filename = function() "qf.rds",
      content  = function(file) {
        req(variables$qfeatures)
        saveRDS(variables$qfeatures, file)
      }
    )

    return(list(
      qfeatures          = reactive(variables$qfeatures),
      filterList         = reactive(filterList()),
      doZeroToNA         = reactive(input$doZeroToNA),
      doLog              = reactive(input$doLog),
      fCol               = reactive(input$fCol),
      nameAssay          = reactive(input$nameAssay),
      threshold          = reactive(input$threshold),
      nameFilterNAAssay  = reactive(input$nameFilterNAAssay),
      nameLogAssay       = reactive(input$nameLogAssay),
      normMethod         = reactive(input$normMethod),
      nameNormAssay      = reactive(input$nameNormAssay),
      aggrMethod         = reactive(input$aggrMethod),
      aggrCol            = reactive(input$aggrCol),
      nameAggrAssay      = reactive(input$nameAggrAssay),
      nprecFilter        = reactive(input$nprecFilter),
      threshold2         = reactive(input$threshold2),
      nameFilterNA2Assay = reactive(input$nameFilterNA2Assay)
    ))
  })
}
