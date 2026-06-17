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
#' @importFrom shiny dataTableOutput

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
          column(2, actionButton(NS(id, "test_zero_to_na"), "Test", class = "btn-primary"))
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
          column(2, numericInput(NS(id, "threshold"), "Threshold", value = 1, min = 0, max = 1)),
          column(3, textInput(NS(id, "nameFilterNAAssay"), "Name", value = "quants_filter_na")),
          column(2, actionButton(NS(id, "test_filter_na"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        ),
        helpText("Removes features with proportion of missing values above threshold.")
      ),

      # --- Log transform ---
      div(style = "margin-bottom: 15px;",
        tags$label("Log2-transform"),
        fluidRow(style = "display: flex; align-items: flex-end; gap: 10px;",
          column(4, textInput(NS(id, "nameLogAssay"), "Name", value = "quants_log")),
          column(2, actionButton(NS(id, "test_log"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
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
          column(2, numericInput(NS(id, "threshold2"), "Threshold", value = 1, min = 0, max = 1)),
          column(3, textInput(NS(id, "nameFilterNA2Assay"), "Name", value = "proteins_filter_na")),
          column(2, actionButton(NS(id, "test_filter_na2"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        ),
        helpText("Removes proteins with proportion of missing values above threshold.")
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
#' @importFrom DT datatable renderDataTable
#'
preprocessingServer <- function(id = "preprocessing", variables) {
  moduleServer(id, function(input, output, session) {

    # ---- Snapshot for restore ----
    observeEvent(variables$qfeatures, {
      if (is.null(variables$qfeatures_import)) {
        variables$qfeatures_import <- variables$qfeatures
        variables$qf_tmp <- NULL
      }
    }, once = TRUE)

    observeEvent(input$restore_qf, {
      req(variables$qfeatures_import)
      variables$qfeatures <- variables$qfeatures_import
      setDefaultFilters()
      updateTextInput(session, "nameLogAssay", value = variables$nameLogAssayDefault)
      showNotification("QFeatures restored from import", type = "message")
    })

    # ---- Step functions ----
    stepZeroToNA <- function(qf) {
      QFeatures::zeroIsNA(qf, i = names(qf))
    }

    stepFilter <- function(qf, filters) {
      formula_str <- paste(filters, collapse = " & ")
      filter_formula <- as.formula(paste("~", formula_str))
      QFeatures::filterFeatures(qf, filter_formula)
    }

    stepJoin <- function(qf, fCol, nameAssay) {
      qf <- QFeatures::joinAssays(qf, i = names(qf), fcol = fCol, name = nameAssay)
      qf[, , nameAssay]
    }

    stepFilterNA <- function(qf, i, name, threshold) {
      se <- QFeatures::filterNA(qf[, , i], i = i, pNA = threshold)[[i]]
      QFeatures::addAssay(qf, se, name)
    }

    stepLog <- function(qf, i, name) {
      QFeatures::logTransform(qf, base = 2, i = i, name = name)
    }

    stepNorm <- function(qf, normMethod, nameLogAssay, nameNormAssay) {
      if (normMethod == "Median of Ratios") {
        QFeatures::sweep(qf, MARGIN = 2,
                         STATS = msqrob2::nfLogMedianOfRatios(qf, nameLogAssay),
                         i = nameLogAssay, name = nameNormAssay)
      } else {
        QFeatures::normalize(qf, method = normMethod, i = nameLogAssay, name = nameNormAssay)
      }
    }

    stepAggr <- function(qf, aggrMethod, nameNormAssay, aggrCol, nameAggrAssay, nprecFilter) {
      aggFun <- if (aggrMethod == "maxLFQ") {
        function(X) iq::maxLFQ(X)$estimate
      } else {
        switch(aggrMethod,
          "medianPolish"  = function(X) MsCoreUtils::medianPolish(X, na.rm = TRUE),
          "robustSummary" = function(X) MsCoreUtils::robustSummary(X, na.rm = TRUE),
          "colMeans"      = function(X) base::colMeans(X, na.rm = TRUE),
          "colMedians"    = function(X) matrixStats::colMedians(X, na.rm = TRUE),
          "colSums"       = function(X) base::colSums(X, na.rm = TRUE)
        )
      }
      qf <- QFeatures::aggregateFeatures(qf, i = nameNormAssay, name = nameAggrAssay,
                                          fcol = aggrCol, fun = aggFun)
      counts <- assay(qf[[nameAggrAssay]], "aggcounts")
      a <- assay(qf[[nameAggrAssay]])
      a[counts < nprecFilter] <- NA
      assay(qf[[nameAggrAssay]]) <- a
      qf
    }


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
        "maxquant" = list(
          "Proteins != ''",
          "Reverse != '+'"
        ),
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
      req(variables$qfeatures)
      
      variables$qf_tmp <- try(stepZeroToNA(variables$qfeatures))
      
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
          column(1, actionButton(NS(id, "addFilter"), "Add", style = "margin-bottom: 0;")),
          column(1, actionButton(NS(id, "test_filter"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        ),
        uiOutput(NS(id, "FilterList")),
        fluidRow(style = "margin-top: 5px;",
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
      req(variables$qf_tmp)
      req(length(filterList()) > 0)
      variables$qf_tmp <- try(stepFilter(variables$qf_tmp, filterList()))
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
          column(2, actionButton(NS(id, "test_join"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        ),
        helpText("Joins multiple assays into a single assay.")
      )
    })

    # ---- Test: Join ----
    observeEvent(input$test_join, {
      req(variables$qf_tmp, input$fCol, input$nameAssay)
      if (length(names(variables$qf_tmp)) <= 1) return(NULL)
      variables$qf_tmp <- try(stepJoin(variables$qf_tmp, input$fCol, input$nameAssay))
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Test: Filter NA ----
    observeEvent(input$test_filter_na, {
      req(variables$qf_tmp, input$threshold, input$nameFilterNAAssay)
      filterNA_i <- if (!is.null(input$nameAssay) && input$nameAssay %in% names(variables$qf_tmp)) {
        input$nameAssay
      } else {
        names(variables$qf_tmp)[1]
      }
      variables$qf_tmp <- try(stepFilterNA(variables$qf_tmp, filterNA_i, input$nameFilterNAAssay, input$threshold))
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "error", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Test: Log transform ----
    observeEvent(input$test_log, {
      req(variables$qf_tmp, input$nameFilterNAAssay, input$nameLogAssay)
      variables$qf_tmp <- try(stepLog(variables$qf_tmp, input$nameFilterNAAssay, input$nameLogAssay))
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
                                choices = c("sum", "max", "center.mean", "center.median",
                                            "div.mean", "div.median", "diff.median",
                                            "quantiles", "quantiles.robust", "Median of Ratios"),
                                selected = variables$normMethodDefault)),
          column(3, textInput(NS(id, "nameNormAssay"), "Name", value = if (!is.null(variables$nameNormAssayDefault)) variables$nameNormAssayDefault else "quants_norm")),
          column(2, actionButton(NS(id, "test_norm"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        )
      )
    })

    # ---- Test: Normalisation ----
    observeEvent(input$test_norm, {
      req(variables$qf_tmp, input$normMethod, input$nameLogAssay, input$nameNormAssay)
      variables$qf_tmp <- try(stepNorm(variables$qf_tmp, input$normMethod, input$nameLogAssay, input$nameNormAssay))
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
                                choices = c("medianPolish", "robustSummary", "colMeans",
                                            "colMedians", "colSums", "maxLFQ"),
                                selected = variables$aggrMethodDefault)),
          column(3, selectizeInput(NS(id, "aggrCol"), "Aggregation column", choices = rdCols, selected = variables$aggrColDefault)),
          column(2, textInput(NS(id, "nameAggrAssay"), "Name", value = variables$nameAggrAssayDefault)),
          column(1, numericInput(NS(id, "nprecFilter"), "Min prec.", value = if (!is.null(variables$nprecDefault)) variables$nprecDefault else 1, min = 1)),
          column(1, actionButton(NS(id, "test_aggr"), "Test", class = "btn-primary", style = "margin-bottom: 0;"))
        )
      )
    })

    # ---- Test: Aggregation ----
    observeEvent(input$test_aggr, {
      req(variables$qf_tmp, input$aggrMethod, input$nameNormAssay, input$aggrCol, input$nameAggrAssay, input$nprecFilter)
      variables$qf_tmp <- try(stepAggr(variables$qf_tmp, input$aggrMethod, input$nameNormAssay,
                              input$aggrCol, input$nameAggrAssay, input$nprecFilter))
      if (inherits(variables$qf_tmp, "try-error")) {
        showNotification("Test failed", type = "warning", duration = 5)
      } else {
        showNotification("Okay!", type = "message", duration = 5)
      }
    })

    # ---- Test: Filter NA post-aggregation ----
    observeEvent(input$test_filter_na2, {
      req(variables$qf_tmp, input$threshold2, input$nameAggrAssay, input$nameFilterNA2Assay)
      variables$qf_tmp <- try(stepFilterNA(variables$qf_tmp, input$nameAggrAssay, input$nameFilterNA2Assay, input$threshold2))
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

      # Step 1: zero to NA
      qf <- try(stepZeroToNA(qf))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Zero to NA", type = "error"); return() }

      # Step 2: filters
      if (length(filterList()) > 0) {
        qf <- try(stepFilter(qf, filterList()))
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Filtering", type = "error"); return() }
      }

      # Step 3: join if multiple assays
      if (length(names(qf)) > 1) {
        req(input$fCol, input$nameAssay)
        qf <- try(stepJoin(qf, input$fCol, input$nameAssay))
        if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Join assays", type = "error"); return() }
      }

      # Step 4: filter NA
      req(input$nameFilterNAAssay)
      filterNA_i <- if (!is.null(input$nameAssay) && input$nameAssay %in% names(qf)) {
        input$nameAssay
      } else {
        names(qf)[1]
      }
      qf <- try(stepFilterNA(qf, filterNA_i, input$nameFilterNAAssay, input$threshold))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Filter NA", type = "error"); return() }

      # Step 5: log transform
      req(input$nameLogAssay)
      qf <- try(stepLog(qf, input$nameFilterNAAssay, input$nameLogAssay))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Log transform", type = "error"); return() }

      # Step 6: normalisation
      req(input$normMethod, input$nameNormAssay)
      qf <- try(stepNorm(qf, input$normMethod, input$nameLogAssay, input$nameNormAssay))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Normalisation", type = "error"); return() }

      # Step 7: aggregation
      req(input$aggrMethod, input$aggrCol, input$nameAggrAssay, input$nprecFilter)
      qf <- try(stepAggr(qf, input$aggrMethod, input$nameNormAssay, input$aggrCol, input$nameAggrAssay, input$nprecFilter))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Aggregation", type = "error"); return() }

      # Step 8: filter NA post-aggregation
      req(input$nameFilterNA2Assay)
      qf <- try(stepFilterNA(qf, input$nameAggrAssay, input$nameFilterNA2Assay, input$threshold2))
      if (inherits(qf, "try-error")) { remove_modal_spinner(); showNotification("Failed at: Post-aggregation filter NA", type = "error"); return() }

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

    return(list(qfeatures = reactive(variables$qfeatures)))
  })
}
