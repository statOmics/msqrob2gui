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


preprocessingUI <- function(id="preprocessing")
{
  fluidRow(
    column(width=12,
           div(
             list(
               tags$label("Convert NAs", `for`="zero_to_na"),
               actionButton(NS(id, "zero_to_na"),"Convert zero values to NAs"),
               helpText(id = "tooltip_na",
                        "The button converts all zero values into NA entries.")
             )
           ),
           uiOutput(NS(id, "filtering")),
           uiOutput(NS(id, "join")),
           div(
             list(
               tags$label("Join assays", `for`="join"),
               actionButton(NS(id, "join"),"Join the assays between runs"),
               helpText(id = "tooltip_join",
                        "The button joins the multiple assays of the QFeatures object in a single assay.")
             )
           ),
           
           div(
             style = "display: flex; align-items: center; gap: 10px;",
             tags$label("Filter missing values", `for` = "filter_na"),
             actionButton(NS(id, "filter_na"), "Filter features with many missing values"),
             div(style = "width: 100px;", numericInput(NS(id, "threshold"), "Value",value = NULL, min =0)),
             helpText(id = "tooltip_filter_na",
                      "The button removes the features with a proportion of missing values above the provided threshold.")
           ),
           uiOutput(NS(id, "pepsPerProtCol")),
           
           div(
             list(
               tags$label("Log2-transfrom", `for`="log"),
               fluidRow(
                 column(4, actionButton(NS(id, "log"),"Apply log2 transformation")),
                 column(3, textInput(NS(id, "nameLogAssay"), "Name Log-transformed Experiment"))
               )
             )
           ),
           uiOutput(NS(id, "normalisation")),
           uiOutput(NS(id, "aggregation")),
           div(
             list(
               tags$label("QFeatures summary"),
               verbatimTextOutput(NS(id, "qfeaturesSummary"))
             )
           ),
           div(
             list(
               tags$label("Export QFeatures object", `for`="rds_file"),
               downloadButton(NS(id, "rds_file"),"Export QFeatures object"),
               helpText(id = "tooltip_export_qf",
                        "The button exports the preprocessed QFeatures object.")
             )
           ),
           div(
             list(
               br(),
               h4("How do I cite MSqRob?"),
               h4("msqrob2 is free to use and  open source.
            When making use of msqrob2, we would appreciate it if you could cite our two papers."),
               h4("1. Sticker A, Goeminne L, Martens L, Clement L (2020). Robust Summarization and Inference in Proteome-wide Label-free Quantification. Molecular & Cellular Proteomics, 19(7), 1209-1219. doi: 10.1074/mcp.ra119.001624"),
               h4("2. Goeminne L, Gevaert K, Clement L (2016). Peptide-level Robust Ridge Regression Improves Estimation, Sensitivity, and Specificity in Data-dependent Quantitative Label-free Shotgun Proteomics. Molecular & Cellular Proteomics, 15(2), 657-668. doi: 10.1074/mcp.m115.055897")
             )
           )
    ) #end column
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
preprocessingServer <- function(id="preprocessing", variables){
  moduleServer(
    id,
    function(input,output,session){
     
      # convert zeros to na
      observeEvent(input$zero_to_na, {
        req(variables$qfeatures)
        
        variables$qfeatures <- QFeatures::zeroIsNA(variables$qfeatures,
                                                   i = names(variables$qfeatures))
        
        showNotification("Zero values converted to NA", type = "message")
        
      })
    
      # select filtering options
      output$filtering <- renderUI({
        req(variables$qfeatures)
        
        rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
        rdCols <- colnames(rd)
        
        list(
          tags$label("Add filter"),
          fluidRow(
            column(4, selectizeInput(NS(id, "filterCol"), "Column", choices = rdCols)),
            column(3, selectInput(NS(id, "filterOp"), "Operator", 
                                  choices = c("==","!=","<",">","<=",">=", "%in%"))),
            column(3, textInput(NS(id, "filterVal"), "Value")),
            column(2, actionButton(NS(id, "addFilter"), "Add"))
          ),
          uiOutput(NS(id, "FilterList")),
          actionButton(NS(id, "clearFilters"), "Clear all filters"),
          actionButton(NS(id, "perform_filter"), "Apply selected filtering options")
        )
      })
      
      # store the filtering options provided
      filterList <- reactiveVal(list())
      
      observeEvent(input$addFilter, {
        req(input$filterCol, input$filterOp, input$filterVal)
        
        newFilter <- paste(input$filterCol, input$filterOp, input$filterVal)
        filterList(c(filterList(), newFilter))
      })
      
      output$FilterList <- renderUI({
        filters <- filterList()
        if (length(filters) == 0) return(NULL)
        
        tags$ul(
          lapply(filters, function(f) tags$li(f))
        )
      })
      
      # clear and restart the filtering options
      observeEvent(input$clearFilters, {
        filterList(list())
      })
      
      # perform filtering upon pressing the button
      observeEvent(input$perform_filter, {
        req(variables$qfeatures)
        req(length(filterList()) > 0)
        
        formula_str <- paste(filterList(), collapse = " & ")
        filter_formula <- as.formula(paste("~", formula_str))
        
        variables$qfeatures <- QFeatures::filterFeatures(variables$qfeatures,
                                                         filter_formula)
        
        showNotification("Filtering applied", type = "message")
        
      })
      
      
      # joining assays
      output$join <- renderUI({
        req(variables$qfeatures)
        
        rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
        rdCols <- colnames(rd)
        
        list(
          tags$label("Select fcol to join the assays"),
          fluidRow(
            column(4, selectizeInput(NS(id, "fCol"), "Column", choices = rdCols)),
            column(3, textInput(NS(id, "nameAssay"), "Name Summarized Experiment"))
          )
        )
        })
        
        observeEvent(input$join,{
          req(input$fCol)
          req(input$nameAssay)
          variables$qfeatures <- QFeatures::joinAssays(variables$qfeatures,
                                                       i = names(variables$qfeatures),
                                                       fcol = input$fCol,
                                                       name = input$nameAssay
                                                      )
          variables$qfeatures <- variables$qfeatures[, , input$nameAssay]
          showNotification("Assays joined", type = "message")
        })
        
        # Filter by proportion of missing values
        observeEvent(input$filter_na,{
          req(input$filter_na)
          req(input$threshold)
          
          
          variables$qfeatures <- QFeatures::filterNA(variables$qfeatures,
                                                     i = names(variables$qfeatures),
                                                     pNA = input$threshold)
          showNotification("Missing values filtered", type = "message")
          
        })
        
        output$pepsPerProtCol <- renderUI({
          req(variables$qfeatures)
          
          rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
          rdCols <- colnames(rd)
          
          list(
            tags$label("Filter proteins"),
            fluidRow(
              column(4, selectizeInput(NS(id, "precursorCol"), "Precursors Id", choices = rdCols)),
              column(3, selectizeInput(NS(id, "proteinCol"),  "Protein Id", choices = rdCols)),
              column(2, numericInput(NS(id, "nprec"), "Min precursors", value = 1, min = 0)),
              column(2, actionButton(NS(id, "countPepsPerProt"), "Count"))
            )
          )
        })
        
        # Filter by number of precursors mapping to a protein
        observeEvent(input$countPepsPerProt,{
          req(input$nprec)
          req(input$precursorCol)
          req(input$proteinCol)
          
          for (i in names(variables$qfeatures)) {
            
            rd <- SummarizedExperiment::rowData(variables$qfeatures[[i]]) |> 
              as.data.frame()
            
            pepsPerProtDf <- rd |>
              dplyr::select(dplyr::all_of(c(input$precursorCol, input$proteinCol))) |>
              dplyr::group_by(.data[[input$proteinCol]]) |>
              dplyr::mutate(pepsPerProt = .data[[input$precursorCol]] |> 
                              unique() |> length()) |>
              dplyr::ungroup()
            
            SummarizedExperiment::rowData(variables$qfeatures[[i]])$pepsPerProt <- 
              pepsPerProtDf$pepsPerProt
          }
          
          filter_formula <- as.formula(paste("~ pepsPerProt >", input$nprec))
          variables$qfeatures <- QFeatures::filterFeatures(variables$qfeatures,
                                                           i = names(variables$qfeatures),
                                                           filter_formula,
                                                           keep = TRUE
                                                           )
          showNotification("Proteins filtered", type = "message")
        })
        
        # log transform
        observeEvent(input$log, {
          req(variables$qfeatures)
          req(input$nameLogAssay)
          
          variables$qfeatures <- QFeatures::logTransform(variables$qfeatures,
                                                         base =2,
                                                        i = names(variables$qfeatures),
                                                        name = input$nameLogAssay
                                                        )
          
          showNotification("Applied log-transformation", type = "message")
          
        })
        
        # Normalisation
        output$normalisation <- renderUI({
          
          list(
            tags$label("Normalisation"),
            fluidRow(
              column(4, selectInput(NS(id, "normMethod"), "Normalisation method", 
                                       choices = c("sum","max",
                                                   "center.mean","center.median",
                                                   "div.mean","div.median",
                                                   "diff.median","quantiles",
                                                   "quantiles.robust", "Median of Ratios"))),
              column(2, textInput(NS(id, "nameNormAssay"), "Name normalised assay")),
              column(2, actionButton(NS(id, "perform_norm"), "Normalise"))
            )
          )
        })
        
        observeEvent(input$perform_norm,{
          req(input$normMethod)
          req(input$nameNormAssay)
          req(input$nameLogAssay)
          # remove existing assay with same name if present
          if (input$nameNormAssay %in% names(variables$qfeatures)) {
            variables$qfeatures <- variables$qfeatures[, , -which(names(variables$qfeatures) == input$nameNormAssay)]
          }
          
          if(input$normMethod == "Median of Ratios"){
            variables$qfeatures <- QFeatures::sweep( 
              variables$qfeatures,
              MARGIN = 2,
              STATS = msqrob2::nfLogMedianOfRatios(variables$qfeatures, input$nameLogAssay),
              i = input$nameLogAssay,
              name = input$nameNormAssay
            )
          } else {
            variables$qfeatures <- QFeatures::normalize( 
              variables$qfeatures,
              method = input$normMethod,
              i = input$nameLogAssay,
              name = input$nameNormAssay
            )
          }
          showNotification("Applied normalisation", type = "message")
        })
        
        # aggregation
        output$aggregation <- renderUI({
          rd <- SummarizedExperiment::rowData(variables$qfeatures[[1]])
          rdCols <- colnames(rd)
          
          list(
            tags$label("Aggregation"),
            fluidRow(
              column(4, selectInput(NS(id, "aggrMethod"), "Aggregation method", 
                                    choices = c("medianPolish","robustSummary",
                                                "colMeans","colMedians",
                                                "colSums","maxLFQ"))),
              column(4, selectizeInput(NS(id, "fCol"), "Aggregation column", choices = rdCols)),
              column(2, textInput(NS(id, "nameAggrAssay"), "Name aggregated assay")),
              column(2, actionButton(NS(id, "perform_aggr"), "Aggregate"))
            )
          )
        })
        
        
        observeEvent(input$perform_aggr,{
          req(input$aggrMethod)
          req(input$nameNormAssay)
          req(input$fCol)
          req(input$nameAggrAssay)
          # remove existing assay with same name if present
          if (input$nameAggrAssay %in% names(variables$qfeatures)) {
            variables$qfeatures <- variables$qfeatures[, , -which(names(variables$qfeatures) == input$nameAggrAssay)]
          }
          
          aggFun <- if (input$aggrMethod == "maxLFQ") {
            function(X) iq::maxLFQ(X)$estimate
          } else {
            switch(input$aggrMethod,
                   "medianPolish"  = function(X) MsCoreUtils::medianPolish(X,na.rm=TRUE),
                   "robustSummary" = function(X) MsCoreUtils::robustSummary(X, na.rm=TRUE),
                   "colMeans"      = function(X) base::colMeans(X, na.rm = TRUE),
                   "colMedians"    = function(X) matrixStats::colMedians(X, na.rm = TRUE),
                   "colSums"       = function(X) base::colSums(X, na.rm = TRUE)
            )
            
          }
          
          variables$qfeatures <- QFeatures::aggregateFeatures(
            variables$qfeatures, i = input$nameNormAssay, 
            name = input$nameAggrAssay,
            fcol = input$fCol, 
            fun = aggFun
          )
          showNotification("Features aggregated", type = "message")
        })
        
        # print summary Qfeature object
        output$qfeaturesSummary <- renderPrint({
          req(variables$qfeatures)
          variables$qfeatures
        })
        
        output$rds_file <- downloadHandler(
          filename = function() "qf.rds",
          content  = function(file) {
            req(variables$qfeatures)
            saveRDS(variables$qfeatures,file)
          }
        )
      
      return(
        list(
          qfeatures = reactive(variables$qfeatures)
        )
      )
    }
  )
}

