#' qc UI
#'
#' @return A shiny tagList object that contains the input UI components
#' @rdname INTERNAL_inputUI
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput helpText tags div plotOutput renderPlot selectInput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#' @importFrom shiny dataTableOutput
#' @importFrom SummarizedExperiment colData assay


qcUI <- function(id="qc")
{
  fluidRow(
    column(width=12,
           div(
             list(tags$label("QFeatures input RDS file", `for`="pe"),
                  fileInput(inputId=NS(id,"pe"), label=NULL, multiple = FALSE, accept = NULL, width = NULL),
                    helpText(
                      id="tooltip_qfeatures",
                      "Specify the location of the RDS file that contains
                           your QFeatures object."
                    )
             )
           ),
           div(
             list(
               tags$label("Select Assay", `for`="selectAssay"),
               uiOutput(NS(id,"selectAssay")),
               
                 helpText(id = "tooltip_select_assay",
                          "Select the QFeatures assay for the differential analysis.
        	                      ")
             )
           ),
    # column(width=8,
           box(
             title = "QFeatures Preview",
             status = "primary",
             width = 8,
             solidHeader = FALSE,
             collapsible = TRUE,
             id = NS(id, "qfeatures_preview"),
             DT::dataTableOutput(NS(id, "qfeaturesTable"))
           ),
           box(
             title = "Selected Assay Preview",
             status = "primary",
             width = 12,
             solidHeader = FALSE,
             collapsible = TRUE,
             id = NS(id, "assay_preview"),
             DT::dataTableOutput(NS(id, "assayTable"))
           ),
           div(
             list(
               tags$label("Select variable", `for`="selectVariable"),
               uiOutput(NS(id,"selectVariable")),
               
               helpText(id = "tooltip_select_assay",
                        "Select variable to color data in plots
        	                      ")
             )
           ),
          fluidRow(
            box(
              title = "Intensity Distributions",
              status = "primary", width = 6,
              solidHeader = FALSE, collapsible = TRUE,
              plotOutput(NS(id, "densityPlot"))
            ),
            box(
              title = "Sample Boxplots",
              status = "primary", width = 6,
              solidHeader = FALSE, collapsible = TRUE,
              plotOutput(NS(id, "boxplot"))
            )
          ),
          fluidRow(
            box(
              title = "Identifications per Sample",
              status = "primary", width = 6,
              solidHeader = FALSE, collapsible = TRUE,
              plotOutput(NS(id, "identificationsPlot"))
            ),
            box(
              title = "Charge States",
              status = "primary", width = 6,
              solidHeader = FALSE, collapsible = TRUE,
              plotOutput(NS(id, "chargeStatesPlot"))
            )
          ),
          box(
            title = "Dimensionality Reduction",
            status = "primary", width = 12,
            solidHeader = FALSE, collapsible = TRUE,
            selectInput(NS(id, "dimRedMethod"), "Method",
                        choices = c("MDS", "OmicsGMF"), selected = "MDS"),
            plotOutput(NS(id, "dimRedPlot"))
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

#' Server for input tab
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
qcServer <- function(id="qc", variables){
  moduleServer(
    id,
    function(input,output,session){
      #extract datapath of inputfile
      peDatapath <- metaReactive({getDataPath(..(input$pe$datapath))})

      #import qfeature object
      observeEvent({input$pe},{
        peOut <- try(readRDS(file=peDatapath()))
        if (class(peOut)[1]!="QFeatures") {
          showNotification("Upload proper QFeatures RDS file",id="noProperRDSFile",type="error",duration=NULL,closeButton=FALSE)
        } else {
          removeNotification(id="noProperRDSFile")}
        variables$qfeatures <- peOut
      })

      # get assay names and colData variable names
      assayNamesPe <- reactive(names(variables$qfeatures))
      colDataNames <- reactive(names(colData(variables$qfeatures)))

      # select assay
      output$selectAssay<- renderUI({
        selectInput(session$ns("selectedAssay"), NULL, assayNamesPe(), selected = variables$selectedAssay, width = '100%')})

      output$selectVariable<- renderUI({
        selectInput(session$ns("selectedVariable"), NULL, colDataNames())})

      observeEvent(input$selectedAssay,
                   {
                     variables$selectedAssay <- input$selectedAssay
                     variables$selectedLowLevelAssay <- input$selectedAssay
                   })

      # convert qfeatures object to data table for exploration
        qfeatures_df <- reactive({
        error_handler(
          qfeatures_to_df,
          component_name = "qfeatures_to_df",
          variables$qfeatures
        )
      })

      output$qfeaturesTable <- DT::renderDataTable({
        DT::datatable(qfeatures_df(),
                      #extensions = "FixedColumns",
                      selection = "single",
                      options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15)
                      )
        )
      })


      output$assayTable <- DT::renderDataTable({
        if (!is.null(input$selectedAssay)) {
          row <- input$selectedAssay
          DT::datatable(
            data.frame(assay(variables$qfeatures[[row]])),
            #extensions = "FixedColumns",
            options = list(
              searching = FALSE,
              scrollX = TRUE,
              fixedColumns = TRUE,
              pageLength = 5,
              lengthMenu = c(5, 10, 15, 20)
            )
          )
        }
      })

      output$densityPlot <- renderPlot({
        req(variables$qfeatures, input$selectedAssay, input$selectedVariable)
        NewPlotDensities(variables$qfeatures, input$selectedAssay, input$selectedVariable)
      })

      output$boxplot <- renderPlot({
        req(variables$qfeatures, input$selectedAssay, input$selectedVariable)
        PlotNormBoxplots(variables$qfeatures, input$selectedAssay, input$selectedVariable)
      })

      output$identificationsPlot <- renderPlot({
        req(variables$qfeatures, input$selectedAssay, input$selectedVariable)
        PlotIdentifications(variables$qfeatures, input$selectedAssay, input$selectedVariable)
      })

      output$chargeStatesPlot <- renderPlot({
        req(variables$qfeatures, input$selectedAssay)
        PlotChargeStates(variables$qfeatures, input$selectedAssay)
      })

      output$dimRedPlot <- renderPlot({
        req(variables$qfeatures, input$selectedAssay, input$selectedVariable, input$dimRedMethod)
        PlotDimReduction(variables$qfeatures, input$selectedAssay, input$selectedVariable, input$dimRedMethod)
      })
      return(
        list(
          selectedAssay = reactive(input$selectedAssay),
          peDatapath = peDatapath,
          assayNamesPe = assayNamesPe
          )
        )
    }
  )
}

