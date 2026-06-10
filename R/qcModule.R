#' input UI
#'
#' @return A shiny tagList object that contains the input UI components
#' @rdname INTERNAL_inputUI
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput helpText tags div
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#' @importFrom DT dataTableOutput


inputUI <- function(id="import")
{
  fluidRow(
    column(width=12,
           div(
             list(tags$label("QFeatures input RDS file", `for`="pe"),
                  # tags$button("button_pe", tags$sup("[?]")),
                  fileInput(inputId=NS(id,"pe"), label=NULL, multiple = FALSE, accept = NULL, width = NULL),
                  # hidden(
                    helpText(
                      id="tooltip_qfeatures",
                      "Specify the location of the RDS file that contains
                           your qfeatures object."
                    )
                  #) # close hidden
             )
           ),
           div(
             list(
               tags$label("Select Assay", `for`="selectAssay"),
               #tags$button("button_selectedAssay", tags$sup("[?]")),
               uiOutput(NS(id,"selectAssay")),
               #hidden(
                 helpText(id = "tooltip_select_assay",
                          "Select the QFeatures assay for the differential analysis.
        	                      ")
               #) #close hidden
             )
           ),
    #), end column
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
               #tags$button("button_selectedVariable", tags$sup("[?]")),
               uiOutput(NS(id,"selectVariable")),
               #hidden(
               helpText(id = "tooltip_select_assay",
                        "Select variable to color data in plots
        	                      ")
               #) #close hidden
             )
           ),
          uiOutput(NS(id,'densityPlot')),
          uiOutput(NS(id,'pcaPlot')),
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
inputServer <- function(id="import", variables){
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
        variables$pe <- peOut
      })

      # get assay names and colData variable names
      assayNamesPe <- reactive(names(variables$pe))
      colDataNames <- reactive(names(colData(variables$pe)))

      # select assay
      output$selectAssay<- renderUI({
        selectInput(session$ns("selectedAssay"), NULL, assayNamesPe(), selected = variables$selectedAssay, width = '100%')})

      output$selectVariable<- renderUI({
        selectInput(session$ns("selectedVariable"), NULL, colDataNames(), width = '100%')})

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
          variables$pe
        )
      })

      output$qfeaturesTable <- DT::renderDataTable({
        DT::datatable(qfeatures_df(),
                      extensions = "FixedColumns",
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
            data.frame(assay(variables$pe[[row]])),
            extensions = "FixedColumns",
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

      output$densityPlot <- renderUI(
        renderPlot(
          plotDensities(variables$pe,
                        input$selectedAssay,
                        input$selectedVariable)
          )
      )

      ### PCA plot

      output$pcaPlot <- renderUI(
        {
          renderPlot(
            plotPCA(variables$pe,
                    input$selectedAssay,
                    input$selectedVariable)
            )
        }
        )
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

