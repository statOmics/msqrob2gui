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


importUI <- function(id="import")
{
  fluidRow(
    column(width=12,
           div(
             list(
               tags$label("Software", `for`="software"),
               selectInput(NS(id, "software"), label = NULL,
                           choices = c("DIA-NN"      = "diann",
                                       "Spectronaut" = "spectronaut"),
                           selected = "diann"
                           ),
               
               helpText(id = "tooltip_select_software",
                        "Specify the software used for identification. DIA-NN is set as default.")
             )
           ),
           div(
             list(tags$label("Input file", `for`="pe"),
                  fileInput(inputId=NS(id,"pe"), label=NULL, multiple = FALSE, accept = NULL, width = NULL),
                    helpText(
                      id="tooltip_input",
                      "Select the input file."
                    )
             )
           ),
           div(
             list(
               tags$label("Intensity column(s)", `for`="quantCols"),
               textInput(NS(id, "quantCols"), 
                         label = NULL,
                         placeholder = "e.g. Precursor.Normalised or 5, 6, 7"),
               helpText(id = "tooltip_quantcols",
                        "Specify the column name or index of the intensity columns. For the input file in wide format specify multiple columns separated with a comma.")
             )
           ),
           div(
             list(
               tags$label("Export sample names for the annotation file", `for`="printed_annot"),
               downloadButton(NS(id, "printed_annot"),"Export sample names"),
               helpText(id = "tooltip_printed_annot",
                        "The button generate a csv file with the samples' names of the input file, which the user can modify to create an annotation file.")
             )
           ),
      
           div(
             list(
               tags$label("Annotation file", `for`="annot"),
               fileInput(inputId=NS(id,"annot"), label=NULL, multiple = FALSE, accept = NULL, width = NULL),
               
                 helpText(id = "tooltip_annotation",
                          "If available, upload the annotation file")
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
importServer <- function(id="import", variables){
  moduleServer(
    id,
    function(input,output,session){
      #extract datapath of inputfile
      peDatapath <- metaReactive({getDataPath(..(input$pe$datapath))})

      #import the input file
      observeEvent({input$pe},{
        peOut <- try(arrow::read_parquet(file=peDatapath()))
        if (inherits(peOut, "try-error")) {
          peOut <- try(data.table::fread(file=peDatapath()))
          
          if (inherits(peOut, "try-error")){
            showNotification("Upload proper input file",id="noProperFile",type="error",duration=NULL,closeButton=FALSE)
          }
          
        } else {
          removeNotification(id="noProperFile")}
        variables$pe <- peOut
      })
      
      #extract datapath of annotation file
      annotDatapath <- metaReactive({getDataPath(..(input$annot$datapath))})
      
      #import the annotation file
      observeEvent({input$annot},{
          annotOut <- try(data.table::fread(file=annotDatapath(),check.names = TRUE))
          if (inherits(annotOut, "try-error")) {
            showNotification("Upload proper annotation file",id="noProperAnnotFile",type="error",duration=NULL,closeButton=FALSE)
          }
         else {
          removeNotification(id="noProperAnnotFile")}
        variables$annot <- annotOut
        }
      )
      
      # waiting time for user
      quantColsDebounced <- debounce(reactive(input$quantCols), 1000)  # wait 1 second
      
      # Get the intensity columns
      parsedQuantCols <- reactive({
        req(quantColsDebounced())
        req(variables$pe)
        val <- trimws(unlist(strsplit(input$quantCols, ",")))
        
        # check if indices or names
        if (all(!is.na(suppressWarnings(as.integer(val))))) {
          as.integer(val)   # indices
        } else {
          which(colnames(variables$pe) %in% val)  # column names
        }
      })
      
      
      # build QFeatures
      qfeatures <- reactive({
        req(variables$pe)
        req(input$quantCols)
        
        pe <- if (input$software == "diann") {
          QFeatures::readQFeatures(
            assayData = variables$pe,
            fnames    = "Precursor.Id",
            runCol    = "Run",
            quantCol  = parsedQuantCols(),
            name      = "precursors"
          )
        } else {
          quantCols <- which(sapply(variables$pe, is.numeric))
          QFeatures::readQFeatures(
            assayData = variables$pe,
            fnames    = "EG.PrecursorId",
            quantCols = parsedQuantCols(),
            name      = "precursors"
          )
        }
        
        # add annotation if available
        if (!is.null(variables$annot)) {
          SummarizedExperiment::colData(pe) <- S4Vectors::DataFrame(variables$annot)
        }
        
        pe
      })
      
      # store in variables
      observe({
        req(qfeatures())
        variables$qfeatures <- qfeatures()
      })
      
      # wirte in csv the run col with sample annotation
      output$printed_annot <- downloadHandler(
        filename = function() "annotation.csv",
        content  = function(file) {
          req(variables$qfeatures)
          
          annot <- data.frame(sampleName = colnames(variables$qfeatures))
          write.csv(annot, file, row.names = FALSE)
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

