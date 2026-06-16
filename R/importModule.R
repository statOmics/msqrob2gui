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
#' @importFrom shiny dataTableOutput


importUI <- function(id="import")
{
  fluidRow(
    column(width=12,
           div(
             list(
               tags$label("Software", `for`="software"),
               selectInput(NS(id, "software"), label = NULL,
                           choices = c("DIA-NN"      = "diann",
                                       "Spectronaut" = "spectronaut",
                                       "Max-Quant" = "maxquant",
                                       "Other" = "other"),
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
               tags$label("Input file preview"),
               DT::dataTableOutput(NS(id, "pePreview"),)
             )
           ),
           uiOutput(NS(id, "columnSelectors")),
           uiOutput(NS(id, "nameInput")),
           div(
             list(
               tags$label("QFeatures", `for`="buildQFeatures"),
               actionButton(NS(id, "buildQFeatures"),"Build the QFeatures object"),
               helpText(id = "tooltip_buildQFeatures",
                        "The button generate a QFeatures object.")
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
               actionButton(NS(id, "addAnnot"), "Add annotation"),
               helpText(id = "tooltip_annotation",
                        "If available, upload the annotation file and click 'Add annotation'. The first column must match the samples' names of the QFeatures object.")
             )
           ),
           div(
             list(
               tags$label("Annotation file preview"),
               DT::dataTableOutput(NS(id, "annotPreview"),)
             )
           ),
           div(
             list(
               tags$label("QFeatures summary"),
               verbatimTextOutput(NS(id, "qfeaturesSummary"))
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
          peOut <- try(data.table::fread(file=peDatapath(), check.names = TRUE, integer64 = "double"))

          if (inherits(peOut, "try-error")){
            showNotification("Upload proper input file",id="noProperFile",type="error",duration=NULL,closeButton=FALSE)
          }

        } else {
          removeNotification(id="File Not Valid")}
        variables$pe <- peOut
        variables$rawFilePath <- input$pe$datapath
        variables$rawFileName <- input$pe$name
      })
      
      
      
      # select columns
      output$columnSelectors <- renderUI({
        req(variables$pe)
        
        cols <- colnames(variables$pe)
        isLong <- input$software %in% c("diann", "spectronaut")
        
        cfg <- switch(input$software,
                      diann = list(
                        fnames   = "Precursor.Id",
                        runCol   = "Run",
                        quantCol = "Precursor.Quantity"
                      ),
                      spectronaut = list(
                        fnames   = "EG_PrecursorId",
                        runCol   = "R_FileName",
                        quantCol = "FG_MS2RawQuantity"
                      ),
                      maxquant = list(
                        fnames   = "Sequence",
                        runCol   = NULL,
                        quantCol = grep("Intensity ", cols, value = TRUE)
                      ),
                      other = list(
                        fnames   = NULL,
                        runCol   = NULL,
                        quantCol = NULL
                      )
        )
        
        if (isLong) {
          list(
            tags$label("Column selection"),
            selectizeInput(NS(id, "fnames"),   "Feature ID column",
                           choices = cols, selected = cfg$fnames),
            selectizeInput(NS(id, "runCol"),   "Run column",
                           choices = cols, selected = cfg$runCol),
            selectizeInput(NS(id, "quantCol"), "Intensity column",
                           choices = cols, selected = cfg$quantCol)
          )
        } else {
          list(
            tags$label("Column selection"),
            selectizeInput(NS(id, "fnames"),    "Feature ID column",
                           choices = cols, selected = cfg$fnames),
            selectizeInput(NS(id, "runCol"),   "Run column",
                           choices = cols, selected = cfg$runCol),
            selectizeInput(NS(id, "quantCols"), "Intensity columns",
                           choices = cols, selected = cfg$quantCol,
                           multiple = TRUE)
          )
        }
      })
      
      # preview raw input file
      output$pePreview <- DT::renderDataTable({
        req(variables$pe)
      
        DT::datatable(
          head(variables$pe, 5),  
          options = list(scrollX = TRUE)
        )
      })
      
      
      #name input summarized experiment, only showing for maxquant and other
      output$nameInput <- renderUI({
        if (input$software %in% c("maxquant", "other")) {
          div(list(
            tags$label("Name summarized experiment"),
            textInput(NS(id, "name"), label = NULL, value = "precursors"),
            helpText("Assign a name to the summarized experiment.")
          ))
        }
      })
      
      
      # build qfeatures
      qfeatures <- eventReactive(input$buildQFeatures, {
        req(variables$pe)
        req(input$fnames)
        
        
        isLong <- input$software %in% c("diann", "spectronaut")
        
        pe <- if (isLong) {
          req(input$runCol)
          req(input$quantCol)
          QFeatures::readQFeatures(
            assayData = variables$pe,
            fnames    = input$fnames,
            runCol    = input$runCol,
            quantCol  = input$quantCol,
            name      = input$name
          )
        } else {
          req(input$quantCols)
          QFeatures::readQFeatures(
            assayData = variables$pe,
            fnames    = input$fnames,
            quantCols = which(colnames(variables$pe) %in% input$quantCols),
            name      = input$name
          )
        }
        
        pe
       
      })
      
      # store in variables
      observe({
        req(qfeatures())
        variables$qfeatures <- qfeatures()
        variables$software <- input$software
      })
      
      #extract datapath of annotation file
      annotDatapath <- metaReactive({getDataPath(..(input$annot$datapath))})
      
      #import the annotation file
      observeEvent({input$annot},{
        annotOut <- try(data.table::fread(file=annotDatapath(),check.names = TRUE, integer64 = "double"))
        if (inherits(annotOut, "try-error")) {
          showNotification("Upload proper annotation file",id="noProperAnnotFile",type="error",duration=NULL,closeButton=FALSE)
        }
        else {
          removeNotification(id="File not valid. Rownames must match the sample names of the QFeatures object")}
        variables$annot <- annotOut
        variables$annotFilePath <- input$annot$datapath
        variables$annotFileName <- input$annot$name
      }
      )
      
      
      observe({
        req(variables$qfeatures)
        
        variables$annot_tmp <- data.frame(
        sampleName = rownames(colData(variables$qfeatures)),
        row.names  = rownames(colData(variables$qfeatures))
      )}
      )
      
      # write in csv the runCol with sample annotation
      output$printed_annot <- downloadHandler(
        filename = "annotation.csv",
        content  = function(file) {
          
          write.csv(variables$annot_tmp, file)  
          
        },
        contentType = "text/csv"
      )
      
      # add annotation table to the QFeatures
      observeEvent(input$addAnnot, {
        req(variables$qfeatures)
        req(variables$annot)
        
        annot <- as.data.frame(variables$annot)
        rownames(annot) <- annot[,1]
        
        qf <- variables$qfeatures
        sampleNames <- rownames(colData(qf) ) # might be S4 — check below
        
        annot <- annot[sampleNames, , drop = FALSE]
        
        SummarizedExperiment::colData(qf) <- annot
        
        variables$qfeatures <- qf
        
        showNotification("Annotation added successfully", type = "message")
      })
      
      # preview annotation input file
      output$annotPreview <- DT::renderDataTable({
        req(variables$qfeatures)
        req(variables$annot)
        
        DT::datatable(
          head(variables$annot, 5),  
          options = list(scrollX = TRUE)
        )
      })
      
      
      
      output$qfeaturesSummary <- renderPrint({
        req(variables$qfeatures)
        variables$qfeatures
      })
      
      return(
        list(
          qfeatures = reactive(variables$qfeatures),
          software = reactive(variables$software)
      )

      )
    }
  )
}

