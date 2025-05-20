#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#' @import tidyverse shiny shinymeta rmarkdown knitr msqrob2 grDevices limma graphics ggplot2 ExploreModelMatrix openxlsx reshape2


# Define server 
msqrob2Server <- function(input, output, session) {
shinyjs::useShinyjs()
variables <- reactiveValues(pe=NULL, selectedAssay=NULL, selectedLowLevelAssay=NULL)

  ###########################################
  #Input Tab
  ###########################################



  ########################################################
  #Clear datapaths of backslashes (Needed on Windows only)
  ########################################################

  peDatapath <- metaReactive({getDataPath(..(input$pe$datapath))})

  observeEvent({input$pe},{
    peOut <- try(readRDS(file=peDatapath()))
    if (class(peOut)[1]!="QFeatures") {
      showNotification("Upload proper QFeatures RDS file",id="noProperRDSFile",type="error",duration=NULL,closeButton=FALSE)
    } else {
      removeNotification(id="noProperRDSFile")}
    variables$pe <- peOut
  })

  plotDependentVars <- reactive({
    if (!is.null(variables$pe))
      as.list(c("none",colnames(colData(variables$pe))))
  })
  
  
  assayNamesPe <- reactive(names(variables$pe))
  output$selectAssay<- renderUI({
    selectInput("selectedAssay", NULL, assayNamesPe(), selected = variables$selectedAssay, width = '100%')})
  
  observeEvent(input$selectedAssay,
               {
                 variables$selectedAssay <- input$selectedAssay
               })
  
  
  ########################################
  #Generate options for fixed effect variables
  ########################################

  colClasses <- reactive({
    if(isTRUE(input$asis_numeric)){
      colClasses <- "keep"
    } else{colClasses <- "factor"}
    return(colClasses)
  })


observe({
  shinyjs::onclick("button_project_name",
                   shinyjs::toggle(id = "tooltip_project_name", anim = TRUE))

  shinyjs::onclick("button_formula",
                   shinyjs::toggle(id = "tooltip_formula", anim = TRUE))
  shinyjs::onclick("button_doRidge",
                   shinyjs::toggle(id = "tooltip_doRidge", anim = TRUE))
  shinyjs::onclick("button_contrast",
                   shinyjs::toggle(id = "tooltip_contrast", anim = TRUE))
  shinyjs::onclick("button_alpha",
                   shinyjs::toggle(id = "tooltip_alpha", anim = TRUE))
  shinyjs::onclick("button_sigOnly",
                   shinyjs::toggle(id = "tooltip_sigOnly", anim = TRUE))
  shinyjs::onclick("button_selColPlotProt",
                   shinyjs::toggle(id = "tooltip_selColPlotProt", anim = TRUE))

  })



  #######################
  #Model Tab
  #######################
  

  output$selectFixed <- renderUI({
      h4(paste(colnames(colData(variables$pe)),collapse="\n"))
      })

  
  
  visDesign <- reactive({

      #If the formula contains a random effect, remove it in order to use VisualizeDesign
      if (any(grepl("\\|",attr(terms(as.formula(input$designformula)), "term.labels")))){
        out <- VisualizeDesign(colData(variables$pe),update(as.formula(input$designformula), as.formula(paste("~. -",paste0("(",attr(terms(as.formula(input$designformula)), "term.labels")[grepl("\\|", attr(terms(as.formula(input$designformula)), "term.labels"))], ")")))))
        #input$designformula <- update(input$designformula, as.formula(paste("~. -",paste0("(",attr(terms(designformula), "term.labels")[grepl("\\|", attr(terms(designformula), "term.labels"))], ")"))))
      } else {
        out <- VisualizeDesign(colData(variables$pe),input$designformula)
      }

      rank <- qr(out$designmatrix)$rank
      if (rank==nrow(out$designmatrix)) {
        showNotification(paste0("The model is overparameterized. ",
                        "The residual degrees of freedom is 0. ",
                        "Variances and standard errors can not be ",
                        "estimated from data with this design."),
                        type="error",
                        closeButton=FALSE,
                        duration=NULL,
                        id="modError"
                        )
        } else {
          removeNotification(id="modError")
        }
      if (rank<ncol(out$designmatrix)) {
          showNotification(paste0("The model is poorly parametrised. ",
                          "Not all model parameters can be estimated ",
                          "and the model will not be fitted."
                          ),
                          type="error",
                          closeButton=FALSE,
                          duration=NULL,
                          id="modError2"
                          )
          } else {
            removeNotification(id="modError2")
          }
      return(out)
      })
      output$fitted_values_plot <- renderUI({
          lapply(1:length(visDesign()[[2]]),
                 function(j){
                 renderPlot(visDesign()[[2]][[j]])
                 })
        })
      observeEvent(input$fitModel,{
          show_modal_spinner(
            spin = "cube-grid",
            color = "#112446",
            text = "Fitting models..."
            )

          peOut <- variables$pe
          peOut <- try(msqrob(object=peOut,i=variables$selectedAssay, formula=stats::as.formula(input$designformula),overwrite=TRUE, ridge=input$doRidge==1))

          if (class(peOut)=="QFeatures") {
          variables$pe <- peOut
          }
          remove_modal_spinner()
      })
      output$annotationDataMatrix <- DT::renderDT(as.data.frame(colData(variables$pe)))

  ###########
  #Inference tab
  ###########
  ranges <- reactiveValues(x = NULL, y = NULL)



  dataAll <- reactive({
    if(input$doRidge==1 ){
      #Intercept is not penalized, this way we get the correct parameter names of the fixed effects
      parameter_names <- paste0("ridge",colnames(visDesign()[[3]]))
      parameter_names <- gsub("ridge(Intercept)", "(Intercept)",parameter_names)
      output$modelParams <- renderUI({paste(parameter_names,collapse=" \n")})
    } else {
      parameter_names <- colnames(visDesign()[[3]])
      output$modelParams <- renderUI({paste(parameter_names,collapse=" \n")})
    }

    data <- topFeatures(rowData(variables$pe[[variables$selectedAssay]])$msqrobModels,msqrob2::makeContrast(input$contrast,parameterNames= parameter_names)[,1])
      data$minusLog10Pval <- - log10(data$pval)
      return(data)
      }
  )

  ###boxplotFC
  makeBoxplotFC<-function(dataset)
  {
    s = input$table_rows_all
    boxplot(dataset()[s,"logFC"], xlab="logFC",horizontal=TRUE,ylim=range(dataset()[["logFC"]],na.rm=TRUE))
  }

  output$boxplotFC <- renderPlot(makeBoxplotFC(data))

  ###Volcanoplot
  output$volcanoPlot <- renderPlot(
      makeVolcanoPlot(dataAll(),clickInfo,input,ranges)
  )




    #When a double-click happens, check if there's a brush on the plot.
    #If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plotVolcano_dblclick, {
      brush <- input$plotVolcano_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }

      #Set selection to zero: happens already if ranges change, but should also happen on normal double click
      proxy %>% DT::selectRows(NULL)

    })

    #for zooming
    clickInfo <- reactive({
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
      if(!is.null(ranges$x) && !is.null(ranges$y)){clickInfo <- subset(dataAll(), (dataAll()$logFC>ranges$x[1] & dataAll()$logFC<ranges$x[2] & (-log10(dataAll()$pval))>ranges$y[1] & (-log10(dataAll()$pval))<ranges$y[2]))
      } else if(is.null(ranges$x) && is.null(ranges$y)){clickInfo <- dataAll()}
      return(clickInfo)
    })

    data <- reactive({
        data <- clickInfo()
        if (input$sigOnly){
                          return(na.exclude(data[data$adjPval<input$alpha,]))
                          } else {
                          return(data)
                          }
   })

output$table<-DT::renderDT(
  formatSignif(datatable(data()[,1:6]),columns=1:6,digits=3)
  )

#Set table Proxy so as to reduce the table according to the zoom in the plot and to highlight points
proxy <- dataTableProxy('table')

observeEvent(input$plotVolcano_click, {

  selected <- nearPoints(clickInfo(), input$plotVolcano_click, addDist = TRUE,maxpoints=1, xvar="logFC", yvar="minusLog10Pval")
  sel_rows <- which(rownames(clickInfo()) %in% rownames(selected))
  #Rows which were selected and selected again are removed, rows which were already selected but not selected again are retained
  #Don't sort this! Otherwise reacalculated.
  new_rows <- c(sel_rows[!sel_rows%in%input$table_rows_selected], input$table_rows_selected[!input$table_rows_selected%in%sel_rows])

  proxy %>% DT::selectRows(new_rows)
})

#Enable or disable add brush to selection and remove brush from selection buttons
observe({
  if (is.null(input$plotVolcano_brush)) {
    shinyjs::disable("add_area_selection")
    shinyjs::disable("remove_area_selection")
  } else {
    shinyjs::enable("add_area_selection")
    shinyjs::enable("remove_area_selection")
  }
})

observeEvent(input$add_area_selection, {

  selected <- brushedPoints(clickInfo(), input$plotVolcano_brush, xvar="logFC", yvar="minusLog10Pval")
  sel_rows <- which(rownames(clickInfo) %in% rownames(selected))
  #Rows which were selected and selected again are removed, rows which were already selected but not selected again are retained
  #Don't sort this! Otherwise reacalculated.
  new_rows <- unique(c(input$table_rows_selected,sel_rows))

  proxy %>% DT::selectRows(new_rows)
})

observeEvent(input$remove_area_selection, {

  selected <- brushedPoints(clickInfo(), input$plot1_brush, xvar="logFC", yvar="minusLog10Pval")
  sel_rows <- which(rownames(clickInfo) %in% rownames(selected))
  #Rows which were selected and selected again are removed, rows which were already selected but not selected again are retained
  #Don't sort this! Otherwise reacalculated.
  new_rows <- input$table_rows_selected[!(input$table_rows_selected %in% sel_rows)]

  proxy %>% DT::selectRows(new_rows)
})

observeEvent(input$remove_all_selection, {
  proxy %>% DT::selectRows(NULL)
})



#########DetailPlots 

output$selectedLowLevelAssay<- renderUI({
  selectInput("selectedLowLevelAssay", NULL, assayNamesPe(), selected = variables$selectedLowLevelAssay, multiple =TRUE, width = '100%')})

observeEvent(input$selectedLowLevelAssay,
             {
               variables$selectedLowLevelAssay <- input$selectedLowLevelAssay
             })


  detailPlotHlp <- reactive(
      makeDetailPlots(variables$pe,clickInfo,input)
      )

  output$detailPlots <- renderUI(
      lapply(1:length(detailPlotHlp()),function(j) renderPlot(detailPlotHlp()[[j]]))
  )

  output$selectColDetailPlot2 <- renderUI({
    div(
        list(
          tags$label("Color variable", `for`="selColDetailPlot2"),
          tags$button(id="button_selColDetailPlot2", tags$sup("[?]")),
          selectInput("selColDetailPlot2", label=NULL,  plotDependentVars()),
          hidden(helpText(id="tooltip_selColDetailPlot2","Select the variable by which the boxplots should be colored."))
        )
    )
  })

  output$selectHorizontalDetailPlot2 <- renderUI({
    div(
        list(
          tags$label("Horizontal split", `for`="selHorDetailPlot2"),
          tags$button(id="button_selHorDetailPlot2", tags$sup("[?]")),
          selectInput("selHorDetailPlot2", label=NULL,  plotDependentVars()),
          hidden(helpText(id="tooltip_selHorDetailPlot2","Select the variable by which the plots should be stratified horizontally. Select \'none\' to show all data in one plot."))
        )
    )
  })

  output$selectVerticalDetailPlot2 <- renderUI({
      div(
          list(
            tags$label("Vertical split", `for`="selVertDetailPlot2"),
            tags$button(id="button_selVertDetailPlot2", tags$sup("[?]")),
            selectInput("selVertDetailPlot2", label=NULL,  plotDependentVars()),
            hidden(helpText(id="tooltip_selVertDetailPlot2","Select the variable by which the plots should be stratified vertically. Select \'none\' to show all data in one plot."))
          )
      )
    })

    observe({
      shinyjs::onclick("button_selColDetailPlot2",
                     shinyjs::toggle(id = "tooltip_selColDetailPlot2", anim = TRUE))
      shinyjs::onclick("button_selHorDetailPlot2",
                     shinyjs::toggle(id = "tooltip_selHorDetailPlot2", anim = TRUE))
      shinyjs::onclick("button_selVertDetailPlot2",
                     shinyjs::toggle(id = "tooltip_selVertDetailPlot2", anim = TRUE))
    })

### use metaReactive to write options to file
 
     selectedAssay <- metaReactive({..(input$selectedAssay)}, varname = "selectedAssay")
     form <- metaReactive({..(input$designformula)}, varname = "form")
     doRidge <- metaReactive({..(input$doRidge==1)}, varname = "doRidge")
     contrast <- metaReactive({..(input$contrast)}, , varname = "contrast")
     sigLevel <- metaReactive({..(input$alpha)}, varname = "sigLevel")
     maxPlot <- metaReactive({..(input$maxPlot)}, varname = "maxPlot")
     selectedLowLevelAssay <- metaReactive({..(input$selectedLowLevelAssay)}, varname = "selectedLowLevelAssay")
     selHorPlot <- metaReactive({..(input$selHorDetailPlot2)}, varname = "selHorPlot")
     selVertPlot <- metaReactive({..(input$selVertDetailPlot2)}, varname ="selVertPlot")
     selColPlot <- metaReactive({..(input$selColDetailPlot2)}, varname = "selColPlot")

     output$report <- downloadHandler(
       filename = function() {
         paste0(input$project_name,"-report-", gsub(" |:","-",Sys.time()),".zip")
       },
       content = function(file) {
         file.copy(from = peDatapath(), to = "qfeaturesFile.rds", overwrite = TRUE)
         input <- expandChain(
           quote({
                 qfeaturesFile <- "qfeaturesFile.rds"
                 }))
         model <- expandChain(
           invisible(selectedAssay()),
           invisible(form()),
           invisible(doRidge())
           )
         inference <- expandChain(
           invisible(contrast()),
           invisible(sigLevel()))
         report <- expandChain(
           invisible(maxPlot()),
           invisible(selectedLowLevelAssay()),
           invisible(selHorPlot()), 
           invisible(selVertPlot()),
           invisible(selColPlot())
           )
         buildRmdBundle(
           system.file("data/report.Rmd",package="msqrob2gui"),
           file,
           list(
                       input = input,
                       model = model,
                       inference = inference,
                       report = report
                       ),
           render=FALSE,
           include_files = c("qfeaturesFile.rds")
         )
       })

  #Stop the App when closing the browser or ending the session
  session$onSessionEnded(stopApp)
}
