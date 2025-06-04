#' inference UI
#'
#' @return A shiny tagList object that contains the inference UI components
#' @rdname INTERNAL_inferenceUI
#' @keywords internal
#'
#' @importFrom dplyr arrange

inferenceUI <- function(id="inference")
{
  fluidPage(
    fluidRow(
      column(width=12,
             h3("Specify contrast"),
             h4("Following parameters can be used in contrasts for hypothesis tests: "),
             h4(htmlOutput(NS(id,"modelParams"))),
             div(
               list(
                 tags$label("Null hypothesis"),
                 #tags$button(id="button_contrast", tags$sup("[?]")),
                 shiny::textInput(NS(id,"contrast"), label=NULL,""),
                 #hidden(
                   helpText(id="tooltip_contrast",
                            "Formulate null hypothesis in terms of a (linear combination) of the model parameters.
                  The name of the model parameters (intercept and slopes) are indicated above the Null Hypothesis Field.
                  They are also included in the Model tab in the plot for Visualize Design.
                  In the Visualize Design plot every group mean of the experimental design is given.
                  A contrast is the difference between group means, which typically is a linear combination of the slope terms in the linear model.
                  Suppose for instance that the model consists of one Design Variable named ‘treatment’, which consist of two levels stimulus A or stimulus B, incoded with a letter ‘A’ or ‘B’, respectively.
                  In R the stimulus A will be the reference class and its group mean will be modelled with the intercept of the linear model: ‘(intercept)’.
                  The group mean for stimulus B will then be modelled using the sum of the intercept and its corresponding slope named ‘treatmentB’: ‘(intercept)+treatmentB’.
                  If the intensities are log2 transformed, the average log2 fold change between stimulus B and stimulus A will then be equalt to the slope: ‘treatmentB’.
                  Assessing if the log2 FC is equal to 0, involves nulhypothesis ‘treatmentB = 0’. Note, that the quotes have to be removed when inputting this in the field Null Hypothesis."
                   )
                 #) #end hidden
               )
             ),

             div(
               list(
                 tags$label("Significance level", `for`="alpha"),
                 #tags$button(id="button_alpha", tags$sup("[?]")),
                 numericInput(NS(id,"alpha"), label=NULL, value=.05, min = 0, max = 1, step = 0.01, width = NULL),
                 #hidden(
                   helpText(id="tooltip_alpha","Select the significance level (alpha) at which the type I error needs to be performed.
     		         Tests are traditionally performed at the 5% false discovery rate (FDR) level, but more stringent control (e.g. 1% FDR or even less) is sometimes adopted in experiments where false positives are highly unwanted (e.g. clinical settings).
     		         The lower this level, the more stringent the cut-off and thus the less proteins that will be declared significant, but the higher the statistical certainty of the differential abundance of these proteins.
     		         An FDR of 5% means that on average an expected 5% of the proteins that are called significant will be in fact false positives."
                   )
                 #) # end hidden
               )
             ),

             div(
               list(
                 tags$label("Only significant features in table", `for`="sigOnly"),
                 #tags$button(id="button_sigOnly", tags$sup("[?]")),
                 checkboxInput(NS(id,"sigOnly"), label=NULL, value=TRUE),
                 #hidden(
                   helpText(id="tooltip_sigOnly",
                            "If sigOnly is checked only the features with an adjusted p-value below the significance level are returned."
                   )
                 #) #end hidden
               )
             )
      #), # End column
      ,
      #column(width=8,
             plotOutput(NS(id,"volcanoPlot"),
                        click = "plotVolcano_click",
                        dblclick = "plotVolcano_dblclick",
                        brush = brushOpts(
                          id = "plotVolcano_brush",
                          resetOnNew = TRUE
                        )
             )
      ) # end Column
    ),

    fluidRow(column(width = 12, h3("Results table"), DT::DTOutput(NS(id,'table')))),
    fluidRow(column(width = 12,
                    h3("Boxplot of fold changes"),
                    div(
                      list(
                        tags$label("Regulation", `for`="regulation"),
                        #tags$button(id="regulation", tags$sup("[?]")),
                        selectInput(NS(id,"regulation"), label=NULL,  choices = c("both", "up", "down")),
                        #hidden(
                        helpText(id="tooltip_regulation","Select fold changes for boxplot (up: upregulated, down: downregulated, both: up & downregulated.)")
                        #) #close hidden
                      )
                    ),
                    plotOutput(NS(id,"boxplotFC"), height = 200)
                    )
             ),
    fluidRow(h3("DetailPlots")),
    fluidRow(column(width=4,
                    htmlOutput(NS(id,"selectedLowLevelAssay")),
                    htmlOutput(NS(id,"selectColDetailPlot2")),
                    htmlOutput(NS(id,"selectHorizontalDetailPlot2")),
                    htmlOutput(NS(id,"selectVerticalDetailPlot2"))
    ),
    column(width=8,
           h4("Select one feature in the volcano plot or in the table of the inference tab to visualize the expression values"),
           uiOutput(NS(id,"detailPlots"))
    )
    )
  )
}

#' Server for inference tab
#'
#' @param id module id
#' @param variables global reactive values object to share objects across modules
#' @param inputServerInput list with reactive values containing the input of the inputServer module
#' @return list of reactive inputs
#' @rdname INTERNAL_inferenceServer
#' @keywords internal
#'
inferenceServer <- function(id="inference", variables, inputServerInput){
  moduleServer(
    id,
    function(input,output,session){
      # extract assayNames
      assayNamesPe <- reactive(names(variables$pe))

      # extract names colData
      plotDependentVars <- reactive({
        if (!is.null(variables$pe))
          as.list(c("none",colnames(colData(variables$pe))))
        })

      output$modelParams <- renderUI({paste(variables$parameterNames,collapse=" \n")})

      # range plots
      ranges <- reactiveValues(x = NULL, y = NULL)

      # generate resultTable for contrast
      dataAll <- reactive({
        output$modelParams <- renderUI({paste(variables$parameterNames,collapse=" \n")})
        contrast <- makeContrast(input$contrast,parameterNames= variables$parameterNames)
        try(variables$pe <- hypothesisTest(variables$pe, i=variables$selectedAssay, contrast, overwrite=TRUE))
        data <- rowData(variables$pe[[variables$selectedAssay]])[,colnames(contrast)] |>  arrange(pval)
        #data <- topFeatures(rowData(variables$pe[[variables$selectedAssay]])$msqrobModels,msqrob2::makeContrast(input$contrast,parameterNames= variables$parameterNames)[,1])
        data$minusLog10Pval <- - log10(data$pval)
        return(data)
        })

      ###boxplotFC
      #reactive(
        output$boxplotFC <- renderPlot(makeBoxplotFC(dataset = data(), sel = input$table_rows_all, regulation= input$regulation))
      #)

      ###Volcanoplot
      output$volcanoPlot <- renderPlot(makeVolcanoPlot(dataAll(),clickInfo,input,ranges))

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
        if(!is.null(ranges$x) && !is.null(ranges$y)){
          clickInfo <- subset(dataAll(), (dataAll()$logFC>ranges$x[1] & dataAll()$logFC<ranges$x[2] & (-log10(dataAll()$pval))>ranges$y[1] & (-log10(dataAll()$pval))<ranges$y[2]))
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

      ####################
      ### Detail Plots
      ####################

      output$selectedLowLevelAssay<- renderUI({
        div(
          list(
            tags$label("Select assays for detail plots", `for`="selectedLowLevelAssay"),
            #tags$button(id="button_selectedLowLevelAssay", tags$sup("[?]")),
            selectInput(
              session$ns("selectedLowLevelAssay"),
              NULL,
              assayNamesPe(),
              selected = variables$selectedLowLevelAssay,
              multiple =TRUE,
              width = '100%'),
            #hidden(
            helpText(id="tooltip_selectedLowLevelAssay","Select the variable by which the boxplots should be colored.")
            #) #close hidden
            )
          )
      })

      output$selectColDetailPlot2 <- renderUI({
        div(
          list(
            tags$label("Color variable", `for`="selColDetailPlot2"),
            #tags$button(id="button_selColDetailPlot2", tags$sup("[?]")),
            selectInput(session$ns("selColDetailPlot2"), label=NULL,  plotDependentVars()),
            #hidden(
            helpText(id="tooltip_selColDetailPlot2","Select the variable by which the boxplots should be colored.")
            #) #close hidden
          )
        )
      })

      output$selectHorizontalDetailPlot2 <- renderUI({
        div(
          list(
            tags$label("Horizontal split", `for`="selHorDetailPlot2"),
            #tags$button(id="button_selHorDetailPlot2", tags$sup("[?]")),
            selectInput(session$ns("selHorDetailPlot2"), label=NULL,  plotDependentVars()),
            #hidden(
            helpText(id="tooltip_selHorDetailPlot2","Select the variable by which the plots should be stratified horizontally. Select \'none\' to show all data in one plot.")
            #) #close hidden
          )
        )
      })

      output$selectVerticalDetailPlot2 <- renderUI({
        div(
          list(
            tags$label("Vertical split", `for`="selVertDetailPlot2"),
            #tags$button(id="button_selVertDetailPlot2", tags$sup("[?]")),
            selectInput(session$ns("selVertDetailPlot2"), label=NULL,  plotDependentVars()),
            #hidden(
            helpText(id="tooltip_selVertDetailPlot2","Select the variable by which the plots should be stratified vertically. Select \'none\' to show all data in one plot.")
            #) #close hidden
          )
        )
      })
      observeEvent(input$selectedLowLevelAssay,
                   {
                     variables$selectedLowLevelAssay <- input$selectedLowLevelAssay
                     }
                   )

      detailPlotHlp <- reactive(
        makeDetailPlots(
          variables$pe,
          clickInfo,
          detailServerInput = input,
          inputServerInput = inputServerInput)
        )

      output$detailPlots <- renderUI(
        lapply(
          1:length(detailPlotHlp()),
          function(j) renderPlot(detailPlotHlp()[[j]])
          )
        )

      # return input for other modules
      return(
        list(
          contrast = reactive(input$contrast),
          alpha = reactive(input$alpha),
          sigOnly = reactive(input$sigOnly),
          selectedLowLevelAssay = reactive(input$selectedLowLevelAssay),
          selColDetailPlot2 = reactive(input$selColDetailPlot2),
          selHorDetailPlot2 = reactive(input$selHorDetailPlot2),
          selVertDetailPlot2 = reactive(input$selVertDetailPlot2)
          )
        )
    })
  }
