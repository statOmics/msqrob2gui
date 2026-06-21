#' module UI
#'
#' @return A shiny tagList object that contains the model UI components
#' @rdname INTERNAL_modelUI
#' @keywords internal
#'

modelUI <- function(id="model")
{
  fluidRow(
    column(width=12,
           h3("Build Model"),
           h4("Following variables can be selected to build the model: "),
           h4(htmlOutput(NS(id,"selectFixed"))),
           div(
             list(
               tags$label("Design formula"),
               #tags$button(id="button_formula", tags$sup("[?]")),
               shiny::textInput(NS(id,"designFormula"), label=NULL,"~1"),
               #hidden(
                 helpText(id="tooltip_formula","
  Models for are specified symbolically. The formula is build using the names of the design variables.
  A typical model has the form ‘ ~ terms’ where ‘terms’ is a series of terms which specifies a
  linear model.
  A terms specification of the form ‘variable1’ will model the preprocessed intensities in function of the design variable ‘variable1’.
  If ‘variable1’ is a continuous variable this will result in a linear model with an intercept and a slope for the variable treatment.
  If ‘variable1’ is a factor variable it will result in a linear model with an intercept for the reference class and slope parameters
  with the interpretation of the average difference between the preprocessed intensity of the current class and the reference class.
  A terms specification of the form ‘variable1 + variable2’ indicates the inclusion of the main effects (terms for all slope terms) for ‘variable1’ and ‘variable2’.
  A specification of the form ‘variable1:variable2’ indicates the set of
  terms obtained by taking the interactions of all terms in ‘variable1’
  with all terms in ‘variable2’, i.e. the effect of ‘variable1’ can be altered according to the value of ‘variable2’.
  The specification ‘variable1*variable’
  indicates the _cross_ of ‘variable1’ and ‘variable2’.  This is the same
  as ‘variable1 + variable2 + variable1:variable2’. "
                          )
               #) #close hidden
             )
             ),
           div(
             list(
               tags$label("Ridge regression for fixed effects?", `for`="doRidge"),
               #tags$button(id="button_doRidge", tags$sup("[?]")),
               radioButtons(NS(id,"doRidge"), label=NULL,c("No"= 0,"Yes" = 1)),
               #hidden(
                 helpText(id="tooltip_doRidge","
               When \"Yes\" is selected the fixed effects are estimated using ridge regression. This shrinks the estimates with low evidence for differential abundance towards zero and improves the performance.
               But, the method is computationally much more demanding.
               The method mainly seems to improve in experiments with multiple groups
               We therefore suggest to switch ridge regression off \"No\". "
                 )
               #) #close hidden
             )
           ),
           div(
             list(
               tags$label("Robust regression for fixed effects?", `for`="doRobust"),
               #tags$button(id="button_doRidge", tags$sup("[?]")),
               radioButtons(NS(id,"doRobust"), label=NULL,c("No"= 0,"Yes" = 1), selected=1),
               #hidden(
               helpText(id="tooltip_doRobust",
                        "When \"Yes\" is selected the fixed effects are estimated using robust regression. This downweights the impact of outliers on the regression."
               )
               #) #close hidden
             )
           ),

           #), # end column
           #column(width=8,
           h3("Visualize Design"),
           uiOutput(NS(id,'visDesignPlot')),
           br(),
           actionButton(inputId=NS(id,"fitModel"), label="Fit Model!", class = "btn-success")
           ) #end column
          )
}

#' Server for model tab
#'
#' @param id module id
#' @param variables global reactive values object to share objects across modules
#' @return list of reactive inputs
#' @rdname INTERNAL_modelServer
#' @keywords internal
#'
#' @importFrom ExploreModelMatrix VisualizeDesign
#' @importFrom msqrob2 msqrob
modelServer <- function(id="model", variables){
  moduleServer(
    id,
    function(input,output,session){

      ## Pass variable names in coldata to UI
      output$selectFixed <- renderUI({
        h4(paste(colnames(colData(variables$qfeatures)),collapse="\n"))
      })

      ## Visualise design
      visDesign <- reactive({
        #If the formula contains a random effect, remove it in order to use VisualizeDesign
        if (any(grepl("\\|",attr(terms(as.formula(input$designFormula)), "term.labels")))){
          modFixed <- as.formula(input$designFormula)
          for (mixedTerms in paste0("(",attr(terms(modFixed), "term.labels")[grepl("\\|", attr(terms(as.formula(modFixed)), "term.labels"))], ")"))
            modFixed <- update(as.formula(modFixed), as.formula(paste("~. -",mixedTerms)))
          out <- VisualizeDesign(colData(getWithColData(variables$qfeatures,variables$selectedAssay)),modFixed)
          } else {
          out <- VisualizeDesign(colData(getWithColData(variables$qfeatures,variables$selectedAssay)),input$designFormula)
        }

        ### Evaluate model
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

      ### Make plots
      output$visDesignPlot <- renderUI({
        lapply(1:length(visDesign()[[2]]),
               function(j){
                 renderPlot(visDesign()[[2]][[j]])
               })
      })

      ### Fit model and return results
      observeEvent(input$fitModel,{
        show_modal_spinner(
          spin = "cube-grid",
          color = "#112446",
          text = "Fitting models..."
        )

        peOut <- variables$qfeatures
        peOut <- try(msqrob(object=peOut,i=variables$selectedAssay, formula=stats::as.formula(input$designFormula),overwrite=TRUE, robust=input$doRobust==1, ridge=input$doRidge==1))

        if (class(peOut)=="QFeatures") {
          variables$qfeatures <- peOut
          if(input$doRidge==1){
            #Intercept is not penalized, this way we get the correct parameter names of the fixed effects
            parameterNames <- paste0("ridge",colnames(visDesign()[[3]]))
            parameterNames <- gsub("ridge(Intercept)", "(Intercept)",parameterNames)
            variables$parameterNames  <- parameterNames
          } else {
            parameterNames <- colnames(visDesign()[[3]])
            variables$parameterNames <- parameterNames
          }
          variables$formula <- input$designFormula
          variables$doRidge <- input$doRidge
        }
        remove_modal_spinner()
      })

      ## Make data matrix
      output$annotationDataMatrix <- DT::renderDT(as.data.frame(colData(variables$qfeatures)))

      ## Return input variables for other modules
      return(
        list(
          designFormula = reactive(input$designFormula),
          doRidge = reactive(input$doRidge),
          doRobust = reactive(input$doRobust)
          )
        )
      })
}

