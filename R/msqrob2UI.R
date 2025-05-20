#' Shiny app server object
#' @import shiny shinymeta DT shinyjs shinythemes shinybusy

# create the shiny application user interface
msqrob2UI <- fluidPage(theme = shinytheme("spacelab"),
    shinyjs::useShinyjs(),

############################################################################
#Navigation bar with 4 panel:Input, preprocessing, summarisation, quantification
############################################################################
   navbarPage("msqrob2 Shiny App", inverse=TRUE,


    ####################################
    #input tab
    ####################################

    tabPanel('Input',
      sidebarLayout(
  	   sidebarPanel(
  	    h3("Settings"),

        div(
  	      list(tags$label("Project name", `for`="project_name"),
  	           tags$button(id="button_project_name", tags$sup("[?]")),
  	           textInput("project_name", NULL, value = "project", width = '100%', placeholder = NULL),
  	           hidden(helpText(id="tooltip_project_name",
  	           "Give your project a meaningful name.
  	            This name will be given to your results files.
  	            A time stamp will be automatically appended to name."))
  	           )
        ),


  	    #PE
  	    div(
  	        list(tags$label("QFeatures input RDS file", `for`="pe"),
  	             tags$button(id="button_features", tags$sup("[?]")),
  	             fileInput(inputId="pe", label=NULL, multiple = FALSE, accept = NULL, width = NULL),
  	             hidden(helpText(id="tooltip_features","Specify the location of the RDS file that contains
  	                             your qfeatures object.
  	                               "))
  	            )
  	        ),

  	   ),

		   #Main panel with number of output and plots
       mainPanel(width = 5,
                 div(
                   list(
                     tags$label("Select Assay", `for`="selectAssay"),
                     tags$button(id="button_selectAssay", tags$sup("[?]")),
                     htmlOutput("selectAssay"),               
                     hidden(
                       helpText(id="tooltip_select_assay",
                                "Select the QFeatures assay for the differential analysis.
        	                      ")
                     )
                   )
                 ),
            h3("Frequently asked questions"),
            htmlOutput("folderError"),

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
            )
        )
    )



    ###########################
    #Build Model
    ###########################
    , tabPanel("Model",
      sidebarLayout(
       sidebarPanel(
           h3("Build Model"),
           h4("Following variables can be selected to build the model: "),
  	       h4(htmlOutput("selectFixed")),
           div(
            list(
              tags$label("Design formula"),
              tags$button(id="button_formula", tags$sup("[?]")),
              shiny::textInput("designformula", label=NULL,"~1"),
              hidden(
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
                )
              )
            ),
           div(
            list(
              tags$label("Ridge regression for fixed effects?", `for`="doRidge"),
              tags$button(id="button_doRidge", tags$sup("[?]")),
              radioButtons("doRidge", label=NULL,c("No"=0,"Yes" = 1)),
              hidden(
                helpText(id="tooltip_doRidge","
                  When \"Yes\" is selected the fixed effects are estimated using ridge regression. This shrinks the estimates with low evidence for differential abundance towards zero and improves the performance.
                  But, the method is computationally much more demanding.
                  The method mainly seems to improve in experiments with multiple groups
                  We therefore suggest to switch ridge regression off \"No\". "
                  )
                )
              )
            ),

          actionButton(inputId="fitModel", label="Fit Model!")
          ),

        mainPanel(
          fluidRow(column(width = 12, h3("Design Variables"),DT::DTOutput('annotationDataMatrix'))),
          h3("Visualize Design"),
          uiOutput('fitted_values_plot')
          )
        )
      )

    #############
    #Inference panel
    #############
    ,tabPanel("Inference",
      sidebarLayout(
        sidebarPanel(
          h3("Specify contrast"),
          h4("Following parameters can be used in contrasts for hypothesis tests: "),
          h4(htmlOutput("modelParams")),
          div(
            list(
              tags$label("Null hypothesis"),
              tags$button(id="button_contrast", tags$sup("[?]")),
              shiny::textInput("contrast", label=NULL,""),
              hidden(
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
                )
              )
            ),

          div(
            list(
              tags$label("Significance level", `for`="alpha"),
     		      tags$button(id="button_alpha", tags$sup("[?]")),
     		      numericInput("alpha", label=NULL, value=.05, min = 0, max = 1, step = 0.01, width = NULL),
     		      hidden(
                helpText(id="tooltip_alpha","Select the significance level (alpha) at which the type I error needs to be performed.
     		         Tests are traditionally performed at the 5% false discovery rate (FDR) level, but more stringent control (e.g. 1% FDR or even less) is sometimes adopted in experiments where false positives are highly unwanted (e.g. clinical settings).
     		         The lower this level, the more stringent the cut-off and thus the less proteins that will be declared significant, but the higher the statistical certainty of the differential abundance of these proteins.
     		         An FDR of 5% means that on average an expected 5% of the proteins that are called significant will be in fact false positives."
                 )
                )
              )
            ),

          div(
            list(
              tags$label("Only significant features in table", `for`="sigOnly"),
      		    tags$button(id="button_sigOnly", tags$sup("[?]")),
      		    checkboxInput("sigOnly", label=NULL, value=TRUE),
      		    hidden(
                helpText(id="tooltip_sigOnly",
                  "If sigOnly is checked only the features with an adjusted p-value below the significance level are returned."
                  )
                )
      		    )
      		   )
          ),

      mainPanel(
          plotOutput("volcanoPlot",
            click = "plotVolcano_click",
            dblclick = "plotVolcano_dblclick",
            brush = brushOpts(
              id = "plotVolcano_brush",
              resetOnNew = TRUE
              )
            ),

          fluidRow(column(width = 12, h3("Results table"),DT::DTOutput('table'))),
          fluidRow(column(width = 12, plotOutput("boxplotFC", height = 200)))
        )
      )
    )
    ###############
    #Detail plots
    ###############
    
    , tabPanel("DetailPlots",
              sidebarLayout(
                sidebarPanel(
                  htmlOutput("selectedLowLevelAssay"),
                  htmlOutput("selectColDetailPlot2"),
                  htmlOutput("selectHorizontalDetailPlot2"),
                  htmlOutput("selectVerticalDetailPlot2")
                ),
                
                mainPanel(
                  h4("Select one feature in the volcano plot or in the table of the inference tab to visualize the expression values"),
                  uiOutput("detailPlots")
                )
              )
    ),
    tabPanel("Report",
             sidebarLayout(
               sidebarPanel(
                 div(
                   list(
                     tags$label("Number of significant features for which you want to have detail plots", `for`="maxPlot"),
                     tags$button(id="button_maxPlot", tags$sup("[?]")),
                     numericInput("maxPlot", label=NULL, value=10, min = 1, max = NA, step = 1, width = '100%'),
                     hidden(
                       helpText(id="tooltip_maxPlot","Number of significant features for which you want to have detail plots in the generated report
      	                  ")
                     )
                   )),
                 #          div(
                 #            list(
                 #              checkboxInput("Render report", label="render", value=TRUE),
                 #              tags$button(id="button_render", tags$sup("[?]")),
                 #              hidden(
                 #                helpText(id="tooltip_render","If you select render, a reproducible markdown script and rendered html report will be generated. Note, that this can take a while because all data analysis steps have to be executed again. If unselected, only an Rmarkdown file is generated that can be rendered in rstudio at any time.  
                 #      	                  ")
                 #              )
                 #            )),
                 downloadButton("report", "Generate report")
               ),
               mainPanel()
             )
    )
    #close navbar, page, etc.
  )
)
