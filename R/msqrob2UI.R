#' Shiny app server object
#' @export
#' @import shiny DT shinyjs shinythemes

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

        div(
        	    list(
        	         tags$label("Input type", `for`="input_type"),
        	         tags$button(id="button_input_type", tags$sup("[?]")),
        	         selectInput("input_type", NULL, c("MaxQuant", "moFF", "mzTab"), width = '100%'),
        	         hidden(
                      helpText(id="tooltip_input_type",
        	                      "Select the type of input.
        	                      ")
                      )
        	         )
        	     ),

  	    #Peptides.txt files
  	    div(
  	        list(tags$label("Peptides file", `for`="peptides"),
  	             tags$button(id="button_features", tags$sup("[?]")),
  	             fileInput(inputId="peptides", label=NULL, multiple = FALSE, accept = NULL, width = NULL),
  	             hidden(helpText(id="tooltip_features","Specify the location of the file that contains
  	                             the peptide-specific intensities.
  	                             When analyzing a MaxQuant shotgun proteomics experiment, this should the peptides.txt file.
  	                             When using moFF, this file should start with \"peptide_summary_intensity\" and end with \".tab\".
			                           When using mzTab, this file should be a tab-delimited file with data summarized at the peptide level (\".tsv\" output file).
  	                             When using Progenesis, this should be a \".csv\" file with data summarized at the peptide level.
			                           "))
  	            )
  	        ),

  	    #Annotation file
  	    div(
  	        list(tags$label("Annotation file", `for`="annotation"),
  	             tags$button(id="button_annotation", tags$sup("[?]")),
		             fileInput(inputId="annotation", label=NULL, multiple = FALSE, accept = NULL, width = NULL),
		             hidden(helpText(id="tooltip_annotation","Specify the location of your experimental annotation file."))
		            )
		       )
  	   ),

		   #Main panel with number of output and plots
       mainPanel(width = 5,
            h3("Frequently asked questions"),
            htmlOutput("folderError"),
            div(
              list(
                h4("What is an annotation file?"),
                tags$button(id="button_newExpAnnText",tags$sup("[?]")),
                actionButton(inputId="goAnnotation", label="Generate Annotation File!"),
                htmlOutput("downloadButtonDownloadAnnot"),
                hidden(helpText(id="tooltip_newExpAnnText",
                  "An experimental annotation file contains the description of your experiment.
                  Indeed, each mass spec run corresponds to e.g. a certain treatment, biological repeat, etc.
                  This should be told to MSqRob via an Excel file or a tab delimited file wherein the first column contains all run names
                  and the other columns contain all predictors of interest.
                  Examples of experimental annotation files for the Francisella and CPTAC experiments can be found ",
                  a("here", href="https://github.com/statOmics/MSqRobData/blob/master/inst/extdata/Francisella/label-free_Francisella_annotation.xlsx"),
                  "and",
                  a("here.", href="https://github.com/statOmics/MSqRobData/blob/master/inst/extdata/CPTAC/label-free_CPTAC_annotation.xlsx"),
                  "Click the button to initialize an Excel file with a \"run\" column (works only if peptides.txt is already uploaded!).
                  The annotation file will be saved in the output location.
                  You still need to add other relevant columns (treatments, biological repeats, technical repeat, etc.) manually!"))
                )
              ),

            div(
            list(
            h4("How do I cite MSqRob?"),
            tags$button(id="button_cite",tags$sup("[?]"))
            )
            ),
            hidden(helpText(id="tooltip_cite",
            "MSqRob is free for you to use and completely open source.
            When making use of MSqRob, we would appreciate it if you could cite our two published articles.",
            br(),
            span("(1) The MSqRob algorithm: ", class="bold"),
            br(),
            "
            Goeminne, L. J. E., Gevaert, K., and Clement, L. (2016) Peptide-level Robust Ridge Regression Improves Estimation, Sensitivity,
            and Specificity in Data-dependent Quantitative Label-free Shotgun Proteomics. Molecular & Cellular Proteomics 15(2), pp 657-668.",
            br(),
            span("(2) The MSqRob GUI tutorial article:", class="bold"),
            br(),
            "
            Goeminne, L. J. E., Gevaert, K. and Clement, L. (2017).
            Experimental design and data-analysis in label-free quantitative LC/MS proteomics:
            A tutorial with MSqRob. Journal of Proteomics (in press).")
            )
            )
        )
    )



    ############################
    #Preprocessing tab
    ###########################
    ,tabPanel('Preprocessing',
        sidebarLayout(
          sidebarPanel(

            h3("Settings"),

            div(
                list(
                    tags$label("Group by", `for`="proteins"),
                    tags$button(id="button_proteins", tags$sup("[?]")),
                    htmlOutput("selectProteins"),
                    hidden(helpText(id="tooltip_proteins","
                              Select the level on which the data should be grouped.
                              This is mostly the column that contains the protein identifiers (\"Proteins\" for MaxQuant data), as for a traditional shotgun experiment, one is mostly interested in which proteins are differentially abundant.
                              However, sometimes, one would for example like to do inference on the peptides.
                              In these more advanced cases, select the appropriate grouping level.
                              ")
                          )
                    )
              ),

            h4("Transformation", class=c("MSqRob_sidebar")),

            div(
                list(
                     checkboxInput("logtransform", label="Log-transform data", value=TRUE),
                     tags$button(id="button_logtransform", tags$sup("[?]")),
                     hidden(
                            helpText(id="tooltip_logtransform",
                                 "Leave this box ticked to log-transform the data.
                                 Log-transformation is almost always performed to make the data less skewed.
                                 Only when the data has already been log-transformed, this box can be unticked."
                                 )
                            )
                    )
               ),

            h4("Filtering"),

            div(
                list(checkboxInput("smallestUniqueGroups", "Remove compromising protein groups", value=TRUE),
                        tags$button(id="button_smallestUniqueGroups", tags$sup("[?]")),
                        hidden(helpText(id="tooltip_smallestUniqueGroups",
                          "Remove protein groups for which any of its member proteins is present in a smaller protein group.
                          This might be done to remove any overlap of proteins in different protein groups.
                          ")
                        )
                )
            ),

            #Filter on features number of occurances
            div(
                list(
                  tags$label("Minimum number of features", `for`="minIdentified"),
                  tags$button(id="button_minIdentified", tags$sup("[?]")),
      	          numericInput("minIdentified", label=NULL, value=2, min = 1, max = NA, step = 1, width = '100%'),
      	          hidden(
                    helpText(id="tooltip_minIdentified","
      	                The minimal number of times a feature sequence should be identified over all samples.
      	                feature sequences that are identified less than this number will be removed from the dataset.
      	                The default of 2 has the rationale that it is impossible to discern between the feature-specific effect and
      	                any other effects for a feature that has only been identified once.
      	                ")
                    )
                  )
            ),

            div(
              list(
                tags$label("Filter columns", `for`="filter"),
                tags$button(id="button_filter", tags$sup("[?]")),
                htmlOutput("selectFilters"),
                hidden(
                  helpText(id="tooltip_filter","
                  Indicate the columns on which filtering should be done.
                  Features for which a \"+\" is present in these columns will be removed from the dataset.
                  This kind of filtering is typically done for common contaminants (e.g. operator's keratin)
                  and reversed sequences from the identificiation step that are still present in the data.
                  "))
              )
            ),

            h4("Normalization"),

            div(
              list(
                tags$label("Normalization", `for`="normalisation"),
                tags$button(id="button_normalisation", tags$sup("[?]")),
                htmlOutput("selectNormalisation"),
                hidden(
                  helpText(id="tooltip_normalisation",
                        "Select the type of normalisation from the dropdown menu.
                        Choose \"none\" if no normalisation should be performed
                        or if the data has already been normalised.
                        Note that with Progenesis data, we try to import the Normalized abundance.
                        Therefore, the default normalisation for Progenesis data is set to \"none\".
                        ")
                )
              )
            ),

            actionButton(inputId="goNorm",
                         label="Start Normalization!"
                         )
          ),


	        #Main panel with number of output and plots
          mainPanel(width = 5,

              h3("Diagnostic plots"),

              strong('Number of features before preprocessing:'),textOutput('nfeaturesRaw',container = span),div(),

              strong('Number of features after preprocessing:'),textOutput('nfeaturesNormalized',container = span),div(),

              htmlOutput("selectColPlotNorm1"),

              div(
                  list(
                    h4("Intensities after transformation"),
                    tags$button(id="button_h4_int_transformation",tags$sup("[?]"))
                    )
                  ),

              hidden(
                helpText(id="tooltip_h4_int_transformation","
                        A density plot showing the distribution of the feature intensities when only
                        the transformation is executed.
                        Transformation is included because a density plot of untransformed intensities is often uninformative
                        due to a strong skew to the right.
                        Brush and double-click on a selected area to zoom in.
                        Double click outside a selected area to zoom out."
                        )
                ),

              plotOutput('plotRaw',
                   click = "plotRaw_click",
                   dblclick = "plotRaw_dblclick",
                   brush = brushOpts(
                     id = "plotRaw_brush",
                     resetOnNew = TRUE)
                   ),

              div(
                  list(
                    h4("feature intensities after normalisation"),
                    tags$button(id="button_h4_normalisation",
                      tags$sup("[?]")                      )
                    )
                  ),

              hidden(helpText(id="tooltip_h4_normalisation","
                        A density plot showing the distribution of the feature intensities
                        after execution of all preprocessing steps.
                        This allows you to evaluate the effect of the preprocessing.
                        Brush and double-click on a selected area to zoom in.
                        Double click outside a selected area to zoom out.")
                        ),

              plotOutput('plotNorm1',
                   click = "plotNorm1_click",
                   dblclick = "plotNorm1_dblclick",
                   brush = brushOpts(
                     id = "plotNorm1_brush",
                     resetOnNew = TRUE
                     )
                   ),

              div(
                list(
                  h4("MDS plot based on normalized feature intensities"),
                  tags$button(id="button_h4_MDS_normalisation",tags$sup("[?]"))
                  )
                ),

              hidden(helpText(id="tooltip_h4_MDS_normalisation","A multidimensional scaling plot. This plot shows a two-dimensional scatterplot
                        so that distances on the plot approximate the typical log2 fold changes between the samples based on a pairwise comparison
                        of the 500 most different features.
                        Brush and double-click on a selected area to zoom in.
                        Double click outside a selected area to zoom out.")
                        ),


              div(checkboxInput("plotMDSPoints",
                "Plot MDS points",
                value=FALSE)
                ),

              plotOutput('plotMDS',
				          click = "plotMDS_click",
                  dblclick = "plotMDS_dblclick",
                  brush = brushOpts(
                    id = "plotMDS_brush",
                    resetOnNew = TRUE
                    )
                  )
            )
       )
    )

    ###
    #Summarisation tab
    ########
    ,tabPanel("Summarization",
      sidebarLayout(
        sidebarPanel(

          h3("Settings"),

          h4("Summarisation"),

          div(
            list(
              tags$label("Summarisation", `for`="Summarisation"),
              tags$button(id="button_summarisation", tags$sup("[?]")),
              selectInput("summarisation", NULL, c("none","robust","medpolish","mean","median","sum"), width = '100%'),
              htmlOutput("selectSummarisation"),
              hidden(
                helpText(id="tooltip_summarisation",
                 "Select the type of summarization from the dropdown menu."
                 )
                ),
              actionButton(inputId="goSum", label="Start Summarisation!")
              )
            )
          ),

        mainPanel(
          h3("Diagnostic Plots"),
          htmlOutput("selectColPlotProt"),
          div(
            list(
              h4("MDS plot after full preprocessing"),
              tags$button(id="button_h4_MDS_summarisation",tags$sup("[?]"))
              )
            ),

          hidden(
            helpText(
              id="tooltip_h4_MDS_summarisation",
              "A multidimensional scaling plot. This plot shows a two-dimensional scatterplot
              so that distances on the plot approximate the typical log2 fold changes between the samples based on a pairwise comparison
              of the 500 most different features.
              Brush and double-click on a selected area to zoom in.
              Double click outside a selected area to zoom out."
              )
            ),
          div(
            checkboxInput("plotMDSPointsProt",
              "Plot MDS points",
              value=FALSE
              )
            ),

          plotOutput('plotMDSProt',
            click = "plotMDSProt_click",
            dblclick = "plotMDSProt_dblclick",
            brush = brushOpts(
              id = "plotMDSProt_brush",
              resetOnNew = TRUE
              )
            )
          )
        )
      )



    ###########################
    #Build Model
    ###########################
    ,tabPanel("Model",
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
                  Make Formula"
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
                  We therefore suggest to switch ridge regression off \"No\" until you want to perform the final analysis. "
                  )
                )
              )
            ),

          actionButton(inputId="fitModel", label="Fit Model!")
          ),

        mainPanel(
          fluidRow(column(width = 12, h3("Design Variables"),DT::dataTableOutput('annotationDataMatrix'))),
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
                  "Formulate null hypothesis in terms of a (linear combination) of the model parameters."
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

          fluidRow(column(width = 12, h3("Results table"),DT::dataTableOutput('table')))
        )
      )
    )

    ###############
    #Detail plots
    ###############

    ,tabPanel("DetailPlots",
      sidebarLayout(
        sidebarPanel(
          htmlOutput("selectColDetailPlot2"),
          htmlOutput("selectHorizontalDetailPlot2"),
          htmlOutput("selectVerticalDetailPlot2")
        ),

      mainPanel(
        h4("Select one feature in the volcano plot or in the table of the inference tab to visualize the expression values"),
        uiOutput("detailPlots")
        )
      )
    )
#close navbar, page, etc.
  )
)
