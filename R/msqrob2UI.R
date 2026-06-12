#' Shiny app server object
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback

msqrob2UI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "import"),
      menuItem("Preprocessing", tabName = "preprocessing"),
    #  menuItem("Summarisation", tabName = "summarisation"),
     # menuItem("Quality control", tabName = "qc"),
      menuItem("Model", tabName = "model"),
      menuItem("Inference", tabName = "inference"),
      menuItem("Report", tabName = "report")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "import",importUI()),
      tabItem(tabName = "preprocessing",preprocessingUI()),
    #  tabItem(tabName = "summarisation",summarisationUI()),
    #  tabItem(tabName = "qc",qcUI()),
      tabItem(tabName="model",modelUI()),
      tabItem(tabName="inference", inferenceUI()),
      tabItem(tabName="report", reportUI())
    )
    )
)
