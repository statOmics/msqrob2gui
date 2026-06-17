#' Shiny app server object
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback

msqrob2ModelingUI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quality control", tabName = "qc"),
      menuItem("Model", tabName = "model"),
      menuItem("Inference", tabName = "inference"),
      menuItem("Report", tabName = "report")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "qc",qcUI()),
      tabItem(tabName="model",modelUI()),
      tabItem(tabName="inference", inferenceUI()),
      tabItem(tabName="report", reportUI())
    )
    )
)
