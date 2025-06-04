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
      menuItem("Model", tabName = "model"),
      menuItem("Inference", tabName = "inference"),
      menuItem("Report", tabName = "report")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "import",inputUI()),
      tabItem(tabName="model",modelUI()),
      tabItem(tabName="inference", inferenceUI()),
      tabItem(tabName="report", reportUI())
    )
    )
)
