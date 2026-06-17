#' Shiny app server object
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback

msqrob2PreprocessingUI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "import"),
      menuItem("Preprocessing", tabName = "preprocessing"),
      menuItem("Quality control", tabName = "qc"),
      menuItem("Report", tabName = "reportPreprocessing")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "import",importUI()),
      tabItem(tabName = "preprocessing",preprocessingUI()),
      tabItem(tabName = "qc",qcUI()),
      tabItem(tabName="reportPreprocessing", reportPreprocessingUI())
    )
    )
)
