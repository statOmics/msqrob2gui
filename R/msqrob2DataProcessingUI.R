#' Shiny app UI object for the data processing app
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback

msqrob2DataProcessingUI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import",          tabName = "import",              icon = icon("file-import")),
      menuItem("Data processing", tabName = "preprocessing",       icon = icon("cogs")),
      menuItem("Quality control", tabName = "qc",                  icon = icon("chart-bar")),
      menuItem("Report",          tabName = "reportDataProcessing", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import",               importUI()),
      tabItem(tabName = "preprocessing",        preprocessingUI()),
      tabItem(tabName = "qc",                   qcUI(showFileInput = FALSE)),
      tabItem(tabName = "reportDataProcessing", reportDataProcessingUI())
    )
  )
)
