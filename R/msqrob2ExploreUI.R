#' Shiny app UI for the msqrob2 Explore app
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shiny icon

msqrob2ExploreUI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quality control",     tabName = "qc",       icon = icon("chart-bar")),
      menuItem("Model",               tabName = "model",    icon = icon("calculator")),
      menuItem("Inference",           tabName = "inference", icon = icon("flask")),
      menuItem("Report",              tabName = "report",   icon = icon("file-alt")),
      menuItem("Explore with altSEE", tabName = "explore",  icon = icon("microscope"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "qc",        qcUI()),
      tabItem(tabName = "model",     modelUI()),
      tabItem(tabName = "inference", inferenceUI()),
      tabItem(tabName = "report",    reportUI()),
      tabItem(tabName = "explore",   altSEEExploreUI())
    )
  )
)
