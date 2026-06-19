#' Shiny app UI object
#' @importFrom shinydashboard dashboardBody tabItem tabItems sidebarMenu menuItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback

msqrob2UI <- dashboardPage(
  dashboardHeader(title = "msqrob2 GUI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import",          tabName = "import",      icon = icon("file-import")),
      menuItem("Data processing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Quality control", tabName = "qc",          icon = icon("chart-bar")),
      menuItem("Model",           tabName = "model",       icon = icon("sliders-h")),
      menuItem("Inference",       tabName = "inference",   icon = icon("flask")),
      menuItem("Report",          tabName = "report",      icon = icon("file-alt")),
      menuItem("Explore with altSEE", tabName = "explore", icon = icon("microscope"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import",        importUI()),
      tabItem(tabName = "preprocessing", preprocessingUI()),
      tabItem(tabName = "qc",            qcUI(showFileInput = FALSE)),
      tabItem(tabName = "model",         modelUI()),
      tabItem(tabName = "inference",     inferenceUI()),
      tabItem(tabName = "report",        reportCombinedUI()),
      tabItem(tabName = "explore",       altSEEExploreUI())
    )
  )
)
