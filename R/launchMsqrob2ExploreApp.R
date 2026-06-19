#' Launch the msqrob2 Explore App
#'
#' A lightweight app with a Quality Control tab for loading a QFeatures object
#' and an "Explore with altSEE" tab for launching an interactive iSEE session.
#' iSEE runs in a background R process so this app stays responsive while you
#' explore results.
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#' @param launch.browser logical; if TRUE (default) the app opens in the system browser
#'
#' @return shiny application object
#'
#' @export launchMsqrob2ExploreApp
#'
#' @examples
#' \dontrun{launchMsqrob2ExploreApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy

launchMsqrob2ExploreApp <- function(maxSize = 500, launch.browser = TRUE) {
  required     <- c("scater", "iSEE", "iSEEu", "altSEE", "callr")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed: ",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Install with:\n",
      "  BiocManager::install(c('scater', 'iSEE', 'iSEEu'))\n",
      "  remotes::install_github('statomics/altSEE')\n",
      "  install.packages('callr')"
    )
  }
  options(shiny.maxRequestSize = maxSize * 1024^2)
  shinyApp(ui = msqrob2ExploreUI, server = msqrob2ExploreServer, options = list(launch.browser = launch.browser))
}
