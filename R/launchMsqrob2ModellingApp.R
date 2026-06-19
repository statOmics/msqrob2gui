#' Launch the msqrob2 Modelling App
#'
#' A modelling and exploration app with tabs for quality control, model
#' specification, inference, reporting, and interactive exploration via altSEE.
#' After running modelling and inference, switch to the "Explore with altSEE"
#' tab to launch an iSEE session in a new browser tab. iSEE runs in a
#' background R process so the modelling app stays responsive — make changes,
#' refit, then re-launch iSEE to explore the updated results.
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#' @param launch.browser logical; if TRUE (default) the app opens in the system browser
#'
#' @return shiny application object
#'
#' @export launchMsqrob2ModellingApp
#'
#' @examples
#' \dontrun{launchMsqrob2ModellingApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy

launchMsqrob2ModellingApp <- function(maxSize = 500, launch.browser = TRUE) {
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
  shinyApp(ui = msqrob2ModellingUI, server = msqrob2ModellingServer, options = list(launch.browser = launch.browser))
}

#' @rdname launchMsqrob2ModellingApp
#' @export launchMsqrob2ModelingApp
launchMsqrob2ModelingApp <- launchMsqrob2ModellingApp
