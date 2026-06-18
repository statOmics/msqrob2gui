#' Launch the msqrob2 Explore App
#'
#' An extension of \code{\link{launchMsqrob2ModelingApp}} that adds an
#' interactive "Explore with altSEE" tab. After running modelling and
#' inference, switch to that tab to configure and launch an iSEE session in a
#' new browser tab. iSEE runs in a background R process, so the modelling app
#' stays responsive — make changes, refit, then re-launch iSEE to explore the
#' updated results.
#'
#' @param maxSize maximum memory size that input files are allowed to have in Mb
#'
#' @return shiny application object
#'
#' @export launchMsqrob2ExploreApp
#'
#' @examples
#' \dontrun{launchMsqrob2ExploreApp()}
#'
#' @import shiny shinymeta QFeatures SummarizedExperiment shinybusy

launchMsqrob2ExploreApp <- function(maxSize = 500) {
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
  shinyApp(ui = msqrob2ExploreUI, server = msqrob2ExploreServer)
}
