
#' Internal utility imports
#'
#' @name msqrob2gui-utilities
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_density geom_boxplot geom_col geom_bar geom_point geom_histogram geom_vline annotate labs theme_minimal theme_bw theme_void theme element_text
#' @importFrom grid unit
#' @importFrom dplyr filter group_by summarise mutate n_distinct first
#' @importFrom QFeatures longForm
#' @importFrom SummarizedExperiment assay colData rowData
#' @importFrom scater runMDS
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom omicsGMF runGMF
NULL

#' Histogram of proportion of missing values per feature
#'
#' @param pe A \code{QFeatures} object.
#' @param assayName Character. Name of the assay to plot.
#' @param threshold Numeric. The NA proportion threshold shown as a vertical
#'   red dashed line.
#' @return A \code{ggplot} object.
#' @keywords internal
PlotMissingValues <- function(pe, assayName, threshold) {
  mat <- SummarizedExperiment::assay(pe[[assayName]])
  df  <- data.frame(pNA = rowMeans(is.na(mat)))
  ggplot(df, aes(x = pNA)) +
    geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
    geom_vline(xintercept = threshold, colour = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = paste("Missing value distribution —", assayName),
      x     = "Proportion of missing values",
      y     = "Number of features"
    ) +
    theme_minimal()
}

#' Plot intensity density curves per sample
#'
#' @param pe A \code{QFeatures} object.
#' @param assayName Character. Name of the assay to plot.
#' @param varName Character. Name of a \code{colData} variable used to colour
#'   the curves.
#' @return A \code{ggplot} object.
#' @keywords internal
NewPlotDensities <- function(pe, assayName, varName) {
  longForm(pe[, , assayName], colvars = varName) |>
    ggplot() +
    aes(x = value, group = colname, col = as.factor(.data[[varName]])) +
    geom_density() +
    labs(
      title = paste("Intensity distributions —", assayName),
      x     = "Intensity",
      y     = "Density",
      col   = varName
    ) +
    theme_minimal() +
    theme(
      legend.position   = "right",
      legend.title      = element_text(face = "bold"),
      legend.key.height = unit(0.6, "cm")
    )
}

#' Boxplot of sample intensities
#'
#' @param pe A \code{QFeatures} object.
#' @param assayName Character. Name of the assay to plot.
#' @param varName Character. Name of a \code{colData} variable used to colour
#'   the boxes.
#' @return A \code{ggplot} object.
#' @keywords internal
PlotNormBoxplots <- function(pe, assayName, varName) {
  longForm(pe[, , assayName], colvars = varName) |>
    ggplot() +
    aes(x = colname, y = value, fill = as.factor(.data[[varName]])) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = paste("Sample intensities —", assayName),
      x     = "Sample",
      y     = "Intensity",
      fill  = varName
    ) +
    theme_minimal() +
    theme(
      axis.text.x       = element_blank(),
      axis.ticks.x      = element_blank(),
      legend.position   = "right",
      legend.title      = element_text(face = "bold"),
      legend.key.height = unit(0.6, "cm")
    )
}

#' Bar chart of identifications per sample
#'
#' Counts the number of distinct features with a non-missing intensity value
#' for each sample, coloured by a selected \code{colData} variable.
#'
#' @param pe A \code{QFeatures} object.
#' @param assayName Character. Name of the assay to plot.
#' @param varName Character. Name of a \code{colData} variable used to colour
#'   the bars.
#' @return A \code{ggplot} object.
#' @keywords internal
PlotIdentifications <- function(pe, assayName, varName) {
  longForm(pe[, , assayName], colvars = varName) |>
    as.data.frame() |>
    filter(!is.na(value)) |>
    group_by(colname) |>
    summarise(IDs = n_distinct(rowname), varCol = first(.data[[varName]]), .groups = "drop") |>
    ggplot(aes(x = colname, y = IDs, fill = as.factor(varCol))) +
    geom_col() +
    labs(
      title = paste("Identifications per sample —", assayName),
      x     = "Sample",
      y     = "Identifications",
      fill  = varName
    ) +
    theme_bw() +
    theme(
      axis.text.x       = element_blank(),
      axis.ticks.x      = element_blank(),
      legend.position   = "right",
      legend.title      = element_text(face = "bold"),
      legend.key.height = unit(0.6, "cm")
    )
}


#' Dimensionality reduction plot
#'
#' Computes a 2D dimensionality reduction of samples and plots them coloured by
#' a selected \code{colData} variable. Supports MDS (via
#' \code{scater::runMDS}) and OmicsGMF (via \code{omicsGMF::runGMF}).
#'
#' @param pe A \code{QFeatures} object.
#' @param assayName Character. Name of the assay to use.
#' @param varName Character. Name of a \code{colData} variable used to colour
#'   the points.
#' @param method Character. One of \code{"MDS"} or \code{"OmicsGMF"}.
#' @return A \code{ggplot} object.
#' @keywords internal
PlotDimReduction <- function(pe, assayName, varName, method = c("MDS", "OmicsGMF")) {
  method  <- match.arg(method)
  varName <- as.character(varName)

  meta <- colData(pe) |> as.data.frame()

  if (method == "MDS") {
    se     <- as(pe[[assayName]], "SingleCellExperiment")
    se     <- scater::runMDS(se, assay.type = 1)
    coords <- SingleCellExperiment::reducedDim(se, "MDS") |> as.data.frame()
    colnames(coords)[1:2] <- c("Dim1", "Dim2")
    xlabel <- "MDS1"
    ylabel <- "MDS2"
  } else {
    se     <- as(pe[[assayName]], "SingleCellExperiment")
    se     <- omicsGMF::runGMF(se, family = gaussian(), ncomponents = 2)
    coords <- SingleCellExperiment::reducedDim(se, "GMF") |> as.data.frame()
    colnames(coords)[1:2] <- c("Dim1", "Dim2")
    xlabel <- "GMF1"
    ylabel <- "GMF2"
  }

  df <- cbind(coords[, 1:2], meta)

  ggplot(df, aes(x = Dim1, y = Dim2, col = as.factor(.data[[varName]]))) +
    geom_point(size = 3) +
    labs(
      title = paste(method, "—", assayName),
      x     = xlabel,
      y     = ylabel,
      col   = varName
    ) +
    theme_minimal() +
    theme(
      legend.position   = "right",
      legend.title      = element_text(face = "bold"),
      legend.key.height = unit(0.6, "cm")
    )
}
