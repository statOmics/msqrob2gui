### add line to define selectedSet

### install missing dependencies (unquote code block below)
# bioc_pkgs <- c("scater", "iSEE", "iSEEu")
# missing_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace, logical(1), quietly = TRUE)]
# if (length(missing_bioc) > 0) {
#   if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
#   BiocManager::install(missing_bioc)
# }
# if (!requireNamespace("altSEE", quietly = TRUE)) {
#   if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
#   remotes::install_github("statomics/altSEE")
# }

### read qfeaturesFile
qf <- readRDS("qfeaturesFile.rds")

#### convert qf to sce
library(scater)
library(QFeatures)
sce <- getWithColData(qf, i = selectedSet) |> 
  as("SingleCellExperiment") |> 
  runMDS(exprs_values = 1) 
rowData(sce) <- rowData(sce) |> 
  as.data.frame() |> 
  dplyr::select(where(is.atomic))  
setNames <- names(qf)
setNames <- setNames[setNames != selectedSet]
for (i in setNames)
{
  altExp(sce,i) <- qf[[i]] |> 
    as("SingleCellExperiment") |> 
    runMDS(exprs_values = 1) 
  rowData(altExp(sce,i)) <- rowData(altExp(sce,i)) |> 
    as.data.frame() |> 
    dplyr::select(where(is.atomic))   
}

#### iSEE panels
library(iSEE)
library(iSEEu)
library(altSEE)

rdp <- ReducedDimensionPlot(PanelWidth = 6L)
ardp <- AltReducedDimensionPlot(PanelWidth = 6L)
vp <- VolcanoPlot(PanelWidth = 6L)
avp <- AltVolcanoPlot(PanelWidth = 6L)
rdt <- RowDataTable(RowSelectionSource = "VolcanoPlot1", PanelWidth = 6L)
ardt <- AltRowDataTable(RowSelectionSource = "AltVolcanoPlot1", 
                        PanelWidth = 6L)
mlfap <- LinkedFeaturesAssayPlot(
  SelectionExperiment = "(Main)",
  Experiment = "(Main)",
  YAxisFeatureSource = "RowDataTable1",
  PlotType = "Scatter + lines",
  XAxis = "Column data",
  XAxisColumnData = "sampleId",
  PanelWidth = 6L
)

alfap <- LinkedFeaturesAssayPlot(
  YAxisFeatureSource = "RowDataTable1",
  SelectionExperiment = "(Main)",
  PlotType = "Scatter + lines",
  XAxis = "Column data",
  XAxisColumnData = "sampleId",
  PanelWidth = 6L
)

initial_panels <- list(rdp, ardp, vp, avp, rdt, ardt, mlfap, alfap)

app <- iSEE(
  sce,
  initial = initial_panels
)

if (interactive()) {
  shiny::runApp(app, launch.browser = TRUE)
}