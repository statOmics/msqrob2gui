#' An wrapper function to handle errors and warnings
#' Will create a notification and add the exception to the global exception data
#' @param func function that is wrapped
#' @param component_name `str` name of the component (will be reported in the exception message)
#' @param ... arguments to be passed to the function
#'
#' @return Does not return anything but will create a notification and add the exception to the global exception data
#' @rdname INTERNAL_error_handler
#' @keywords internal
#'
#' @importFrom shiny showNotification
#' @importFrom htmltools HTML div
error_handler <- function(func, component_name, ...) {
  tryCatch(
    {
      func_call <- gsub(
        "\\s+", " ",
        paste(deparse(substitute(func(...))), collapse = " ")
      )
      func(...)
    },
    warning = function(w) {
      time <- Sys.time()
      showNotification(
        HTML(
          paste0(
            div(HTML(
              paste0(
                "<b> Warning in ",
                component_name,
                " </b> at ", format(time, "%H:%M:%S")
              )
            )),
            div(HTML(
              "<i>Check the top right exception dropdown menu for more details</i>" # nolint
            ))
          )
        ),
        duration = 30,
        type = "warning"
      )
      add_exception(
        title = paste0("Warning in ", component_name),
        type = "warning",
        func_call = func_call,
        message = conditionMessage(w),
        full_message = w,
        time = time
      )
      suppressWarnings(func(...))
    },
    error = function(e) {
      time <- Sys.time()
      showNotification(
        HTML(
          paste0(
            div(HTML(
              paste0(
                "<b> Error in ",
                component_name,
                " </b> at ", format(time, "%H:%M:%S")
              )
            )),
            div(HTML(
              "<i>Check the top right exception dropdown menu for more details</i>" # nolint
            ))
          )
        ),
        duration = 30,
        type = "error"
      )
      add_exception(
        title = paste0("Error in ", component_name),
        type = "error",
        func_call = func_call,
        message = conditionMessage(e),
        full_message = e,
        time = time
      )
      return(NULL)
    }
  )
}

#' A function that will add an exception entry to the global exception data
#'
#' @param title `str` title of the exception
#' @param type `str` type of the exception c("warning", "error")
#' @param func_call `str` function call that caused the exception
#' @param message `str` message of the exception
#' @param full_message `str` full message of the exception
#' @param time `POSIXct` time of the exception
#'
#' @return does not return anything but adds an exception to the global exception data
#' @rdname INTERNAL_add_exception
#' @keywords internal
#'
#' @importFrom shiny isolate
add_exception <- function(title, type, func_call, message, full_message, time) {
  new_data <- data.frame(
    title = as.character(title),
    type = as.character(type),
    func_call = as.character(func_call),
    message = as.character(message),
    full_message = as.character(full_message),
    time = as.POSIXct(time),
    stringsAsFactors = FALSE
  )
  old_data <- isolate(variables$exception_data)
  variables$exception_data <- rbind(new_data, old_data)
}


#' A function that will subset the assays of a QFeatures object
#' @param qfeatures `QFeatures` object to subset
#' @param pattern `str` pattern to match the assays names
#' @return `QFeatures` object with the subsetted assays
#' @rdname INTERNAL_page_assays_subset
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#'
page_assays_subset <- function(qfeatures, pattern=NULL) {
  to_process <- if (is.null(pattern)) {
    1:length(qfeatures)
  } else grep(
    pattern,
    names(qfeatures),
    fixed = TRUE
  )
  if (length(qfeatures) > 0 && length(to_process) == 0) {
    showModal(modalDialog(
      title = "Step not found",
      HTML(paste0(
        "<i>", "The output from the previous step could not ",
        "be found. Are you sure you have saved the processed ",
        "data from the previous step?", "</i>"))
    ))
    QFeatures()
  } else {
    suppressMessages(suppressWarnings(qfeatures[, , to_process]))
  }
}


#' Will convert a qfeatures object to a summary data.frame object
#'
#' @param qfeatures a qfeatures object
#'
#' @return a data.frame object
#' @rdname INTERNAL_qfeatures_to_df
#' @keywords internal
#'
qfeatures_to_df <- function(qfeatures) {
  df <- data.frame(
    "Name" = rep.int(0, length(qfeatures)),
    "Class" = rep.int(0, length(qfeatures)),
    "nrows" = rep.int(0, length(qfeatures)),
    "ncols" = rep.int(0, length(qfeatures))
  )
  for (i in seq_along(qfeatures)) {
    df[i, "Name"] <- names(qfeatures)[[i]]
    df[i, "Class"] <- class(qfeatures[[i]])[[1]]
    df[i, "nrows"] <- nrow(qfeatures[[i]])[[1]]
    df[i, "ncols"] <- ncol(qfeatures[[i]])[[1]]
  }

  df
}


#' Function to convert data paths if on windows
#'
#' @param datapath string with path to file
#'
#' @return string
#' @rdname INTERNAL_getDataPath
#' @keywords internal
#'
getDataPath <- function(datapath){
  if(Sys.info()['sysname']=="Windows"){
    datapath <- gsub("\\","/",datapath, fixed=TRUE)
  }
  return(datapath)
}




#' Function for volcano plot
#'
#' @param dataset dataframe with variables logFC: log2 fold changes and pval: pvalues. Typically output from the hypothesisTest function from msqrob2
#' @param clickInfo clickInfo object with the selection of features in plot or DT data table
#' @param ranges ranges object from the selectio nof features in plot
#' @param input input object with reactive values passed through via the server UI.
#'
#' @return ggplot object
#' @rdname INTERNAL_makeVolcanoPlot
#' @keywords internal
#' @importFrom ggplot2 ggplot geom_point scale_color_manual theme_minimal coord_cartesian aes alpha
#'
#'
makeVolcanoPlot <- function(dataset,
                            clickInfo,
                            input,
                            ranges){
  if (!is.null(dataset)){
    volcano <- ggplot(dataset,
                      aes(x = logFC,
                          y = -log10(pval),
                          color = adjPval < input$alpha)) +
      geom_point(cex = 2.5) +
      scale_color_manual(values = alpha(c("black", "red"), 0.5)) +
      theme_minimal() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)


    s <- input$table_rows_selected
    if (length(s)) {
      subdataset <- clickInfo()[s, , drop = FALSE]
      return(volcano +
               geom_point(data=subdataset[subdataset$adjPval<input$alpha,],col="firebrick",size=5,show.legend=FALSE) +
               geom_point(data=subdataset[subdataset$adjPval>=input$alpha,],col="#112446",size=5,show.legend=FALSE)
      )
    } else {
      return(return(volcano))
    }
  }
}

#' Function for detailplot
#'
#' @param pe QFeatures object
#' @param clickInfo clickInfo object with the selection of features in plot or DT data table
#' @param inputServerInput list with reactive values returned by inputModule Server
#' @param detailServerInput input object with reactive values passed through via the inference Server UI.
#'
#' @return ggplot object
#' @rdname INTERNAL_makeDetailPlots
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot geom_point geom_line facet_grid theme labs  geom_boxplot  scale_shape_manual aes element_text position_jitter
#'
makeDetailPlots <- function(pe,
                            clickInfo,
                            detailServerInput,
                            inputServerInput){
  if (!is.null(pe)){
    s <- detailServerInput$table_rows_selected
    if (length(s)==1)
    {
      featureName <- rownames(clickInfo())[s]
      selectedAssayNames <- c(detailServerInput$selectedLowLevelAssay[!(detailServerInput$selectedLowLevelAssay %in% inputServerInput$selectedAssay())], inputServerInput$selectedAssay())
      pePlot <- pe[featureName,,selectedAssayNames]
      pePlotDf <- data.frame(longForm(pePlot))
      pePlotDf$assay <- factor(pePlotDf$assay,
                               levels = selectedAssayNames)
      if (detailServerInput$selColDetailPlot2!="none"){
        if(class(colData(pePlot)[[detailServerInput$selColDetailPlot2]])=="factor" || class(colData(pePlot)[[detailServerInput$selColDetailPlot2]])=="character"){
          pePlotDf[,detailServerInput$selColDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,detailServerInput$selColDetailPlot2]))
        }
      }
      if (detailServerInput$selVertDetailPlot2!="none"){
        if(class(colData(pePlot)[[detailServerInput$selVertDetailPlot2]])=="factor" || class(colData(pePlot)[[detailServerInput$selVertDetailPlot2]])=="character"){
          pePlotDf[,detailServerInput$selVertDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,detailServerInput$selVertDetailPlot2]))
        }
      }
      if (detailServerInput$selHorDetailPlot2!="none"){
        if(class(colData(pePlot)[[detailServerInput$selHorDetailPlot2]])=="factor"||class(colData(pePlot)[[detailServerInput$selHorDetailPlot2]])=="character"){
          pePlotDf[,detailServerInput$selHorDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,detailServerInput$selHorDetailPlot2]))
        }
      }
      #if (detailServerInput$logtransform) {
      #   ylab <- "feature intensity (log2)"
      #   } else {
      #   ylab <- "feature intensity"
      #   }
      ylab <- "feature intensity"

      p1 <- ggplot(data = pePlotDf,
                   aes(x = colname,
                       y = value,
                       group = rowname)) +
        geom_line() +
        geom_point() +
        facet_grid(~ assay) +
        labs(title = featureName, x = "sample", y = ylab) +
        theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 0.5))

      if (detailServerInput$selColDetailPlot2!="none") {
        if (class(pePlotDf[[detailServerInput$selColDetailPlot2]])=="factor") {
          p2 <- ggplot(pePlotDf, aes(x = colname, y = value,fill=pePlotDf[,detailServerInput$selColDetailPlot2]))
        }
      } else {
        p2 <- ggplot(pePlotDf, aes(x = colname, y = value))
      }
      p2 <- p2 +
        geom_boxplot(outlier.shape = NA) +
        geom_point(position = position_jitter(width = .1), aes(shape = rowname)) +
        scale_shape_manual(values = 1:nrow(pePlotDf)) +
        labs(title = featureName, x = "sample", y = ylab) +
        theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 0.5)) +
        facet_grid(~ assay)

      if (detailServerInput$selVertDetailPlot2!="none"|detailServerInput$selHorDetailPlot2!="none"){
        if (detailServerInput$selVertDetailPlot2=="none") {
          if (class(pePlotDf[[detailServerInput$selHorDetailPlot2]])=="factor")
            p2 <- p2 + facet_grid(~assay+pePlotDf[,detailServerInput$selHorDetailPlot2])
        } else {
          if (detailServerInput$selHorDetailPlot2=="none"){
            p2 <- p2 + facet_grid(pePlotDf[,detailServerInput$selVertDetailPlot2]~assay)
          } else {
            p2 <- p2 + facet_grid(pePlotDf[,detailServerInput$selVertDetailPlot2]~assay+pePlotDf[,detailServerInput$selHorDetailPlot2])
          }
        }
      }
      return(list(p2,p1))
    }
  }
}

#' Function for plotting densities of features
#'
#' @param pe QFeatures object
#' @param assayName string with name of QFeatures assay
#' @param varName string with name of variable in colData used to color the density curves
#' @return ggplot object
#' @rdname INTERNAL_plotDensities
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot geom_density aes_string
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom SummarizedExperiment assay
#'
plotDensities <- function(pe, assayName, varName)
{
  varName <- as.character(varName)
  pe[[assayName]] |>
    assay() |>
    as.data.frame() |>
    gather(sample, intensity) |>
    mutate({{varName}}:=colData(pe)[sample,varName]) |>
    ggplot(aes_string(x="intensity",group="sample",col=varName)) +
    geom_density()
}

#' Function for plotting densities of features
#'
#' @param dataset data frame returned by hypothesisTest function of msqrob2
#' @param sel selection of features to be included in plot
#' @param regulation string "up", "down" or "both" to plot only upregulated, downregulated or all DE features.
#' @return baseplot object
#' @rdname INTERNAL_plotDensities
#' @keywords internal
#'
#'
makeBoxplotFC<-function(dataset, sel, regulation="both")
{
  if (regulation =="both")
    boxplot(dataset[sel,"logFC"], xlab="logFC",horizontal=TRUE,ylim=range(dataset[["logFC"]],na.rm=TRUE))
  if (regulation =="up")
    boxplot((dataset|>filter(logFC>0))[sel,"logFC"], xlab="logFC",horizontal=TRUE,ylim=range(dataset[["logFC"]],na.rm=TRUE))
  if (regulation =="down")
    boxplot((dataset|>filter(logFC<0))[sel,"logFC"], xlab="logFC",horizontal=TRUE,ylim=range(dataset[["logFC"]],na.rm=TRUE))
}


#' Function for pca plot of samples
#'
#' @param pe QFeatures object
#' @param assayName string with name of QFeatures assay
#' @param varName string with name of variable in colData used to color the density curves
#' @return ggplot object
#' @rdname INTERNAL_plotPCA
#' @keywords internal
#'
#' @importFrom pcaMethods pca scores
#' @importFrom ggplot2 ggplot geom_point xlab ylab sym
#'
#'

plotPCA <- function(pe, assayName, varName)
{
  varName <- as.character(varName)
  pc <- pe[[assayName]] |>
    assay() |>
    as.data.frame() |>
    t() |>
    pca(method="nipals")
  df <- merge(scores(pc),colData(pe),by = 0)

  ggplot(df, aes(PC1, PC2, col = !!sym(varName))) +
    geom_point() +
    xlab(paste0("PC1 (", round(pc@R2[1] * 100,1),"%)")) +
    ylab(paste0("PC2 (", round(pc@R2[2] * 100,1),"%)"))
}
