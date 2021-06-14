#Function to convert data paths if on windows
getDataPath <- function(datapath){
  if(Sys.info()['sysname']=="Windows"){
    datapath <- gsub("\\","/",datapath, fixed=TRUE)
  }
  return(datapath)
}

#Helper Function to plot densities
getDensXlimYlim <- function(se){
  densAll=apply(assay(se),2,density,na.rm=TRUE)
  ymax=max(vapply(densAll,function(d) max(d$y),1))
  rangematrix <- vapply(densAll,function(d) range(d$x, na.rm=TRUE), c(1,1)) #no longer range(eset), but range of d$x!
  xlim=range(rangematrix,na.rm=TRUE)
  ylim=c(0,ymax)
  return(list(densAll=densAll, xlim=xlim, ylim=ylim))
}

#Function to plot densities
plotDens <- function(se,
                     xlim=NULL,
                     ylim=NULL,
                     colors=1,
                     las=1,
                     frame.plot=FALSE,
                     ...){
      hlp <- getDensXlimYlim(se)
      if (is.null(xlim)) xlim<-hlp$xlim
      if (is.null(ylim)) ylim <- hlp$ylim
      if (length(colors)>1) {
        plot(hlp$densAll[[1]],col=colors[1],xlim=xlim,ylim=ylim, las=las, frame.plot=frame.plot, main="",...)
        for (i in 2:ncol(se)) lines(hlp$densAll[[i]],col=colors[i])
      } else {
        plot(hlp$densAll[[1]],xlim=xlim,ylim=ylim, las=las, frame.plot=frame.plot, main="",...)
        for (i in 2:ncol(se)) lines(hlp$densAll[[i]])
      }
}

#Function for volcano plot
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

#Function to construct detail plot
makeDetailPlots <- function(pe,
                            clickInfo,
                            input){
    if (!is.null(pe)){
        s <- input$table_rows_selected
        if (length(s)==1)
        {
          featureName <- rownames(clickInfo())[s]
          pePlot <- pe[featureName,,c("featureNorm","summarized")]
          pePlotDf <- data.frame(longFormat(pePlot))
          pePlotDf$assay <- factor(pePlotDf$assay,
                                  levels = c("featureNorm", "summarized"))
          if (input$selColDetailPlot2!="none"){
              if(class(colData(pePlot)[[input$selColDetailPlot2]])=="factor"){
              pePlotDf[,input$selColDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,input$selColDetailPlot2]))
              }
          }
          if (input$selVertDetailPlot2!="none"){
              if(class(colData(pePlot)[[input$selVertDetailPlot2]])=="factor"){
              pePlotDf[,input$selVertDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,input$selVertDetailPlot2]))
              }
          }
          if (input$selHorDetailPlot2!="none"){
              if(class(colData(pePlot)[[input$selHorDetailPlot2]])=="factor"){
              pePlotDf[,input$selHorDetailPlot2] <- as.factor(as.character(colData(pePlot)[pePlotDf$colname,input$selHorDetailPlot2]))
              }
          }
          if (input$logtransform) {
             ylab <- "feature intensity (log2)"
             } else {
             ylab <- "feature intensity"
             }
          p1 <- ggplot(data = pePlotDf,
                 aes(x = colname,
                     y = value,
                     group = rowname)) +
              geom_line() +
              geom_point() +
              facet_grid(~ assay) +
              labs(title = featureName, x = "sample", y = ylab)
          if (input$selColDetailPlot2!="none") {
              if (class(colData(pePlot)[[input$selColDetailPlot2]])=="factor") {
                p2 <- ggplot(pePlotDf, aes(x = colname, y = value,fill=pePlotDf[,input$selColDetailPlot2]))
                }
              } else {
              p2 <- ggplot(pePlotDf, aes(x = colname, y = value))
              }
          p2 <- p2 +
              geom_boxplot(outlier.shape = NA) +
              geom_point(position = position_jitter(width = .1), aes(shape = rowname)) +
              scale_shape_manual(values = 1:nrow(pePlotDf)) +
              labs(title = featureName, x = "sample", y = ylab)
          if (input$selVertDetailPlot2!="none"|input$selHorDetailPlot2!="none"){
              if (input$selVertDetailPlot2=="none") {
                  if (class(colData(pePlot)[[input$selHorDetailPlot2]])=="factor")
                      p2 <- p2 + facet_grid(~pePlotDf[,input$selHorDetailPlot2])
                  } else {
                  if (input$selHorDetailPlot2=="none"){
                      p2 <- p2 + facet_grid(pePlotDf[,input$selVertDetailPlot2]~.)
                      } else {
                      p2 <- p2 + facet_grid(pePlotDf[,input$selVertDetailPlot2]~pePlotDf[,input$selHorDetailPlot2])
                      }
                  }
          }
          return(list(p2,p1))
          }
        }
}

##' Copied from MSnbase
##' Given a text spread sheet \code{f} and a \code{pattern} to
##' be matched to its header (first line in the file), the function
##' returns the matching columns names or indices of the
##' corresponding \code{data.frame}.
##'
##' The function starts by reading the first line of the file (or connection)
##' \code{f} with \code{\link{readLines}}, then splits it
##' according to the optional \code{...} arguments (it is important to
##' correctly specify \code{\link{strsplit}}'s \code{split} character vector here)
##' and then matches \code{pattern} to the individual column names using
##' \code{\link{grep}}.
##'
##' Similarly, \code{getEcols} can be used to explore the column names and
##' decide for the appropriate \code{pattern} value.
##'
##' These functions are useful to check the parameters to be provided to
##' \code{\link{readMSnSet2}}.
##'
##' @title Returns the matching column names of indices.
##' @param f A connection object or a \code{character} string to be
##'     read in with \code{readLines(f, n = 1)}.
##' @param pattern A \code{character} string containing a regular
##'     expression to be matched to the file's header.
##' @param ... Additional parameters passed to \code{\link{strsplit}}
##'     to split the file header into individual column names.
##' @param n An \code{integer} specifying which line in file \code{f}
##'     to grep (get). Default is 1. Note that this argument must be
##'     named.
##' @return Depending on \code{value}, the matching column names of
##'     indices. In case of \code{getEcols}, a \code{character} of
##'     column names.
##' @seealso \code{\link{readMSnSet2}}
##' @author Laurent Gatto
grepEcols <- function(f, pattern, ..., n = 1)
  grep(pattern, strsplit(readLines(f, n), ...)[n][[1]])


