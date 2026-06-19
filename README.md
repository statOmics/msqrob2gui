# GUI implementation for the msqrob2 package

[msqrob2](https://www.bioconductor.org/packages/release/bioc/html/msqrob2.html)
is a Bioconductor package for differentially expression analysis in
mass spectrometry using the
[QFeatures](https://www.bioconductor.org/packages/release/bioc/html/QFeatures.html)
class infrastructure.

The `msqrob2gui` package implements a graphical user interface by
means of a shiny app, alleviating the need for programming skills to
use `msqrob2`.

## Installation

Get the `remotes` packages to install from GitHub

```r
# Check if remotes is installed. Otherwise install it.
if (!require("remotes", quietly = TRUE)){
    install.packages("remotes")
}
```

Install the `msqrob2gui` package from the statOmics branch. 

```r
remotes::install_github("statOmics/msqrob2gui")
```

Load the package.

```r
library(msqrob2gui)
```

## Launch app

The msqrob2gui package provides three gui's: 

1. A basic App for data processing: `launchMsqrob2DataProcessingApp()` that 
   starts from the raw search results and provides filtering, log-transformation, 
   normalisation, summarisation upto protein level abundances. The output is a 
   QFeatures object. For more advanced options or to develop your own data 
   processing workflow we recommend the use of [QFeaturesGUI](https://github.com/rformassspectrometry/QFeaturesGUI).

2. `launchMsqrob2ModelingApp()` or `launchMsqrob2ModellingApp()`: The main 
   msqrob2 modeling workhorse. It takes in a QFeatures object, e.g. 
   generated with the DataProcessingApp or `QFeaturesGUI` and provides 
   data exploration, statistical modeling and inference to prioritise 
   differential abundant features, and exploring the results with the 
   [altSEE](https://github.com/statOmics/altSEE) extension for [iSEE](https://github.com/iSEE). 
   The altSEE functionality enables the user to interactively explore the DA 
   proteins along with the raw and processed precursor/PSM-level and/or 
   peptide-level quantificiations. 

3. `launchMsqrob2App()`: An end-to-end workflow integrating 1 and 2 in one App. 

4. `launchMsqrob2ExploreApp()` the stand-alone altSEE/iSEE app for exploring 
   QFeatures objects. 

Note, that all apps provide the user with R-objects, plots and RMD reports that 
can be downloaded to document the data analysis in reproducible way. 

```{r}
library(msqrob2gui)

# Data Processing only (Alternatively users can use QFeaturesGUI)
launchMsqrob2DataProcessingApp()

# Statistical Modeling only, starting from processed data stored as a QFeatures 
# object followed with altSEE interactive visualisation
# 
launchMsqrob2ModelingApp()

# End-to-end workflow with Data Processing, Statistical Modeling and altSEE 
# interactive visualisation
launchMsqrob2App()

# Interactive altSEE visualisation of QFeatures objects
launchMsqrob2ExploreApp()
```
