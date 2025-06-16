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

Install the `msqrob2gui` package from the statOmics branch. **For the
EBI course**, this is what you should run!

```r
remotes::install_github("statOmics/msqrob2gui", ref = "gui2modules")
```

Load the package.

```r
library(QFeaturesGUI)
```

## Launch app

The app can be launched by loading the package and running the
`launchMsqrob2App` function

```{r}
library(msqrob2gui)
launchMsqrob2App()
```
