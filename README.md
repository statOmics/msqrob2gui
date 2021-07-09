# GUI implementation for the msqrob2 package for differentially expression analysis in mass spectrometry using the Features infrastructure

The `[`msqrob2`](https://www.bioconductor.org/packages/release/bioc/html/msqrob2.html) Bioconductor package ports and modernises the method presented in
[`MSqRob`](https://github.com/statOmics/MSqRob) and
[`MSqRobSum`](https://github.com/statOmics/MSqRobSum) to use the
[`QFeatures`](https://www.bioconductor.org/packages/release/bioc/html/QFeatures.html)
class infrastructure.

The `msqrob2gui` package implements shiny apps for Bioconductor package [`msqrob2`](https://www.bioconductor.org/packages/release/bioc/html/msqrob2.html).

## Installation

```{r}
if(!requireNamespace("BiocManager", quietly = TRUE)) {
 install.packages("BiocManager")
}
BiocManager::install("msqrob2")
BiocManager::install("statomics/msqrob2gui")
```

## Launch app

The app can be launched by loading the package and running the `launchMsqrob2App` function

```{r}
library(msqrob2gui)
launchMsqrob2App()
```

## Use App

<iframe width="560" height="315"
src="https://www.youtube.com/embed/F5d0E-U4rOM"
frameborder="0"
style="display: block; margin: auto;"
allow="autoplay; encrypted-media" allowfullscreen></iframe>   
