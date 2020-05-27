# GUI implementation for the msqrob2 package for differentially expression analysis in mass spectrometry using the Features infrastructure

The `msqrob2` package ports and modernises the method presented in
[`MSqRob`](https://github.com/statOmics/MSqRob) and
[`MSqRobSum`](https://github.com/statOmics/MSqRobSum) to use the
[`Features`](https://rformassspectrometry.github.io/Features/articles/Features.html)
class infrastructure.

The `msqrob2gui` package implements shiny apps for [`msqrob2`](github.com/statOmics/msqrob2).

## Installation

```{r}
BiocManager::install("lgatto/Features")
BiocManager::install("statomics/msqrob2")
BiocManager::install("statomics/msqrob2gui")
```

## Launch app

The app can be launched by loading the package and running the `launchMsqrob2App` function

```{r}
library(msqrob2gui)
launchMsqrob2App()
```
