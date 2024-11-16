packages <- c(
  "tidyverse", 
  "dplyr", 
  "descr", 
  "sjmisc", 
  "sjlabelled", 
  "sjPlot", 
  "sjstats", 
  "easystats", 
  "ggpubr", 
  "psych",
  "tableone", 
  "mice", 
  "VIM",
  "car",
  "MASS",
  "brant",
  "gofcat",
  "pscl",
  "ordinal",
  "openxlsx",
  "gplots",
  "lemon"
)

## Now load or install & load all if necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

