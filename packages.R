# Define a list of required packages for this project

packages <- c(
  "brant",
  "car",
  "descr",
  "dplyr",
  "easystats",
  "ggpubr",
  "gofcat",
  "gplots",
  "lemon",
  "MASS",
  "mice",
  "openxlsx",
  "ordinal",
  "pscl",
  "psych",
  "RColorBrewer",
  "sjlabelled",
  "sjmisc",
  "sjPlot",
  "sjstats",
  "tableone",
  "tidyverse",
  "VIM"
)

# Check if the required packages are installed
# If not installed, install them and then load the library
package.check <- lapply(
  packages,
  FUN = function(x) {
    # Check if the package is installed
    if (!require(x, character.only = TRUE)) {
      # Install the package with dependencies
      install.packages(x, dependencies = TRUE)
      
      # Load the package after installation
      library(x, character.only = TRUE)
    }
  }
)
