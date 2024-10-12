
if (!"pacman" %in% installed.packages()[,"Package"]){
  install.packages("pacman")
}
library(pacman)
install_these <- c("tidyverse","readxl","data.table","collapse","tictoc","tidyfast")

if(sum(!p_isinstalled(install_these))>0) {
  install_these <-  packages_CRAN[!p_isinstalled(install_these)]
  for (i in 1:length(install_these)){
    install.packages(install_these[i],
                     dependencies = "Depends")
  }
}

p_load(install_these, character.only = TRUE)

source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_sensitivity_functions.R")

