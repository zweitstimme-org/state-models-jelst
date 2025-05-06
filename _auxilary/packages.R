rm(list = ls()) # clean the working environment 

p_required <- c("tidyverse",
                "haven",
                "lubridate",
                "openxlsx",
                "dlm", 
                "future.apply",
                "parallel", 
                "stringr", 
                "rstan",
                "rstanarm",
                "bayesplot",
                "progressr",
                "rvest",
                "httr",
                "zoo",
                "ggrepel",
                "xtable",
                "kableExtra",
                "stargazer",
                "ggrepel",
                "ggplot2")

packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
