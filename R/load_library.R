#load library


# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load pacman
library(pacman)

# Use pacman to install and load packages
p_load(flextable,
  readxl,
  sf,
  tidyverse,
  dplyr,
  ggplot2,
  ggh4x,
  knitr,
  tinytex,
  haven,
  janitor,
  #summarytools,
  lubridate,
  grates,
  forcats,
  flextable,
  magrittr,
  gtsummary, 
  flextable
  #RecorLinkage
  #openai,
  #DescTools
)
