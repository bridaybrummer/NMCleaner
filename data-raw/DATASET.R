## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(openxlsx)

# this should be used as the main reference for diseases. We should also be able to make a table for these for the relevant centres and contacts.
# it should probbaly be stored on github aswell


###
infectious_diseases <-
  tibble::tibble(
    condition = "Cholera", "Measles",
    mean_incubation_period = c(4, 2),
    Rt = c(2.5, 3.5),
    generation_time = c(10, 12),
    refs = c("WHO", "CDC")
  )%>%
  save(., file = "data/infectious_diseases.rda")

usethis::use_data(infectious_diseases, overwrite = TRUE)
