#download population estimates from StatsSA

pop_directory <- "population_estimates"

# Create a new directory
if (file.exists(pop_directory)) {
  # If it exists, remove it
  unlink(pop_directory, recursive = TRUE)
}

dir.create(pop_directory)

# Check if the directory was created successfully
if (file.exists(pop_directory) && file.info(pop_directory)$isdir) {
  cat("New directory created:", pop_directory, "\n")
} else {
  cat("Failed to create the directory:", pop_directory, "\n")
}

#read in the datasets but reduce their size immediately.
url <- "https://www.statssa.gov.za/publications/P0302/District_Council_projection_by_sex_and_age_2002_2022.xlsx"
url_projection <- "https://www.statssa.gov.za/publications/P0302/District_projections_by_sex_and_age_2023_2027.xlsx"



download.file(url, destfile=paste0(pop_directory,"/pop.xlsx"), mode="wb")

download.file(url_projection, destfile=paste0(pop_directory,"/projection.xlsx"), mode="wb")



