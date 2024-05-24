
if (interactive()) {
  require("devtools", quietly = TRUE)
  # automatically attaches usethis
}

load_all()
document()
test()
check()
# so when making a package ensure you, load_all, document(), also use_package() and use_import()

use_r("epicurve_df.R")
usethis::use_import_from("flextable","readxl")
usethis::use_import_from("glue")

usethis::use_import_from("glue", "glue_collapse")

load("data/condition_df.rda")
use_data(condition_df, overwrite = TRUE)

use_package("janitor", type = "depends")
use_package("tidyverse", type = "depends")
use_package("dplyr", type = "depends")
use_package("readxl", type = "depends")
use_package("sf", type = "depends")
use_package("ggplot2", type = "depends")
use_package("ggh4x", type = "depends")
use_package("knitr", type = "depends")
use_package("tinytex", type = "depends")
use_package("fuzzyjoin", type = "depends")
use_package("grates", type = "depends")
use_package("grates", type = "depends")



load_all()
document()
check()

packages <- c(
  "flextable",
  "readxl",
  "sf",
  "tidyverse",
  "dplyr",
  "ggplot2",
  "ggh4x",
  "knitr",
  "tinytex",
  "haven",
  "janitor",
  "lubridate",
  "grates",
  "forcats",
  "flextable",
  "magrittr",
  "gtsummary",
  "stringdist",
  "fuzzyjoin",
  "stringr"
)

for( i in packages){
  use_package(i, type = "depends")
}

# Loop through the list of packages and import them using usethis::import_from()

if (interactive()) {
  require("devtools", quietly = TRUE)
  # automatically attaches usethis
}

# To do list
#| add shape files
#| add population data and also files about how that was made (may ein .rmd)
#| improve documentation
#| add more tests

# This package should be able to clean data, have some helping functions to create an epicurve,
# and also be able to create a report


# Shape file

list.files("shape", pattern = ".shp", full.names = TRUE)

# Load the shape file
provinces <- sf::st_read("shape/zaf_admbnda_adm1_sadb_ocha_20201109.shp")

provinces<- provinces%>%rename(province = ADM1_EN,
                               prov = ADM1_ID)
provinces%>%names()
provinces<- provinces[,c(1:3,13,14)]

districts <- sf::st_read("shape/zaf_admbnda_adm2_sadb_ocha_20201109.shp")
districts
districts%>%names()

districts<-districts %>%
  rename(district = ADM2_EN,
         province = ADM1_EN,
         prov = ADM1_ID)

districts<- districts[,c(1:3,8,15,17)]
districts

sub_districts  <- sf::st_read("shape/zaf_admbnda_adm3_sadb_ocha_20201109.shp")

sub_districts<- sub_districts%>%
  rename(
         sub_district = ADM3_EN,
         district = ADM2_EN,
         province = ADM1_EN,
         prov = ADM1_ID)
sub_districts
sub_districts%>%names()
sub_districts <- sub_districts[,c(1:3,8,10,17,20)]

sub_districts

shape_files <-  list(list(provinces = provinces,
                    districts = districts,
                    sub_districts = sub_districts))

shape_files[['provinces']]<- provinces


save(shape_files, file = "data/shape_files.rda")

load("~/Desktop/SAFETP/CLA/23.NMC_reporting_clean/population_estimates/pop_expanded.rda")
names(pop_expanded)
pop_expanded%>%
  group_by(prov)

pop<-pop_expanded%>%na.omit

  select(-c(1:3)) %>%
  pivot_longer(cols = -c(1,2), names_to = "date", values_to = "pop") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

save(pop, file = "data/pop.rda")

load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

names(new_master)

anonymise<- function(data){

  anonymised<- data%>%
    #remove personal identifiers
    mutate(notifier_id = NULL,
           patient_id_no = NULL,
           patient_name = NULL,
           patient_surname = NULL,
           patient_dob = NULL,
           folder_no = NULL,
           patient_passport_no = NULL,
           patient_contact_no= NULL,
           specimen_bar_code = NULL,
           episode_number= NULL,
           notifier = NULL)

  return(anonymised)

}

#anonymise(new_master)%>%view()

sample<- new_master[c(sample(nrow(new_master),100, )),]%>%anonymise()

sample %>%view()

save(sample, file ="data/sample.rda")
