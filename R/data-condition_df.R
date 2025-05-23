#' A dataframe of the NMC conditions with some extra infromation
#'
#' The dataset contains the conditiosn up to date as of December 2023.
#' The relevant gazeet cna be found at:
#' https://www.nicd.ac.za/wp-content/uploads/2023/10/NMC-Government-Gazette.pdf
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{id}{Unique identifier for each entry (integer)}
#'   \item{age}{Randomly generated age values between 20 and 40 (integer)}
#'   \item{gender}{Gender of the individual (factor with levels "Male" and "Female")}
#' }
#' @examples
#' sample_data
#'
#' # View the first few rows
#' head(condition_df)
#'
#' # Summarize the data
#' summary(condition_df)
#'
#' @source Generated for demonstration purposes
"condition_df"

# Define the conditions and their respective groups
conditions <- c(
  "Acute Flaccid Paralysis", 
  "Acute rheumatic fever", 
  "Agricultural or stock remedy poisoning",
  "Botulism", "Cholera",
  "Crimean-Congo viral haemorrhagic fever (human)", "Ebola Virus (VHF)",
  "Lassa Fever Virus(VHF)", "Lujo Virus(VHF)", "Marburg Virus (VHF)",
  "Diphtheria", "Enteric fever (typhoid or paratyphoid fever)",
  "Food borne illness outbreak", "Listeriosis", "Malaria", "Measles",
  "Meningococcal Disease", "Pertussis", "Plague", "Rabies", "Smallpox",
  "VHF Other", "Rift Valley Fever", "Yellow Fever",
  #"Waterborne illness outbreak - UNDEFINED",
  "Congenital rubella syndrome",
  "Rubella", "Haemolytic uraemic syndrome (HUS)",
  "Agricultural or stock remedy poisoning", "Bilharzia (schistosomiasis)",
  "Brucellosis", "Congenital syphilis", "Haemophilus influenzae type B",
  "Hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis E", "Legionellosis",
  "Leprosy", "Lead poisoning", "Mercury poisoning",
  "Maternal death (pregnancy, childbirth and puerperium)",
  "Soil transmitted helminths", "Tetanus", "Tuberculosis:pulmonary",
  "Tuberculosis:extra-pulmonary", "Tuberculosis: extensively drug -resistant (XDR -TB)",
  "Tuberculosis: multidrug- resistant (MDR -TB)",
  "Endemic arboviral diseases Chikungunya virus",
  "Endemic arboviral diseases West Nile Virus",
  "Non-Endemic Arboviral Diseases: Zika Virus",
  "Non-Endemic arboviral diseases : Dengue Fever Virus",
  "Non-typhoidal Salmonellosis", "Shiga toxin-producing Escherichia coli",
  "Shigellosis", "Endemic arboviral diseases Sindbis Virus", "Congenital rubella syndrome", "Mpox"
)


category_1 <- c(
  # cat 1
  "Acute Flaccid Paralysis",
  "Acute rheumatic fever",
    "Agricultural or stock remedy poisoning",
  "Anthrax",
  "Botulism",
  "Cholera",
  "COVID-19",
  "Congenital rubella syndrome",
  "Diphtheria",
  "Enteric fever (typhoid or paratyphoid fever)",
  "Food borne illness outbreak",
  "Haemolytic uraemic syndrome (HUS)",
  "Listeriosis",
  "Malaria",
  "Ebola virus (vhf)",
  "Marburg virus (vhf)",
  "Measles",
  "Meningococcal Disease",
  "Mpox",
  "Pertussis",
  "Plague",
  "Poliomyelitis",
  "Rabies", # on guideline it is Rabies (Human)
  "Respiratory disease caused by a novel respiratory pathogen",
  "Rift valley fever (human)",
  "Rubella",
  "Smallpox",
  "Crimean-Congo viral haemorrhagic fever (human)", # i dont think this is in the guideline
  #"Waterborne illness outbreak - UNDEFINED", # i dont think this is in the guideline
  "Yellow Fever")

cat1_from_NMC_vector <- c(
  #"Marburg Virus (VHF)",
  #"Rabies"
)


category_1<- c(category_1 ,cat1_from_NMC_vector) %>%str_to_lower() %>%str_to_sentence

category_2<- c(
  #"Agricultural or stock remedy poisoning", # is now a category 2 NMC 
  "Bilharzia (schistosomiasis)",
  "Brucellosis",
  "Congenital syphilis",
  "Haemophilus influenzae type B",
  "Hepatitis A",
  "Hepatitis B",
  "Hepatitis C",
  "Hepatitis E",
  "Lead poisoning",
  "Legionellosis",
  "Leprosy",
  "Maternal death (pregnancy, childbirth and puerperium)",
  "Mercury poisoning",
  "Soil transmitted helminths",
  "Tetanus",
  "Tuberculosis: extensively drug -resistant (xdr -tb)"  ,
  "Tuberculosis: multidrug- resistant (mdr -tb)" ,
  "Tuberculosis:extra-pulmonary"    ,
  "Tuberculosis:pulmonary" )

category_2<-category_2%>%str_to_lower() %>%str_to_sentence

cat3_from_NMC_vector <- c(
  "Endemic arboviral diseases Chikungunya virus",
  "Endemic arboviral diseases Sindbis Virus",
  "Endemic arboviral diseases West Nile Virus",
  "Non-Endemic Arboviral Diseases: Zika Virus",
  "Non-Endemic arboviral diseases : Dengue Fever Virus",
  "Shiga toxin-producing Escherichia coli",
  "Shigellosis",
  "Non-typhoidal Salmonellosis"
)

category_3_from_guideline_vector<- c(
  "Gonorrhoea Ceftriaxone-resistant Neisseria gonorrhoea",
  "Endemic Arboviral Disease West Nile virus, Sindbis virus, Chikungunya virus",
  "Non-Endemic Arboviral disease Dengue fever virus, other imported arboviruses of medical importance",
  "Non-typhoidal Salmonellosis Salmonella spp. other than S. Typhi and S. Paratyphi",
  "Shiga toxin-producing Escherichia coli Shiga toxin-producing Escherichia coli",
  "Shigellosis Shigella spp.")

category_3<-c(cat3_from_NMC_vector, category_3_from_guideline_vector)%>%str_to_lower() %>%str_to_sentence

category_4<- c(
  "Carbapenemase-producing Enterobacteriaceae",
  "Vancomycin-resistant enterococci",
  "Staphylococcus aureus: hGISA and GISA",
  "Colistin-resistant Pseudomonas aeruginosa",
  "Colistin-resistant Acinetobacter baumanii",
  "Clostridium difficile"
)

# Concatenate all conditions into a single string


centres <- c(
  "Zoonotic and Parasitic Diseases",
  "Enteric Diseases",
  "Healthcare-Associated Infections, Antimicrobial Resistance & Mycoses",
  "HIV & STIs",
  "Respiratory Diseases and Meningitis",
  "Tuberculosis",
  "Vaccines and Immunology",
  "Public Health Surveillance and Response"
)

# Assign groups to conditions
#groups <- c(rep(1, 28), rep(2, 20), rep(3, 8))

# Create the data frame

hep_pattern <- "(?<=Hepatitis\\s)[A-Za-z]+"
#make a pattern for what is contained in brackets
in_brackets_pattern <- "\\((.*?)\\)"

before_brackets_pattern <- "(.*?)(?=\\()"


condition_df <- data.frame(condition = c( category_1, category_2, category_3, category_4),
                           nmccategories = c( rep (1, length(category_1)),
                                              rep(2, length(category_2)),
                                              rep(3, length(category_3)),
                                              rep(4, length(category_4)))
)%>%
  mutate( centre = case_when(
    condition %in% c("Ebola Virus (VHF)","Crimean-Congo viral haemorrhagic fever (human)",
                     "Lassa Fever Virus(VHF)", "Lujo Virus(VHF)", "Marburg Virus (VHF)",
                     "Malaria","Plague", "Rabies", "VHF Other", "Rift Valley Fever", "Yellow Fever",
                     "Endemic arboviral diseases Chikungunya virus",
                     "Endemic arboviral diseases West Nile Virus",
                     "Non-Endemic Arboviral Diseases: Zika Virus",
                     "Non-Endemic arboviral diseases : Dengue Fever Virus",
                     "Endemic arboviral diseases Sindbis Virus", "Brucellosis")
    ~  "Zoonotic and Parasitic Diseases",
    condition %in% c( "Non-typhoidal Salmonellosis", "Shiga toxin-producing Escherichia coli",
                      "Shigellosis","Cholera" , "Enteric fever (typhoid or paratyphoid fever)",
                      "Food borne illness outbreak", "Listeriosis"#,
                      #"Waterborne illness outbreak - UNDEFINED"
    )
    ~   "Enteric Diseases",

    condition %in% c( ) ~  "Healthcare-Associated Infections, Antimicrobial Resistance & Mycoses",
    condition %in% c("Congenital syphilis" ) ~  "HIV & STIs",
    condition %in% c( "Meningococcal Disease", "Pertussis", "Legionellosis" ) ~  "Respiratory Diseases and Meningitis",
    condition %in% c( "Tuberculosis:pulmonary",
                      "Tuberculosis:extra-pulmonary", "Tuberculosis: extensively drug -resistant (XDR -TB)",
                      "Tuberculosis: multidrug- resistant (MDR -TB)") ~  "Tuberculosis",
    condition %in% c("Hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis E" ,"Congenital rubella syndrome",
                     "Rubella") ~  "Vaccines and Immunology",
    condition %in% c(    "Carbapenemase-producing Enterobacteriaceae",
                         "Vancomycin-resistant enterococci",
                         "Staphylococcus aureus: hGISA and GISA",
                         "Colistin-resistant Pseudomonas aeruginosa",
                         "Colistin-resistant Acinetobacter baumanii",
                         "Clostridium difficile") ~   "Healthcare-Associated Infections, Antimicrobial Resistance & Mycoses",
    .default = "Public Health Surveillance and Response"
  ))%>%
  mutate(
    condition = ifelse(grepl("hepatitis", ignore.case = TRUE, condition),

                       paste0( "Hepatitis ", str_to_upper(str_extract(condition, hep_pattern))),

                       condition),
    condition = ifelse(grepl("xdr|mdr|vhf|hus", ignore.case = TRUE, condition),

                       paste0( str_extract(condition , before_brackets_pattern ), str_to_upper(str_extract(condition, in_brackets_pattern))),

                       condition),

    condition = gsub("*type b*", "type B", condition)
  )
# Display the data frame
#condition_df%>%view()

#provinces<- df%>%select(prov_) %>%unique%>%pull



provinces <- c("Western Cape", "North West", "Mpumalanga", "Limpopo", "KwaZulu Natal", "Gauteng", "Free State", "Eastern Cape")
contact_person <- c("Janine Bezuidenhoudt", "Wellington Maruma", "Naume Tebeila", "Unarine Makungo", "Moshibudi Poncho Bapela", "Nomathamsanqa Ndhlovu", "Motshabi Modise", "Ruvimbo Chingonzoh")
phone_number <- c("0823270394", "0729393614", "0609870121", "0609634760", "0716816683", "0609634605", "0826163642", "0609634824")
email <- c("JanineB@nicd.ac.za", "WellingtonM@nicd.ac.za", "NaumeT@nicd.ac.za", "UnarineM@nicd.ac.za", "PonchoB@nicd.ac.za", "NomathamsanqaN@nicd.ac.za", "MotshabiM@nicd.ac.za", "RuvimboC@nicd.ac.za")

prov_epis<- data.frame(province = c(provinces),
                       provincial_epi = c(rep(NA, length(provinces))),
                       email = c(rep(NA, length(provinces) ))
)

centre_contacts<- data.frame(province = c(centres),
                             contact1_name = c(rep(NA, length(centres))),
                             contact_1_email =  c(rep(NA, length(centres))),
                             contact2_name  = c(rep(NA, length(centres) )),
                             contact2_email  = c(rep(NA, length(centres) ))
)

#write.xlsx(prov_epis, file = "contact_sheets/prov_epis.xlsx")
#write.xlsx(condition_df, file = "contact_sheets/nmc_centres.xlsx")
#write.xlsx(centre_contacts, file = "contact_sheets/centre_contacts.xlsx")
#save(condition_df, file= "contact_sheets/condition_df.rda")


usethis::use_data(condition_df, overwrite = TRUE)

condition_df%>%filter( nmccategories==1)
