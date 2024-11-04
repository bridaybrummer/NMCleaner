#' Standardise District Names
#'
#' I would recommend aggregating data before using this function to reduce run time (even thought it is farly fast already)
#' Description: This script standardises district names across different datasets.
#' @param data The dataset, or pipe it in.
#' @param district_variable The name of the variable containing the district names.
#' @return A dataset with the district names standardised.
#' @export 
#' @examples
#' new_master[sample(1000: 1),]%>%
#'    mutate_district_name(., district_variable = "district")->
#'    master_subset
mutate_district_name <- function(data, district_variable = district_variable) {

        district_lookup <- tibble(
          district_standard = c(
            "Alfred Nzo", "Amathole", "Buffalo City", "Chris Hani", "Joe Gqabi", 
            "Nelson Mandela Bay", "O.R. Tambo", "Sarah Baartman", "Fezile Dabi",
            "Lejweleputswa", "Mangaung", "Thabo Mofutsanyana", "Xhariep",
            "City of Johannesburg", "City of Tshwane", "Ekurhuleni", "Sedibeng",
            "West Rand", "Amajuba", "Harry Gwala", "King Cetshwayo", "Ugu", 
            "Umzinyathi", "Zululand", "eThekwini", "iLembe", "uMgungundlovu", 
            "uMkhanyakude", "uThukela", "Capricorn", "Sekhukhune", "Mopani", 
            "Vhembe", "Waterberg", "Ehlanzeni", "Gert Sibande", "Nkangala", 
            "Frances Baard", "John Taolo Gaetsewe", "Namakwa", "Pixley ka Seme", 
            "ZF Mgcawu", "Bojanala Platinum", "Dr Kenneth Kaunda", "Dr Ruth Segomotsi Mompati", 
            "Ngaka Modiri Molema", "Cape Winelands", "Central Karoo", "City of Cape Town", 
            "Garden Route", "Overberg", "West Coast"
          ),
          district_variant = list(
            # Alfred Nzo
            c("Alfred Nzo", "Alfred Nzo District", "Alfred Nzo Municipality", "DC44", "Alfred Nzo DM", "AlfredNzo"),
            # Amathole
            c("Amathole", "Amathole District", "Amathole District Municipality", "DC12", "Amatole", "Amathol"),
            # Buffalo City
            c("Buffalo City", "Buffalo City Metro", "Buffalo Metropolitan", "Buffalo City Municipality", "BuffaloCity", "Buffalo"),
            # Chris Hani
            c("Chris Hani", "Chris Hani District", "Chris Hani District Municipality", "DC13", "C.Hani"),
            # Joe Gqabi
            c("Joe Gqabi", "Joe Gqabi District", "Joe Gqabi District Municipality", "DC14", "JGqabi"),
            # Nelson Mandela Bay
            c("Nelson Mandela Bay", "NMB", "Nelson Mandela Bay Metro", "Nelson Mandela Metropolitan Municipality"),
            # O.R. Tambo
            c("O.R. Tambo", "OR Tambo", "O.R.Tambo District", "DC15", "ORTambo", "o.r.tambo", "o r tambo"),
            # Sarah Baartman
            c("Sarah Baartman", "Sarah Baartman District", "Cacadu", "Sarah Baartman Municipality", "DC10"),
            # Fezile Dabi
            c("Fezile Dabi", "Fezile Dabi District", "Fezile Dabi Municipality", "DC20", "FezileD"),
            # Lejweleputswa
            c("Lejweleputswa", "Lejweleputswa District", "Lejweleputswa Municipality", "DC18", "Lejwe"),
            # Mangaung
            c("Mangaung", "Mangaung Metro", "Mangaung Municipality", "MAN", "Mangaung Metropolitan"),
            # Thabo Mofutsanyana
            c("Thabo Mofutsanyana", "Thabo Mofutsanyana District", "Thabo Mofutsanyane Municipality", "DC19", "Thabo M", "thabo mutsanyane", "thabo mutsanyana"),
            # Xhariep
            c("Xhariep", "Xhariep District", "Xhariep Municipality", "DC16"),
            # City of Johannesburg
            c("City of Johannesburg", "Johannesburg Metro", "Johannesburg Metropolitan Municipality", "Johannesburg", "JHB"),
            # City of Tshwane
            c("City of Tshwane", "Tshwane Metro", "Tshwane Metropolitan Municipality", "Tshwane", "Pretoria"),
            # Ekurhuleni
            c("Ekurhuleni", "Ekurhuleni Metro", "Ekurhuleni Metropolitan Municipality", "EKU"),
            # Sedibeng
            c("Sedibeng", "Sedibeng District", "Sedibeng Municipality", "DC42"),
            # West Rand
            c("West Rand", "West Rand DM", "West Rand District", "DC48", "WR"),
            # Amajuba
            c("Amajuba", "Amajuba District", "Amajuba Municipality", "DC25"),
            # Harry Gwala
            c("Harry Gwala", "Harry Gwala District", "Sisonke", "Harry Gwala Municipality", "DC43"),
            # King Cetshwayo
            c("King Cetshwayo", "King Cetshwayo District", "Uthungulu", "King Cetshwayo Municipality", "DC28"),
            # Ugu
            c("Ugu", "Ugu District", "Ugu Municipality", "DC21"),
            # Umzinyathi
            c("Umzinyathi", "Umzinyathi District", "Umzinyathi Municipality", "DC24"),
            # Zululand
            c("Zululand", "Zululand District", "Zululand Municipality", "DC26"),
            # eThekwini
            c("eThekwini", "eThekwini Metro", "Durban", "eThekwini Municipality", "eThekwini Metropolitan"),
            # iLembe
            c("iLembe", "iLembe District", "iLembe Municipality", "DC29"),
            # uMgungundlovu
            c("uMgungundlovu", "Umgungundlovu", "uMgungundlovu District", "DC22", "Umgung"),
            # uMkhanyakude
            c("uMkhanyakude", "Umkhanyakude", "uMkhanyakude District", "DC27", "Mkhanyakude"),
            # uThukela
            c("uThukela", "Uthukela", "uThukela District", "DC23", "Thukela"),
            # Capricorn
            c("Capricorn", "Capricorn District", "Capricorn Municipality", "DC35"),
            # Sekhukhune
            c("Sekhukhune", "Greater Sekhukhune", "Sekhukhune District", "Sekhukhune Municipality", "DC47"),
            # Mopani
            c("Mopani", "Mopani District", "Mopani Municipality", "DC33"),
            # Vhembe
            c("Vhembe", "Vhembe District", "Vhembe Municipality", "DC34"),
            # Waterberg
            c("Waterberg", "Waterberg District", "Waterberg Municipality", "DC36"),
            # Ehlanzeni
            c("Ehlanzeni", "Ehlanzeni District", "Ehlanzeni Municipality", "DC32"),
            # Gert Sibande
            c("Gert Sibande", "Gert Sibande District", "Gert Sibande Municipality", "DC30", "GSibande"),
            # Nkangala
            c("Nkangala", "Nkangala District", "Nkangala Municipality", "DC31"),
            # Frances Baard
            c("Frances Baard", "Frances Baard District", "Frances Baard Municipality", "DC9"),
            # John Taolo Gaetsewe
            c("John Taolo Gaetsewe", "John Taolo Gaetsewe District", "John Taolo", "DC45"),
            # Namakwa
            c("Namakwa", "Namakwa District", "Namakwa Municipality", "DC6"),
            # Pixley ka Seme
            c("Pixley ka Seme", "Pixley ka Seme District", "Pixley ka Seme Municipality", "DC7"),
            # ZF Mgcawu
            c("ZF Mgcawu", "Z F Mgcawu", "ZF Mgcawu District", "DC8"),
            # Bojanala Platinum
            c("Bojanala Platinum", "Bojanala", "Bojanala District", "DC37"),
            # Dr Kenneth Kaunda
            c("Dr Kenneth Kaunda", "Dr Kenneth Kaunda District", "DC40", "D.K.Kaunda"),
            # Dr Ruth Segomotsi Mompati
            c("Dr Ruth Segomotsi Mompati", "Dr Ruth Mompati", "DC39", "Ruth Mompati"),
            # Ngaka Modiri Molema
            c("Ngaka Modiri Molema", "Ngaka Modiri", "DC38", "Modiri Molema"),
            # Cape Winelands
            c("Cape Winelands", "Cape Winelands District", "DC2", "CapeWinelands"),
            # Central Karoo
            c("Central Karoo", "Central Karoo District", "Central Karoo Municipality", "DC5"),
            # City of Cape Town
            c("City of Cape Town", "Cape Town Metro", "Cape Town
                Metropolitan Municipality", "Cape Town", "CPT"),
            # Garden Route
            c("Garden Route", "Garden Route District", "Garden Route Municipality", "DC4", "Eden"),
            # Overberg
            c("Overberg", "Overberg District", "Overberg Municipality", "DC3"),
            # West Coast
            c("West Coast", "West Coast District", "West Coast Municipality", "DC1")
            )
        )   -> 

        district_names_db

        # make v look up function that returns the standard district name


        vlookup <- function(x) {
          x <- str_to_lower(x)
          for (i in 1:nrow(district_lookup)) {
            if (x %in% str_to_lower(district_lookup$district_variant[[i]])) {
              return(district_lookup$district_standard[i])
            }
          }
          return(NA)
        }

        # test the function
        vlookup("Overberg")
        vlookup("Cacadu")
        vlookup("o.r.tambo")

        # Save this as a funciton in the NMCleaner package and also impleemnt it on the population  and shape data, then create th dashboard. 

        # get common words that could be confusing  and exclude them from the names 
        # Commented out code
        # c(pop_district_names, nmc_districts) %>%
        #     tolower()%>%
        #     str_split( " ")%>%
        #     map( ~data.frame( word = .x, stringsAsFactors = FALSE))%>%
        #     bind_rows()%>%
        #     count( word )%>%
        #     arrange( -n)%>%
        #     dplyr::filter( n > 4) %>%# so things like west and cape are kept * also note the "of " in thabo mofutsanyane
        #     pull( word )%>%
        #     str_c( collapse = paste0("|"))-> 
        #     common_words
        # 
        # common_words
        # 
        # c("municipality|district|city|metro|metropolitan|of")-> common_words
        # 
        # rep( NA, 0)
        # tibble( 
        #     district_pop = c(pop_district_names%>%str_to_lower%>%gsub( "\\(.*", "", . )%>%gsub( common_words,  "", . ) %>%trimws()%>%sort(),  rep(NA, length(  nmc_districts)- length(pop_district_names)) ) ,
        #     district_shape = c(shape_district_names%>%str_to_lower%>%gsub( "\\(.*", "", . )%>%gsub( common_words,  "", . ) %>%trimws()%>%sort(), rep(NA, length( nmc_districts)- length(shape_district_names))),
        #     district_nmc = nmc_districts%>%str_to_lower%>%gsub( "\\(.*", "", . )%>%gsub( common_words,  "", . ) %>%trimws()%>%sort()
        # ) %>%
        #     mutate( 
        #         district_standard_pop = map_chr( district_pop, vlookup),
        #         district_standard_shape = map_chr( district_shape, vlookup),
        #         district_standard_nmc = map_chr( district_nmc, vlookup)
        #     )%>%arrange( district_standard_pop) ->
        #     pop_districts
        # 
        #     pop_districts[c("district_pop", "district_standard_pop", "district_nmc","district_standard_nmc", "district_shape", "district_standard_shape" )]%>%view()
        # 
        # pop_districts%>%print(n = 100)
        # 
        # library(tidytext)
        # library(tidyverse)
        # 
        # common_words

        # Make this into a function that will take some sort of district name and return its standardised version 


    
    c("municipality|district|city|metro|metropolitan|of") -> common_words
    
    data %>%
        mutate(
            clean_district_string = c(data[, district_variable] %>% pull %>% str_to_lower %>% gsub("\\(.*", "", .) %>% gsub(common_words, "", .) %>% trimws()),
            district_standard = map_chr(clean_district_string, vlookup)
        ) %>%
        select(-clean_district_string) ->
        data
    
    return(data)
}

