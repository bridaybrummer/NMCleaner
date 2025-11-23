# Purpose: 
#  - This script will show the sources via URL of shape files aswell as cleaning processes for province, district and subdistrict shape files 

NMCleaner::shape_files$sub_district%>%names()

library(sf)
library(dplyr)
library(tidyverse)

read_sf("shape/zaf_admbnda_adm1_sadb_ocha_20201109.shp") # provinces
read_sf("shape/zaf_admbnda_adm2_sadb_ocha_20201109.shp") # districts
read_sf("shape/zaf_admbnda_adm3_sadb_ocha_20201109.shp") -> subdist_shape # sub_districts
read_sf("shape/zaf_admbnda_adm4_sadb_ocha_20201109.shp") -> subdist4_shape # sub_districts

subdist4_shape%>%
    ggplot() +
    geom_sf()

subdist4_shape
subdist4_shape %>%
    filter( 
        AMD4_EN %in% "ekurhuleni"
    )


subdist_shape %>% glimpse()
subdist_shape$ADM1_EN %>% unique()

# rename vars
subdist_shape %>%
    rename(
        sub_district = ADM3_EN,
        district = ADM2_EN,
        province = ADM1_EN
    ) %>%
    mutate(
        across(c(sub_district, district, province), str_to_lower)
    ) -> subdist_shape

subdist_shape %>%  
    mutate_provinces(.,
        fuzzy = TRUE
    ) %>%
        mutate_district_name_v2(.,
            fuzzy = TRUE
        ) %>%
        mutate_sub_district(.,
            subdistrict_variable = "sub_district",
            use_fuzzy = TRUE
        ) ->
    shape_standardised

    shape_standardised%>% names() 


mutate_sub_district(data.frame(subdistrict = "pixley ka seme"))
mutate_sub_district(data.frame(subdistrict = "js moroka"))


shape_standardised%>%select( 
    subdistrict_standard, 
    district_standard, 
    province_standard, 
    prov, 
    geometry, 
    Shape_Leng,
    Shape_Area
)-> sub_district_shape_select

sub_district_shape_select%>%
    st_as_sf() %>%  
    ggplot() +
    geom_sf() 


# how well do pop and sub_district shape match now? 
# From the pepate_pop_files.R
# using tidyverse
pop_sub_dist_summary %>%    
    filter( year %in% "2025")%>%
    group_by( subdistrict_standard)%>%
    summarise( pop = sum(pop) )%>%
    arrange(subdistrict_standard) -> pop_2025

    pop_2025

pop_sub_dist_summary%>%setDT() 

# using data.table
setorder(pop_sub_dist_summary[year == 2025,.(pop = sum(pop)),  by = c("subdistrict_standard", "district_standard", "province_standard")], subdistrict_standard)-> pop_2025_dt
pop_2025_dt



pop_2025_dt[grepl("moroka", subdistrict_standard), ]
pop_2025_dt[grepl("sekh", ignore.case = TRUE, district_standard), ]
pop_2025_dt[grepl("limp", ignore.case = TRUE, province_standard), ]

left_join( 
sub_district_shape_select, 
pop_2025_dt, 
by = c("province_standard", "district_standard", "subdistrict_standard")
)%>%    
filter( is.na(pop))

# now we need to look at NMC data 
new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")

new_master%>%   
group_by( 
    prov_, district, sub_district
)%>%
reframe( n = n() )%>%
arrange( -n) -> 
master_agg

master_agg

new_master %>%setDT

new_master[, .(n = .N), by = c( "prov_", "district", "sub_district")]%>%na.omit()-> master_agg_dt

master_agg_dt%>%arrange( - n)

sum(master_agg_dt$n)


# Standardise 
master_agg_dt %>%
    mutate_provinces(.,
        province_col = "prov_",
        fuzzy = TRUE
    ) %>%
        mutate_district_name_v2(.,
            fuzzy = TRUE
        ) %>%
        mutate_sub_district(.,
            subdistrict_variable = "sub_district",
            use_fuzzy = TRUE
        ) -> master_agg_dt_standard

master_agg_dt_standard%>% 
    group_by( 
        subdistrict_standard, district_standard, province_standard
    )%>%reframe( 
        n = sum(n)
    )%>%
    arrange( -n) 

master_agg_dt_standard[ , .(n = sum(n), by = c("subdistrict_standard", "district_standard", "province_standard"))]

# look at hessequa
generate_canonical_districts()-> districts 

mutate_district_name_v2(data.frame(district = "ekhurhuleni north 1"), fuzzy = TRUE)


sub_district_shape_select
sub_district_shape_select[grepl("ekur", sub_district_shape_select$subdistrict_standard), ]
pop_2025_dt[grepl("kaun", subdistrict_standard), ]

mutate_district_name_v2( data.frame(district = c("greater sekhukhune", "kenneth kaunda", "ekurhuleni")))
mutate_sub_district(data.frame( subdistrict = "ekurhuleni north 1"), use_fuzzy =  TRUE)



# there needs ot be a fair amoutn fo work in standardising sub-dsitricts 

# I think the plan will be to go metro by metro, find a shape file that uses a similar naming cheme and place those shapse inside the metro area that aligns with NMC. 

