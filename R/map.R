# Map 

# MAke a MAP? ----
library(sf)
#install.packages("sf")
# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
#install.packages("rgdal")
#library(rgdal)


# Read the subdistrict shapefile and filter for Free State province

#subdist_shape <- st_read("~/Desktop/SAFETP/CLA/2.2 HDR (Mortality surveillance)/HDR_Covid_RiskFactors/shapes/zaf_admbnda_adm_sadb_ocha_20201109.shp")

#save(subdist_shape, file = "subdist_shape.rda")

load("subdist_shape.rda")
subdist_shape
#st_read("/Users/YourUsername/Desktop/SAFETP/CLA/2.2 HDR (Mortality surveillance)/HDR_Covid_RiskFactors/shapes/zaf_admbnda_adm2_sadb_ocha_20201109.shp")


prov_shape<- subdist_shape %>% 
  group_by(ADM1_ID) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


prov_shape

#fs_shape <- subdist_shape %>% filter(ADM1_EN == "Free State")

# Extract unique district names from your data frame
provinces <- df$prov_ %>% unique()

# Prepare the data for plotting

df_shape <- df %>% 
  group_by(ADM1_ID = prov_) %>%
  summarise(Cases = n()) %>% 
  mutate(ADM1_ID = factor(ADM1_ID, levels = provinces))%>%
  filter(!is.na(ADM1_ID))

df_shape$ADM1_ID
prov_shape$ADM1_ID
# Merge shape data with your summary data
shape_data <-fuzzyjoin::stringdist_inner_join(prov_shape, df_shape, by = "ADM1_ID", method = "jw", distance_col = "string_dist")%>%
  group_by(ADM1_ID.x) %>%filter( string_dist == min(string_dist))%>%
  mutate(text = paste0( ADM1_ID.y, "\nn=", ifelse(is.na(Cases), 0, Cases)))%>%
  rename(ADM1_ID = ADM1_ID.y)


# Plot the map using ggplot2
map<- ggplot(data = shape_data) +
  geom_sf( aes(fill = Cases)) +
  geom_sf_label(aes(label = text), size = 3) + 
  scale_fill_gradient(low = "lightyellow", high = "darkolivegreen") +  # Customize fill color gradient
  labs(#title = #"Covid Cases in Free State Districts",       # Add a title
    fill = "Number of Cases",                           # Legend title for fill color
    #caption = "Source: Your Data Source"
  ) +             # Caption/source information
  theme(text = element_text(family = "Century Gothic"))+
  theme_classic()                       # Use a minimal theme for the plot



map

