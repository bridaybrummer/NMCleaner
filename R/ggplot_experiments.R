

df1$condition <- factor(df1$condition, levels = rev(unique(df1$condition)))

condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , condition ) %>%arrange(n)%>%select(condition) %>%pull

prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , prov_) %>%arrange(-n)%>%select(prov_) %>%pull

dot_nmc1<-df1%>%filter(nmccategories ==1, !is.na(prov_)) %>%
  mutate(prov_ = factor(prov_ , 
                       levels = prov_levels_dot[1:9]),
         condition = factor(condition , 
                            levels = condition_levels_dot))%>%
  ggplot(., aes(x = prov_, y = condition, color = condition)) +
  geom_count(show.legend = FALSE) +
  scale_size_area(max_size = 15)+
  labs(#title = paste0("Notifications of category ", df1 %>%select(nmccategories)%>%unique() %>%pull(),   " by Province"),
       x = "Province", y = "Condition") +
  theme_minimal() +
  #  scale_color_brewer(palette = "Set1")+
 scale_color_viridis_d( option = "c") +
  theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
        panel.background = element_rect(fill = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8"),
        panel.grid = element_line(color = "#DDDDDD"),
        legend.background = element_rect(fill = "#F8F8F8"),
        legend.text = element_text(color = "#333333"),
        legend.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 16, #face = "bold",
                                  color = "#666666"),
        plot.subtitle = element_text(size = 12, color = "#666666"),
        plot.caption = element_text(size = 10, color = "#666666"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.key = element_blank()
        #legend.title = element_blank()
        ) +
  guides(color = guide_legend(nrow = 7))

dot_nmc1

save(dot_nmc1, file = "plots/dot_nmc1.rda" )

new_levels<- df1%>%filter(nmccategories == 2) %>%select(condition ) %>%unique%>%pull%>%as.character%>%sort(decreasing   = TRUE)

condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) ,
                             condition ) %>%arrange(n)%>%select(condition) %>%pull

prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) , 
                        prov_) %>%arrange(-n)%>%select(prov_) %>%pull



dot_nmc2<-df1%>%filter(nmccategories ==2, !is.na(prov_)) %>%
  mutate( condition = factor(condition, levels = new_levels))%>%
  mutate(prov_ = factor(prov_ , 
                        levels = prov_levels_dot[1:9]),
         condition = factor(condition , 
                            levels = condition_levels_dot))%>%
  ggplot(., aes(x = prov_, y = condition, color = condition)) +
  geom_count(show.legend = FALSE) +
  scale_size_area(max_size = 15)+
  labs(#title = paste0("Notifications of category 2 within Each Province"),
       x = "Province", y = "Condition") +
  theme_minimal() +
  #  scale_color_brewer(palette = "Set1")+
  scale_color_viridis_d( option = "C") +
  
  # i want to try different colors scales and palletes
  # scale_color_viridis_c(option = "C") +
  # scale_color_viridis_c(option = "D") +
  # scale_color_viridis_c(option = "E") +
   #scale_color_viridis_c(option = "F") +

  theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
        panel.background = element_rect(fill = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8"),
        panel.grid = element_line(color = "#DDDDDD"),
        legend.background = element_rect(fill = "#F8F8F8"),
        legend.text = element_text(color = "#333333"),
        legend.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 16, #face = "bold",
                                  color = "#666666"),
        plot.subtitle = element_text(size = 12, color = "#666666"),
        plot.caption = element_text(size = 10, color = "#666666"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.key = element_blank()
        #legend.title = element_blank()
  ) +
  guides(color = guide_legend(nrow = 10))

dot_nmc2
save(dot_nmc2, file = "plots/dot_nmc2.rda" )
#dot_nmc2


xtabs(~ condition , data = df1) 


## Number 1 is information for action. Are there outbreaks? What are trends over last month. 
## I actually think we need to restructure it to show how the month fits into the yearly trend. 

## This would require the entire years data and then a system whereby we add the next month/week to it. 

# Big one is differences between provinces. Show the endemic provinces. 
# Show trends of NMC diseases by line graph? 
# case source etc should all be 

# see if we can make average notifications per day of the week per month .

month(df1$Month_notification)

month.abb



df1 %>%mutate( day_of_week = factor(weekdays(notification_date), levels = c(weekdays(as_date(seq(11, 17, 1 ))))),
               Month_notification = factor( format(Month_notification, "%b"), levels = rev(month.abb))) %>%
  group_by( Month_notification, day_of_week) %>%
  summarise (n = n() ) %>%
  ggplot(., aes(x = Month_notification, y = day_of_week, )) +
  geom_count(aes(size = n), show.legend = , max_size = 10) +coord_flip()
  
  
### symptoms with count s
symptoms_split<- df1$symptoms%>%
    strsplit(., "\\|")%>%
    unlist()%>%unique()%>%na.omit()
  
  #tabyl(symptoms_split)%>%arrange(-n)%>%filter( )

df_symptoms <- df1 %>%
  separate_rows(symptoms, sep = "\\|") %>%
  filter(!is.na(symptoms))  # Remove NA values

# Create an empty list to store results
result_list <- list()

# Iterate over each symptom
for (symptom in unique(df_symptoms$symptoms)) {
  # Count occurrences of each symptom for each condition
  symptom_counts <- df_symptoms %>%
    filter(condition %in% category_1)%>%
    filter(symptoms == symptom) %>%
    group_by(condition) %>%
    summarise(count = n()) %>%
    mutate(symptom = symptom)
  
  # Print the result
  result_list<- bind_rows(symptom_counts, result_list)
  
  # Store the result in the list
  #result_list[[symptom]] <- symptom_counts
}

result_list%>%filter( condition %in% category_1)%>%
  ggplot(., aes(x = symptom, y =  condition, )) +
  geom_count(aes(size = count), show.legend = , max_size = 10) +coord_flip()

ggplot(result_list%>%filter( condition %in% category_1), aes(x = symptom, y = condition, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


mat <- pivot_wider(result_list, names_from = condition, values_from = count, values_fill = 0) %>%
  column_to_rownames(var = "symptom") %>%
  as.matrix()


order <- rev(order(rowSums(mat)))
order
# Relevel variables based on the order
result_list$symptom <- factor(result_list$symptom, levels = rownames(mat)[order])

result_list


ggplot(result_list , aes(x = symptom, y = condition, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability






