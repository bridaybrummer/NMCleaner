devtools::load_all()
devtools::document()

use_data()
# load datasets to be used in the script

# Province abbreviation is easy 

# load the population data 
NMCleaner::pop$Name%>%
    gsub( ".*- ", "", . )%>%unique-> 
pop_district_names 

# load shape file 
shape_files$districts$district-> 
    shape_district_names

# oad nmc database 
load("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.rda")

# Exclude some random district names from NMC 
new_master%>%
    dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
    select(district)%>%unique%>%pull() -> 
    nmc_districts

nmc_districts%>%length()


# can we do three runs for V look up 
? NMCleaner::mutate_district_name


# for using this unction with NMC it is probably advisable to apply it once data is grouped to reduce potenitaly run time 
# So now technically you should be able to create an incidence of NMCs per district in SA 

# district notifications in 2024. 

# So now, I should be able to group the data

new_master %>% 
   dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
   dplyr:: filter( !condition %in% "Covid-19", 
    nmccategories ==1 , 
    year %in% 2024)%>%
    group_by( district) %>%
    summarise( n = n())%>%
    mutate_district_name( ., "district")->
    notifications_df

notifications_df

#standardise pop data 
NMCleaner::pop$Name
NMCleaner::pop%>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    mutate_district_name(., "district")-> 
    pop

pop$district_standard%>%unique()

pop %>% 
    mutate( district = district_standard)->
    pop

usethis::use_data( pop, overwrite = TRUE)

save( pop, file = "data/pop.rda")


NMCleaner::pop%>%
    dplyr::filter( Year %in% 2024) %>%
    mutate( district = gsub( ".*- ", "", Name ))%>%
    select( - Name)%>%
    group_by( district)%>%
    summarise( pop = sum( Population))%>%
    mutate_district_name(., "district")-> 
    pop_df 


shape_files[["districts"]]$district%>%unique
shape_files[["sub_districts"]]$district%>%unique

# standardsie the district names in these two datasets. 
shape_files[c("districts", "sub_districts")] <- 
  map(c("districts", "sub_districts"), ~
    shape_files[[.x]] %>%
      as_tibble() %>%
      mutate_district_name("district") %>%
      st_as_sf()
  )

usethis::use_data( shape_files, overwrite = TRUE)
save( shape_files, file = "data/shape_files.rda")


shape_files$districts%>%
    as_tibble() %>%
    mutate_district_name(., "district")%>%
    st_as_sf()->
    shape_df

shape_df%>%
    left_join( pop_df, by = "district_standard")%>%
    left_join( notifications_df, by = "district_standard")%>%
    mutate( incidence = n/pop*100000)->
    final_df

final_df %>% 
    ggplot()+
    geom_sf(aes(fill = incidence, group = district))+
    geom_sf_label(aes(label = district_standard), size = 2)+
    scale_fill_viridis_c()+
    theme_minimal()->
    notifications_map 

notifications_map%>%plotly::ggplotly()


# Can we make an epicurve of incidence? 
# first get case coutns with df_epicurve

new_master %>% 
    dplyr::filter(! district %in% c("Unknown", "Isazi Import", "Not Applicable"))%>%
   dplyr:: filter( !condition %in% "Covid-19", 
    nmccategories ==1 , 
    year %in% 2024)%>%
    epicurve_df(., 
    date_index = "notification_date", 
    grouping_vars = "district")->
df_for_epicurve

df_for_epicurve%>%
    mutate_district_name(., "district")%>%
    left_join( pop_df, by = "district_standard")%>%
    mutate( incidence = n/pop*100000)->
df_for_epicurve

df_for_epicurve
?plot_epicurve_df

df_for_epicurve%>%
        mutate( 
            n = incidence,
            district = district_standard) %>%
            filter( year %in% as.factor(2024))-> 
df_for_epicurve1

df_for_epicurve1%>%
    plot_epicurve_df(
        .,
        x_axis_option = "epiweeks",
        grouping_vars = "district", 
        add_rolling_avg = c(FALSE, 7 )
    )->
epicurve


epicurve$plot

epicurve$plot+
    theme( 
        strip.text.y = element_text(hjust = 0 )
    )

epicurve$data

# Find the peaks of the data 
epicurve$data %>% 
# maybe in the last 5 weeks? 
    arrange( -n) %>%
    slice(1:10 )%>%
    group_by( district )%>%
    filter( n ==max(n))-> 
peaks_df
    
peaks_df%>%
    select(district)%>%
    pull()%>%
    unique()-> top_districts 

epicurve$data%>%
#mutate( date = as_date(paste0( year, "-", month, "-", day)))%>%
ggplot() + 
    geom_line( aes( x = epiweek, y = n, color = district, group = district) )+
    geom_text(
        check_overlap =  TRUE, 
        data = peaks_df, 
        aes(x = epiweek, y = n, label = district))+
        labs( y = "incidence per 100 000")+
    theme( legend.position = "bottom")-> 
incidence_plot

incidence_plot+
theme_classic()+
theme( legend.position = "bottom")

incidence_plot%>%plotly::ggplotly()

# So this tells us there were a pile of things notifiied in a few districts 

# namely namakwa, ZF mgcawu, and uthukela and pixley ka seme

# this shoudl also be shown in some kind of table to show you the places with the highest incidence of notifications

new_master %>% 
    filter( nmccategories ==1, 
    !condition %in% "Covid-19",
    year %in% 2024,
     district %in%top_districts)%>%
    select( condition, district) %>% 
    tbl_summary( by = district, missing = "no", sort = list( condition = "frequency"))

# look at nevashans table:
# By province. 
    # Get incidence per 100 000 % change in last 7 days (compared to what?)

new_master%>%
    epicurve_df(
         .,
          date_index = "notification_date",
          grouping_vars = "prov_")%>%
          filter( year %in% as.factor( "2024"))-> 
df_for_epicurve

df_for_epicurve

# calcultate incidence per 100 000

shape_files$provinces%>% 
    mutate( prov = if_else( prov == "LIM", "LP", prov))%>%
    
    left_join(
        ., 
        df_for_epicurve,
        by =c( "prov" = "prov_")
    )-> 
    shape_df

pop%>% filter( Year == 2024)%>%
    group_by( prov)%>%
    summarise( pop = sum( Population))%>%
    mutate_district_name(., "prov")%>%
    left_join( df_for_epicurve, by = c("prov"= "prov_"))%>%
    mutate( 
        incidence = n/pop*100000,
        cumulative_incidence  = cumulative/pop * 100000)->
    prov_incidence_df

prov_incidence_df[c("prov", "pop", "n", "incidence", "date")]%>%print(n = 9)

prov_incidence_df$prov%>%unique()
# Calcualte smallest part first, incidence per day 

conflicted::conflicts_prefer(dplyr::lag)

stats::poisson.test( x = 1, T = 1000)

stats::prop.test( x = 45, n = 100)
stats::poisson.test( x = 45, T = 100)
?stats::t.test
stats::t.test( x = -45:45, n = 100)
stats::chisq.test( x = c(45, 55), p = c(0.5, 0.5))
stats::dnbinom( x = 45, size = 100, mu = 50)



c(seq( 10, 1000, 10 ), seq( 1000, 10 , -10))-> increase_case_counts 
# calc perc changes 
tibble( 
    x = seq( 1, length(increase_case_counts), 1),
    n = increase_case_counts)%>%
    mutate( 
        seven_day_avg = rollmean( n, 7, fill = NA, align = "right"),
        seven_day_avg_change = ((lag(seven_day_avg,0) /lag(seven_day_avg, 1))*100),
        # get the variance of the last 7 case counts for a CI to compute the CI of the percentage change
        ci_lower = 
            lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 7, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "left")[,1],0),
        ci_upper = 
            lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 7, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "left")[,2],0),
        prev_seven_day_avg = rollapply(n, width = 10, FUN = mean, fill = NA, align = "right"))%>%
        filter( !is.na(seven_day_avg_change))->

        demo_increase

    demo_increase%>%
        ggplot( 
            data = . ,
        )+
        geom_col( aes( x = x, y = n ))+
        geom_line(aes(x = x, y = seven_day_avg_change))
        
    
        # Calculate the percentage change compared to the average of the previous 7 days
    
    demo_increase%>%
        ggplot( 
            data = . ,
            aes( x = x , y = seven_day_avg_change))+
        geom_ribbon(group = 1, aes(  ymin = ci_lower, ymax = ci_upper)) +
        geom_line()+
        theme_classic()
        
    
        # Calculate the percentage change compared to the average of the previous 7 days
    )%>%
    rowwise()%>%
    mutate(
        seven_day_avg_change = lead((seven_day_avg / prev_seven_day_avg ) * 100, 0) 
    )-> 
        demo_increase

demo_increase%>%view()
demo_increase$seven_day_avg_change%>%round()%>%abs()

demo_increase%>% mutate( 
    upper_ci = lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 1, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "right")[,1],0),
    lower_ci = lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 1, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "right")[,2],0)
    )%>%view() 
# it might be best to just use some kind f distribution form the poisson and make it into a percentage. 

wind_and_lag<- 14
prov_incidence_df1<- NULL
prov_incidence_df$incidence



prov_incidence_df %>%
    #group_by( prov)%>%
       # rowwise() %>%
        mutate( 
            seven_day_avg = rollmean( incidence, 7, fill = NA, align = "right"),
            seven_day_avg_change = round(((seven_day_avg - lag(seven_day_avg, 7))/lag(seven_day_avg, 7)*100)),
            seven_day_avg_change = round((seven_day_avg - rollapply(seven_day_avg, width = 7, align = "right", 
                                                            FUN = function(x) mean(lag(x, n = 1), na.rm = TRUE))) /
                                 rollapply(seven_day_avg, width = 7, align = "right", 
                                           FUN = function(x) mean(lag(x, n = 1), na.rm = TRUE)) * 100),
    
            # add a negative binomial confidene interval to this 
            #seven_day_avg_change_CI = 
            upper_ci = lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 1, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "right")[,1],0),
            lower_ci = lag(zoo::rollapply(round(abs(seven_day_avg_change)), width = 1, function(x) stats::poisson.test(sum(x, na.rm = TRUE), T = length(x))$conf.int, fill = 0, align = "right")[,2],0),
            
            seven_day_avg_change_14bin = ((seven_day_avg - lag(seven_day_avg, 14))/lag(seven_day_avg, 14)*100),

            thirty_day_avg = rollmean( incidence, wind_and_lag, fill = NA, align = "right"),
            thirty_day_avg_change = ((thirty_day_avg - lag(thirty_day_avg, wind_and_lag))/lag(thirty_day_avg, wind_and_lag )*100)

        )->
prov_incidence_df1

prov_incidence_df1%>% 
    select( prov, date, seven_day_avg, seven_day_avg_change, upper_ci, lower_ci, seven_day_avg_change_14bin, thirty_day_avg, thirty_day_avg_change)%>%view()


prov_incidence_df1%>%
#filter( prov %in% c("EC"))%>%
    ggplot( )+
  #  geom_col( aes( x = date, y = incidence, fill = prov), position= "dodge", alpha = 0.5)+
  #  geom_line( aes( x = date, y = seven_day_avg, color = prov, group = prov), linetype = 1)+
    geom_line( aes( x = date, y = seven_day_avg_change, color = prov, group = prov), linetype = 1)+
    geom_ribbon( group = 1, aes( x = date, ymin = lower_ci, ymax = upper_ci, fill = prov), alpha = 0.5)+
    facet_grid( prov~1)


prov_incidence_df1%>%
    ggplot( )+
    geom_col( aes( x = date, y = incidence, fill = prov), position= "dodge", alpha = 0.5)+
    geom_line( aes( x = date, y = seven_day_avg, color = prov, group = prov), linetype = 1)+    
    # put the geom_line on a secondary axis 
    geom_line( aes( x = date, y = seven_day_avg_change, color = prov, group = prov), linetype = 1)+
    facet_grid( prov~1)+
    #geom_ribbon( aes( x = date, ymin = ci_lower, ymax = ci_upper, fill = prov), alpha = 0.5)+
    labs( title = "7 day average incidence per 100 000", y = "Incidence per 100 000")

# 7day avergae change (7 day lag)
prov_incidence_df1%>%
    ggplot() +
    geom_col( aes( x = date, y = incidence, fill = prov), position= "dodge", width = 7, alpha = 0.5)+
    geom_line( aes( x = date, y = seven_day_avg_change, color = prov, group = prov), linetype = 1)+
    labs( title = "7 day average change in incidence per 100 000", y = "Percentage change in incidence per 100 000")
  
# 7 day average change 14 day lag
prov_incidence_df1%>%
    ggplot() +
    geom_line( aes( x = date, y = seven_day_avg_change_14bin, color = prov, group = prov), linetype = 1)-> 
    percent_change_plot

percent_change_plot%>%plotly::ggplotly()

# 30 day average 30 day lag 
prov_incidence_df%>%
    ggplot() + 
    geom_line( aes( x = date, y = thirty_day_avg_change*100, color = prov, group = prov), linetype = 1)

# should probably add some kind of confidence to these to help interpretation in a table or make threshold 

# tabulate 

prov_incidence_df%>%
select( prov, date, seven_day_avg)%>%
pivot_wider( names_from = prov, values_from = seven_day_avg)%>%view()

# aggregate by week an dmonth 



format( Sys.Date(), "%A, %d %B %Y")

prov_incidence_df%>%
    filter( !is.na(seven_day_avg))%>%
    filter( date %in% max(as_date(as.character(date))-days(2), na.rm = TRUE))%>%
    mutate(
    across( where(is.numeric), ~round(., 2))
    )%>%
    mutate( date = format( as_date(date),  "%A, %d %B %Y"))%>%
    select( date, prov, cumulative_incidence, incidence, # this is daily incidence
     seven_day_avg, 
     seven_day_avg_change #, 
     #seven_day_avg_change_14bin, 
     #thirty_day_avg_change
     )->
     final_table
     final_table
     
     final_table%>%flextable()

flextable(final_table) %>%
  set_header_labels(
    date = "Date",
    prov = "Province",
    cumulative_incidence = "Cumulative Incidence \n(since 1 Jan '24')",
    incidence = "Day Incidence",
    seven_day_avg = "7-Day Avg",
    seven_day_avg_change = "7-Day Roll. Avg. Change (%)"
  ) %>%
  colformat_double(j = c("cumulative_incidence", "incidence", "seven_day_avg"), digits = 2) %>%
  colformat_double(j = "seven_day_avg_change", digits = 2, suffix = "%") %>%
  merge_v()%>%
  theme_zebra() %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2:6, width = 1.5) %>%
  align(j = 1:6, align = "center", part = "all") %>%
  color(i = ~ seven_day_avg_change < 0, j = "seven_day_avg_change", color = "red") %>%
  color(i = ~ seven_day_avg_change >= 0, j = "seven_day_avg_change", color = "green") %>%
  bold(j = "prov", part = "body") 
  border_outer(border = fp_border(color = "black", width = 1.5))


    

    







