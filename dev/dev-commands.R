
library(devtools)

use_data_raw()

document()
load_all()
document()
test()
check()

?NMCleaner::mutate_district_name

#ensure this is compiled


# Test the epicurve_df() with grouping_vars = NULL

#OR

# Test the epicurve_df() with grouping_vars where the grouping_vars length ==1

sample %>%
  filter( condition %in% "Measles")->sample1

NULL %>% { if (is.null(.)) "NULL" else . } %in% "string"

"string" %>% { if (is.null(.)) "NULL" else . } %in% "string"

sample %>%ungroup()%>%
  filter( condition %in% "Measles")%>%
  epicurve_df(
    date_index = "notification_date",
    grouping_vars = "other"
  )

NMCleaner::condition_df
NMCleaner::infectious_diseases

?create_date_template

epicurve_template<-create_date_template(
  min(dates), max(dates),
  reps = length(data[[grouping_vars]]%>%unique),
  rep_on_var = data[[grouping_vars]]%>%unique,
  rep_var_name = grouping_vars)

create_date_template(
  start_date = min(sample1$notification_date),
  end_date = max(sample1$notification_date),
  reps = 1,
  rep_on_var = sample1$condition %>%unique(),
  rep_var_name = c("condition")
)


epicurve_template<-create_date_template(min(dates), max(dates),
                                        reps = length(data[[grouping_vars]]%>%unique),
                                        rep_on_var = data[[grouping_vars]]%>%unique,
                                        rep_var_name = grouping_vars)

epicurve_template<-
  tibble(
    !!"condition" := rep(sample1[["condition"]] %>%unique(), each = length(10)),
  )

epicurve_template


sample %>%
  ungroup()%>%
  #filter( condition %in% "Measles")%>%
  epicurve_df(
    date_index = "notification_date",
    grouping_vars = "condition"
  )
