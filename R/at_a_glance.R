#' Categry at a glance
#'
#' @param clean_df
#'
#' @return Returns the "category 1 at a glance table"
#' @export
#'
#' @examples
at_a_glance<- function(data){
cat_1<-  data%>%
  filter(nmccategories ==1)%>%select(condition) %>% unique %>%pull

#nrow(guideline_list)
category_1<- condition_df%>%filter(nmccategories ==1)%>%select(condition) %>% unique %>%pull

case_def_labs<- data$case_definition%>%unique

cat_1_glance<-
  data%>%
  filter(nmccategories ==1)%>%
  mutate( condition = factor(condition, levels = c(category_1 # this will ensure that factors are complete. if factors are not complete, "unknwon" will appear on the table.
  )))%>%
  dplyr::select(condition , case_definition)%>%
  tbl_summary(by = case_definition,
              percent = "row",
              statistic = list(all_categorical() ~ "{n}") )%>%
  modify_header(label = "**Condition**",

                all_stat_cols() ~ "**{level}**, \n N = {scales::number(n)}")%>%
  add_overall(last = FALSE, statistic = ~"{n}", col_label = "**Total** \n N = {scales::number(n)}" )%>%

  trim_tbl()%>%
  modify_footnote(update = everything() ~ NA
  ) %>% modify_footnote(all_stat_cols() ~ "Suspected and confirmed cases are independent and are not totalled - suspected and confirmed cases are distinct.")


ft<- cat_1_glance%>%as_flex_table%>%
  flextable::set_caption("The number of notifications that are suspected and confirmed for category 1 conditions.") %>%# Example: making the first row (header) bold
  fontsize(, size = 9, part = "header")%>%
  flextable::set_table_properties(layout = "autofit", width = 0.99)


to_bold<- which(cat_1_glance$table_body$stat_2 != 0 )

flex<- bold(ft, i = c(to_bold)  , j = 4)%>%
  bg(, i = c(to_bold), bg = "grey90", part = "body" )

return(flex)

}
