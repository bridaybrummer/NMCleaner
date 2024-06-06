#' Title: Create Date Template
#'
#'  This fucntion takes argumetns of dat dimensions and extra varibales and creates a template of dates
#'  with the specified dimensions. It can be used to left_join aggregate data to create a full dataset
#'  that includes zero reporting. Future plans also include a date_standardize function that will create the necessary
#'  dates for a given dataset using the date_index so that joining datasets is easy.
#'
#' @param start_date a character string in the format "YYYY-MM-DD" denoting the minimum date in your returned tibble
#' @param end_date a character string in the format "YYYY-MM-DD" denoting the maximum date in your returned tibble
#' @param reps default is NULL, a numeric value denoting  the length of the unique values of the extra varibale you wish to add
#' @param rep_on_var the unique values of the extra variable to add in c()
#' @param rep_var_name a character string value that denotes the name of the extra variable you wish to add
#'
#' @return A tibble of date columns for all dates in a given time period
#' @export
#'
#' @examples
#' template_2024<- create_date_template("2024-01-01", "2024-12-31", reps =2, rep_on_var = c("cholera", "pertussis"), "condition" )
#'
#' #You can check that it ran properly by checkign the numebr of days per month per condition
#'
#' template_2024%>%
#'  group_by(month, condition)%>%
#'  summarise(n = n())
#'
create_date_template<- function( start_date , end_date, reps = 1, rep_on_var = NULL, rep_var_name =NULL) {

  conflicted::conflicts_prefer(grates::year)

  standard_group_vars<- c("year", "month", "epiweek", "date")

  dates <- as_date(seq(as_date(start_date), as_date(end_date), by = "days"))

  if(reps > 0){

    data<-
      tibble(
        date = rep(dates, each = reps),
        !!rep_var_name := rep(rep_on_var, length.out=length(dates)*reps)
      )
  }else{
    data<-
      tibble(
        date = dates
      )
  }
  data<- data%>%
    mutate(
      day = as.factor(as.integer(format(date, "%d"))),
      epiweek =  date %>%grates::as_epiweek()%>%as.character()%>%gsub("\\d+-W", "", .)%>%as.integer(),
      month = factor(grates::as_yearmonth(date)%>%as.character()%>%gsub("\\d+-", "", .), levels = month.abb),
      'year' = lubridate::epiyear(date)

    )%>%
    mutate(across(all_of(standard_group_vars), as.factor))%>%
    select(all_of(c(standard_group_vars, rep_var_name, "day")))

  return(data)
}


