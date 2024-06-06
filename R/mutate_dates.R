#' Mutate_date
#'
#' The function gives mutates individual date related variables that are compatible withother function in this package
#' This will allow you to left_join datasets with date_templates for graphic with zero reporting.
#' If converting dates from excel, use openxlsx::convertToDateTime(data$ExcelNumber, origin = "1900-01-01")
#'
#'
#' @param data a tibble or data.frame
#' @param date_index the date variable. This should be in an lubridate::as_date() format. ( "YYYY-MM_DD")
#'
#' @return a tibble with the follwing variabels as factors: date, day, epiweek (CDC format), month, year
#' @export
#'
#' @examples
#' mutate_dates(data, date_index = "notification_date")
#' # to see whether it is creating dates propoperly
#'
#' dates<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
#'
#' tibble(
#'  date = dates)%>%
#'  mutate_dates(., "date")%>%
#'  group_by(month) %>%
#'  summarise(day_in_month = n())
#'
#'  # or you could see the first and last day of the epiweek, which, according to CDC is, Sunday
#'
#' tibble(
#'  date = dates)%>%
#'  mutate_dates(., "date")%>%
#'  group_by(epiweek) %>%
#'  filter( day == min(day ) | day == max(day))
#'
#'
mutate_dates<- function(data, date_index){

  #run functions to get date in correct format, try with year first or day first, try with excel and stats date formats
  # TO DO
  ## could try make an argument that give a pre_fix or suffix to the date variables for instance, year_notification and so on
  standard_group_vars<- c("year", "month", "epiweek", "date")
  conflicted::conflicts_prefer(grates::year)
  data <- data %>%
    mutate(date = as_date(data[[date_index]]))%>%
    mutate(
      day_name = as.factor( format(date, "%A") ),
      day = as.factor(as.integer(format(date, "%d"))),
      epiweek =  date %>%grates::as_epiweek%>%as.character()%>%gsub("\\d+-W", "", .)%>%as.integer(),
      month = factor(grates::as_yearmonth(date)%>%as.character()%>%gsub("\\d+-", "", .), levels = month.abb),
      'year' = lubridate::epiyear(date)
    )%>%
      mutate(across(all_of(standard_group_vars), as.factor))

  return(data)

}
