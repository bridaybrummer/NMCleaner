#' Title: Epicurve dataframe
#'
#'  This function takes a dataframe of patient level data and returns an aggregated data.frame by day including zero reporting.
#'  This is suitable for plotting an epicurve with ggplot2 and nesting the dates. Ensure that plotting the epicurve is
#'  done  scale_x_discrete and NOT scale_x_date. We recommend using the plot_epicurve_df() function to plot the epicurve
#'
#'  The function currently cannot accept the grouping_vars == NULL argument, so rather use conditions if it sa single condition.
#'
#' @param data a patient level data.frame or tibble with a date column. Does not upport ulti condition dataframes yet
#' @param date_index the name of the date column in the dataframe which should be a lubridate object or in the format YYYY-MM-DD
#' @param grouping_vars default is NULL but can take another grouping variable such as lab confirmation or case_definition (If it exists in your df). To be passed to a fill argument in plot_epicruve_df()
#' @param add_rolling_avg a logical vector of length 2. The first element is whether to add a rolling average to the epicurve and the second element is the window size for the rolling average
#' @return returns a data.frame with the date, epiweek, month, year, lab, n, cumulative and day columns including zero reporting on all dates
#' @export
#'
#' @examples
#' df_to_plot<- epicurve_df(patient_level_data = , date_index = "date", extra_group_by = "case_definition")
#'
epicurve_df<- function( data = data,
                         date_index = date_index,
                         grouping_vars =NULL,
                         add_rolling_avg = c(TRUE, 7)){
  # the goal of this function is to return a dataframe that can be used to plot an epicurve
  # crucially, it has every date in the range of data including zero counts so that it can be plotted.
  # it also takes a single date index and returns the relevant epiweek, month and years.

  # ensure date_index is in

  # put this function in just in case.


  data[[date_index]] <- as_date(data[[date_index]])

  dates<- seq.Date(floor_date(min(data[[date_index]]), "month"), ceiling_date(max(data[[date_index]]), "month"), by = "day")

  if (!is.null(grouping_vars)){


  grouping_vars_length <- data[[grouping_vars]]%>%unique() %>%length # will have to do a few of these if ther are more grouping vars.

  epicurve_template<-
    tibble(
      !!grouping_vars := rep(data[[grouping_vars]] %>%unique(), each = length(dates)),
    )
  }else{
    epicurve_template<-
      tibble(
      )
  }



  standard_group_vars<- c("year", "month", "epiweek", "date")

  group_vars<- c(grouping_vars, standard_group_vars)

  aggregated<- data%>%

    mutate(
      date = as_date(data[[date_index]]))%>%
    mutate(
      epiweek =  date %>%as_epiweek%>%as.character()%>%gsub("\\d+-W", "", .)%>%as.integer(),
      month = factor(as_yearmonth(date)%>%as.character()%>%gsub("\\d+-", "", .), levels = month.abb),
      year = epiyear(date),

    )%>%
    group_by(across(all_of(c(group_vars ))))%>%
    summarise(n = n(),
    )%>%ungroup()



  df<-
    epicurve_template%>%left_join(.,
                                  aggregated ,
                                  by = c(group_vars)
    ) %>%
    ungroup() %>%
    mutate( across(where(is.numeric), ~ifelse( is.na(.) , 0, as.numeric(.))),
            #lab = factor(lab, levels = c("Epi-Link only", "Confirmed")),
            epiweek  = as.factor(epiweek),
            day = as.factor(as.integer(format(date, "%d"))))%>%
    #filter(month %in% month.abb )%>%
    group_by(across(all_of(grouping_vars))) %>%
    mutate( cumulative = cumsum(n))%>%
    ungroup()%>%


    group_by(across(all_of(grouping_vars)))%>%
    mutate(
      roll_avg = zoo::rollmean(n, add_rolling_avg[2], fill = 0, align = "right"), # FILL CAN ALSO BE NA
      # add CI for rollmean
      #ci_upper = zoo::rollapply(n, add_rolling_avg[2], function(x) t.test(x)$conf.int, fill = NA, align = "right")[,1],
      #ci_lower = zoo::rollapply(n, add_rolling_avg[2], function(x) t.test(x)$conf.int, fill = NA, align = "right")[,2]

      # these are better as they use a poisson distributions for the count data in geenrating the CI
      ci_upper = zoo::rollapply(n, width = add_rolling_avg[2], function(x) poisson.test(sum(x), T = length(x))$conf.int, fill = 0, align = "right")[,1],
      ci_lower = zoo::rollapply(n, width = add_rolling_avg[2], function(x) poisson.test(sum(x), T = length(x))$conf.int, fill = 0, align = "right")[,2]
    )%>%ungroup()%>%
    mutate(across(where(is.numeric), ~ifelse( is.na(.) , 0, as.numeric(.))))%>%
    mutate( across( all_of( standard_group_vars), ~as.factor(.)))%>%
    tibble

  # you need to add some descriptors of the dataframe so that they can be used in the plot.


  return(df)

}


