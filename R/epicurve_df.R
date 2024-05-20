#' Epicurve dataframe
#'
#' @param data.frame a dataframe with a date column
#' @param date_index the name of the date column in the dataframe (preferable in as_date() format)
#'
#' @details
#' This function takes a dataframe of patient level data and returns an aggregated data.frame by day including zero reporting.
#' This is suitable for plotting an epicurve with ggplot2 and nesting the dates. Ensure that plotting the epicurve is
#' done  scale_x_discrete and NOT scale_x_date. We recommend using plot_epicurve_df()
#' @return a data.frame with the date, epiweek, month, year, lab, n, cumulative and day columns icluding zero reporting on all dates
#' @export
#'
#' @examples df_to_plot<- epicurve_df(patient_level_data = , date_index = "date")
#'
epicurve_df<- function( data = data, date_index = date_index ){
  # the goal of this function is to return a dataframe that can be used to plot an epicurve
  # crucially, it has every date in the range of data including zero counts so that it can be plotted.
  # it also takes a single date index and returns the relevant epiweek, month and years.

  # ensure date_index is in

  data[[date_index]] <- as_date(data[[date_index]])

  dates<- seq.Date(floor_date(min(data[[date_index]]), "month"), ceiling_date(max(data[[date_index]]), "month"), by = "day")

  epicurve_template<-
    tibble(date = rep(as_date(dates),2),
           epiweek = rep(dates %>%as_epiweek()%>%as.character()%>%gsub("\\d+-W", "", .)%>%as.integer(),2),
           month = rep(factor(as_yearmonth(dates)%>%as.character()%>%gsub("\\d+-", "", .), levels = month.abb),2),
           year = rep(epiyear(dates),2),
           lab = factor(rep(c("Epi-Link only", "Confirmed"), each = length(dates), levels = c("Epi-Link only", "Confirmed"))
           )
    )


  aggregated<- data%>%
    group_by(date, epiweek, month, year, lab)%>%
    summarise(n = n(),
    )%>%ungroup()



  df<-
    epicurve_template%>%left_join(.,
                                  aggregated ,
                                  by = c("date", "epiweek", "month", "year", "lab")
    ) %>%
    mutate( n = if_else(is.na(n), 0, n),
            lab = factor(lab, levels = c("Epi-Link only", "Confirmed")),
            epiweek  = as.factor(epiweek),
            day = as.factor(as.integer(format(date, "%d"))))%>%
    filter(month %in% month.abb[4:7] )%>%
    group_by(lab) %>%
    mutate( cumulative = cumsum(n))%>%
    ungroup()

  # you need to add some descriptors of the dataframe so that they can be used in the plot.


  return(df)

}
