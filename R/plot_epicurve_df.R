#' Plots a simple epicurve with options from the epicurve_df() function.
#'
#' @param data a dataframe that has been processed by the epicurve_df() function
#' @param x_axis_option a string that can be one of the following: "epiweek", "day_month_year", "month_year" The string is evaluated wiht a jarowinkler score enabling ingestion of arguments with small errors.
#' @param color_select a string color options. default is "firebrick"
#' @param fill_var Currenlty unavailable. Planned to take a string that can be used to fill the bars of the epicurve. default is NULL.
#' @param n_x_axis_breaks an integer that sets the number of breaks on the x axis. default is 10
#' @param n_y_axis_breaks an integer that sets the number of breaks on the y axis. default is 10
#' @param add_rolling_avg a logical vector that sets whether to add a rolling average to the epicurve.
#' @param add_bar a logical vector that sets whether to add bars to the epicurve. default is TRUE
#' @param color_select a string that sets the color of the epicurve. default is "firebrick"
#' @param grouping_vars a string that sets the grouping variable for the epicurve. default is NULL
#' @param axis_text_size an integer that sets the size of the axis text. default is 6
#' @return returns a ggplot object
#' @export
#'
#' @examples
#' df_to_plot<- epicurve_df(patient_level_data = ,
#'                          date_index = "date",
#'                          extra_group_by = "case_definition",
#'                          n_axis_breaks = 10 , # 10 is set as default but you may change this #for the month_year options, all months are shown by default.
#'                          n_y_axis_breaks = 10 , # 10 is set as default but you may change this
#'                          add_rolling_avg = c(TRUE,7), # 7 is set as default but you may change this
#'                          add_bar = TRUE # TRUE is set as default, if FALSE then the bars are not shown
#'                          grouping_vars = NULL # Null is standar. If groupin specifed it automaticlly facets by the grouping variable. An option for filling by the goruping variable is being worked on.
#'
#'
#' output<- plot_epicurve_df(data = df_to_plot, x_axis_option = "day_month_year", color_select = "red")
#'
#' output$plot # the plot.
#' output#data # raw data used to plot
#'
plot_epicurve_df<- function(data = data,
                            x_axis_option =x_axis_option,
                            n_x_axis_breaks = 10,
                            n_y_axis_breaks = 10,
                            add_rolling_avg = c(TRUE,7),
                            grouping_vars = NULL,
                            add_bar = TRUE,
                            axis_text_size = 6,
                            #fill_var = NULL, for now, no fill_var can be selected. This should be allowed for atelast case definitions or other.
                            # for the fill var you will need to have pallettes that choose the amount of palettes to use based on the length of levels
                            color_select = NULL ){

  # Create the epicurve nesting options -----
  available_x_axis_options <- c("epiweek", "day_month_year", "month_year")
  # you should NOT add epiweeks_months because epiweeks fall across months.
  # you CAN still add just days_months
  #if the x_axis_option does not match the available options,
  # and the x_axis_topns does not match more than jw 0.8
  # then stop()
  stop_message<- paste0("x_axis_option must be one of the following:", available_x_axis_options )
  #if(!x_axis_option %in% available_x_axis_options){
  if (!x_axis_option %in% available_x_axis_options) {

    jw_match <- tibble(
      x_axis_options = x_axis_option,
      available_x_axis_options = available_x_axis_options
    ) %>%
      mutate(
        jw = 1 - stringdist(x_axis_options, available_x_axis_options, method = "jw")
      )

    if(jw_match[[which.max(jw_match$jw), "jw"]] > 0.8){
      x_axis_option <- jw_match[[which.max(jw_match$jw), "available_x_axis_options"]]

      message(paste0("The x_axis_option did not perfectly match the available options of ",
                     available_x_axis_options,
                     " so it was changed to ",
                     x_axis_option))

    }else{
      stop(paste0(stop_message))
    }

  }

  # create breaks_for_plot based on the axis option

  data<- data %>%ungroup()

  if(x_axis_option == "epiweek"){

    x_1st_level <- "epiweek"
    x_2nd_level <- "year"
    x_3rd_level <- NULL
    x_label <- "Epiweek of Notification"

    data1<- data%>% group_by(across(all_of(c(grouping_vars,
                                             x_2nd_level,
                                             x_1st_level
    ))))%>%

      summarise( n= sum(n, na.rm = TRUE),
                 roll_avg = sum(roll_avg, na.rm = TRUE),
                 ci_upper = sum(ci_upper, na.rm = TRUE),
                 ci_lower = sum(ci_lower, na.rm = TRUE)
      )%>%
      ungroup()%>%
      mutate(cumulative = cumsum(n))



    x_breaks_for_plot =  pretty(seq(min(as.numeric(as.character(data1[[x_1st_level]]))),  max(as.numeric(as.character(data1[[x_1st_level]])))), n = n_x_axis_breaks)
    # the x_breaks_for_plot could technically go outside of the function, but it cant be selected correctly
    # for the epiweek option since they are character dates and not numbers.

  }else if(x_axis_option == "day_month_year"){

    x_1st_level <- "day"
    x_2nd_level <- "month"
    x_3rd_level <- "year"
    x_label <- "Date of Notification"

    data1<- data%>% group_by(across(all_of(c(grouping_vars,
                                             x_3rd_level,
                                             x_2nd_level,
                                             x_1st_level
    ))))%>%

      summarise( n= sum(n, na.rm = TRUE),
                 roll_avg = sum(roll_avg, na.rm = TRUE),
                 ci_upper = sum(ci_upper, na.rm = TRUE),
                 ci_lower = sum(ci_lower, na.rm = TRUE)
      )%>%
      ungroup()%>%
      mutate(cumulative = cumsum(n))


    x_breaks_for_plot =  pretty(seq(min(as.numeric(as.character(data1[[x_1st_level]]))),  max(as.numeric(as.character(data1[[x_1st_level]])))), n = n_x_axis_breaks)
    #  mutate_dates(., date_index = "date")%>%
    #  select(day ) %>%
    #  pull()

    #x_breaks_for_plot =  pretty(axis_string , n = n_x_axis_breaks)


  }else if(x_axis_option == "month_year"){

    x_1st_level <- "month"
    x_2nd_level <- "year"
    x_3rd_level <- NULL
    x_label <- "Month of Notification"

    data%>% group_by(across(all_of(c(grouping_vars,
                                     x_2nd_level,
                                     x_1st_level
    ))))%>%

      summarise( n= sum(n, na.rm = TRUE),
                 roll_avg = sum(roll_avg, na.rm = TRUE),
                 ci_upper = sum(ci_upper, na.rm = TRUE),
                 ci_lower = sum(ci_lower, na.rm = TRUE)
      )%>%
      ungroup()%>%
      mutate(cumulative = cumsum(n))

    data1<- data%>%ungroup()
    # need a way to specify the number of breaks for the x axis when it is a character date.
    # maybe convert to numeric and then back to date?

    x_breaks_for_plot =  month.abb[ month.abb %in% unique(data1[[x_1st_level]])]


  }else{
    stop(paste0(stop_message))
  }

  print(paste0("You selected levels: ",x_1st_level, x_2nd_level, x_3rd_level))

  print(data1)

  #data <- data %>%
  #  group_by(vars)

  # Function to do list
  ## We are going to recreate the first epicurve wiht the function
  ## This will be the dates_months_years epicurve.
  ## first make the epicurve as simple as possible
  ## then add the themes and facet_nested
  ## Then add the strip font changing.


  # need to adjust the x axis for the x axis option you choose.

  max_cases <- ceiling(max((data1$n)*1.5, na.rm = TRUE))
  max_cum_cases <- floor(max((data1$cumulative)*1.1, na.rm = TRUE))
  number_axes <- 10
  primary_by_axes <- ceiling(max_cases/number_axes)
  secondary_by_axes <- ceiling(max_cum_cases/number_axes)
  seccy_axis<- floor(max_cum_cases/max_cases)

  if( !is.null(x_3rd_level)){

    combo <- data1 |>
      distinct(!!sym(x_3rd_level), !!sym(x_2nd_level), !!sym(x_1st_level)) |>
      arrange(!!sym(x_3rd_level), !!sym(x_2nd_level))


    strip_text<-
      strip_nested(text_x =
                     elem_list_text(
                       angle = c(
                         # level1 colors
                         rep(0, length(unique(combo[[x_3rd_level]])) )

                         ,
                         #level2 colors
                         rep(0, length(unique(combo[[x_2nd_level]])))
                       )
                     )
      )
  } else{
    strip_text = NULL
  }

  fill_var <- NULL
  #color_select <- "blue"
  default_function_color<- "firebrick"

  blue_palette<- c() #remember these will be for colour fills for fill variable that you could add for lab_type or case_definition etc.
  red_pallette<- c()

  if(is.null(fill_var)){
    ifelse(is.null(color_select),
           fill_select <- default_function_color,
           fill_select <- color_select)
  }else{
    fill_select <- sym(fill_var)
  }

  plot<- data1  %>%
    ggplot(.,
           aes(x = !! sym(x_1st_level) ))

  if(add_bar == TRUE ){
    plot<- plot+
      geom_bar(   aes(
        #x= day,
        y = n ,
        fill = ""),
        stat = "identity",
      )
  }

  if(add_rolling_avg[1] == TRUE){
    print(paste0("Adding Line"))

    plot<- plot +
      geom_line(aes(x = !!sym(x_1st_level),
                    y = roll_avg,
                    group = 1),
                color = "grey10")+
      geom_ribbon(aes(x = !!sym(x_1st_level),
                      ymin = ci_upper,
                      ymax = ci_lower,
                      group = 1), # group = 1 is crucial for the line to actually show
                  fill = "grey80",
                  color = "grey10",
                  size= 0.1,
                  alpha = 0.25)

  }

  plot<- plot+
    scale_fill_manual(values = fill_select)
  # maybe add to plot a smoothed line or rolling average?
  # plot the cumulative line
  #geom_line(aes(
  #x = day,
  #  y = cumulative,#/seccy_axis,
  #  group = lab,
  #  color = lab
  #),
  #stat = "identity",
  #size = 1
  #)+


  if( !is.null(grouping_vars) ){

    if (!is.null(x_3rd_level)) {
      plot <- plot + facet_nested(cols = vars( !!sym(x_3rd_level), !!sym(x_2nd_level)),
                                  rows = vars(!!sym(grouping_vars)),
                                  scales = "free_y",# other options include "free_x", "free_y", "free", "fixed"
                                  switch = "x")


    } else if(is.null(x_3rd_level)){
      plot <- plot + facet_nested(cols = vars(!!sym(x_2nd_level)),
                                  rows = vars(!!sym(grouping_vars)),
                                  scales = "free_y",
                                  switch = "x")
    }

  }else{

    if (!is.null(x_3rd_level)) {
      plot <- plot + facet_nested(cols = vars( !!sym(x_3rd_level), !!sym(x_2nd_level)),
                                  #rows = vars(!!sym(grouping_vars)),
                                  scales = "free_y",# other options include "free_x", "free_y", "free", "fixed"
                                  switch = "x")


    } else if(is.null(x_3rd_level)){
      plot <- plot + facet_nested(cols = vars(!!sym(x_2nd_level)),
                                  #rows = vars(!!sym(grouping_vars)),
                                  scales = "free_y",
                                  switch = "x")

    }

  }

  plot <- plot +
    scale_x_discrete(
      breaks = x_breaks_for_plot, #find the breaks_for_plot
      expand = c(0,0))+
    #include secondary axis for cumulative line
    scale_y_continuous(#breaks = seq(0, max_cases, primary_by_axes),#seq(0,breaks_ref*1.1, round((breaks_ref/10),0)),
      expand = c(0.1,0),
      #limits = c(0, max_cases),
      # sec.axis = sec_axis(~.*seccy_axis,
      #                     name="Second Axis",
      #                     breaks = seq(0, max_cum_cases, secondary_by_axes),
      #limits = c(0, max_cum_cases)
      #)
    )+
    labs( #title = "Epicurve of Measles notifications Notified to the NMCSS during 2023.",
      fill = "Case Definition",
      x = x_label,
      y = "Notifications (n)")+
    theme(

      panel.spacing=unit(0.17,"lines"),
      strip.background=element_rect(color=NA, fill=NA),
      #panel.border=element_rect(color="grey90"),
      #axis.ticks.x=element_blank(),
      strip.placement = "outside",
      text = element_text(family = "Century Gothic", size = 10, color = "#333333"),
      panel.background = element_rect(fill = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8"),
      panel.grid = element_line(color = "#DDDDDD"),
      legend.text = element_text(color = "#333333", size = 8),
      legend.title = element_text(color = "#333333", size = 10),
      plot.title = element_text(size = 10, color = "#666666"),
      plot.subtitle = element_text(size = 8, color = "#666666"),
      plot.caption = element_text(size = 8, color = "#666666"),
      legend.position = "none", #"right", #"none" is a global option to remove the legend.
      legend.direction = "vertical",
      legend.key = element_blank(),
      legend.title.align = 0.5,
      panel.grid.major = element_blank(),
      axis.text.x = element_text(angle = 30, size = 6),
      axis.text.y = element_text(size = 6, color = "#666666"),
      axis.title = element_text(size = 10, color = "#666666"),
      panel.grid.minor.x =element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_line(size = 0.15, color = "grey20"),
      legend.background = element_rect(fill = "#F8F8F8", size = 0.25, linetype = "solid", colour = "grey50"),
      #strip.text.x = ggplot2::element_text(size = 9, angle=90)
      strip.text.y.right = element_text(angle = 0)
    )

  print(data1)
  return(list(plot = plot, data = data1))

}
