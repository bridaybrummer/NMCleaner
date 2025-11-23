#' Sliding CUSUM window summary
#'
#' Run a sliding-window EARS + CUSUM analysis over a time series and return the
#' last row from each window. The implementation avoids repeated date parsing
#' and uses a single-pass lapply to reduce intermediate allocations.
#'
#' @param data Data frame containing at least a date column and a count column
#'   named `n` (or the column expected by your `ears()` implementation).
#' @param window Integer window size (number of rows) for the sliding window.
#' @param date_index Character scalar giving the name of the date column in
#'   `data` (default: "date"). The function will coerce this column to Date.
#' @param start_date Optional start Date — if provided the function will drop
#'   rows older than `start_date - window` before processing.
#' @param k Numeric CUSUM k parameter (passed to `cusum()`). Default 0.5.
#' @param cusum_threshold Numeric CUSUM threshold (passed to `cusum()`).
#'   Default 5.
#' @return A data frame with one row per window containing the final row
#'   variables from the CUSUM output and two helper columns: `iteration` and
#'   `latest_is_cusums_outbreak`.
#' @export
#' @examples
#' 
#' df <- tibble::tibble(
#'   date = seq.Date(as.Date('2020-01-01'), by = 1, length.out = 200),
#'   n = rpois(200, 2)
#' )
#' # run a fast sliding analysis
#' 
#' 
cusum_window_start <- function(data, window = 180, k = 0.5, cusum_threshold = 5, date_index = "date", start_date = NULL) {
    if (nrow(data) == 0) {
        # Create a tibble (dataframe) with the desired column names and types
        empty_df <- tibble(
            year = factor(),
            month = factor(),
            epiweek = factor(),
            date = factor(),
            condition = character(),
            day = factor(),
            n = double(),
            cumulative = double(),
            roll_avg = double(),
            ci_upper = double(),
            ci_lower = double(),
            lag_1 = double(),
            lag_2 = double(),
            lag_3 = double(),
            lag_4 = double(),
            lag_5 = double(),
            lag_6 = double(),
            lag_7 = double(),
            lag_8 = double(),
            lag_9 = double(),
            lag_10 = double(),
            lag_11 = double(),
            lag_12 = double(),
            lag_13 = double(),
            lag_14 = double(),
            std_t = double(),
            mean_t = double(),
            z_t = double(),
            is_ears_outbreak = logical(),
            ears_threshold = double(),
            cusums_threshold = double(),
            is_cusums_outbreak = logical()
        )
        return(empty_df)
    }

    cusum <- function(dt, k = 0.5, cusum_threshold = 10) {
        # dt: a data.table with numeric column 'n'
        # k: slack parameter; cusum_threshold: outbreak threshold for the cumulative sum
       
      #  dt <- data
        if (!is.data.table(dt)) setDT(dt)

        # Calculate the overall mean and standard deviation for 'n'
        y_hat <- dt[, mean(n, na.rm = TRUE)]
        s <- dt[, sd(n, na.rm = TRUE)]

        # Compute standardized values (z-scores)
        suppressWarnings(dt[, z_t := (n - y_hat) / s])

        # Pre-allocate a numeric vector for CUSUM values
        n_rows <- nrow(dt)
        cusums <- numeric(n_rows)
        c_t_prev <- 0 # Initialize the previous CUSUM value

        # Recursive computation of the CUSUM values
        for (i in seq_len(n_rows)) {
            # Calculate the current cumulative sum: reset to 0 if negative
            c_t <- max(0, c_t_prev + dt$z_t[i] - k)
            # In case of NA, we force the value to 0
            cusums[i] <- ifelse(is.na(c_t), 0, c_t)
            c_t_prev <- c_t # Update for the next iteration
        }

        # Add the computed CUSUM values to the data.table
        dt[, cusums_threshold := cusums]

        # Flag an outbreak if the CUSUM exceeds the specified threshold
        dt[, is_cusums_outbreak := cusums > cusum_threshold]

        return(dt)
    }

   # cusum(data, k = k, cusum_threshold = cusum_threshold)%>%print() 
    
    data %>%mutate_dates(., date_index = date_index)-> data

    # Convert start_date to Date type if provided
    if (!is.null(start_date)) {
        start_date <- as_date(start_date)
        data <- data %>%
            filter(as_date(!!sym(date_index)) >= start_date - days(window))
    }

    # Validate that there is enough data for the specified window
    if (nrow(data) < window) {
        stop("Not enough data after the start_date for the specified window size.")
    }

    total_iterations <- nrow(data) - window

    pb <- progress::progress_bar$new(
        format = "Processing [:bar] :percent in :elapsed | ETA: :eta",
        total = total_iterations, clear = FALSE, width = 60
    )

    time_start <- Sys.time() # Start timing

    system.time({
        df_with_ears <- data %>%
            #filter(as_date(!!sym(date_index)) < as_date("2024-08-19")) %>%
            cusum(k = k, cusum_threshold = cusum_threshold)
    }) -> time_taken
    
    
    # Pre-allocate results list for efficiency
    results_list <- vector("list", length = total_iterations)

    for (i in 1:total_iterations) {
        # Progress bar update
        pb$tick() # Update progress bar

        # Subset the data for the window
        subset_df_to_plot <- data[i:(i + window - 1), ]

        cusum_result <- subset_df_to_plot %>%
            #ears() %>%
            cusum(k = k, cusum_threshold = cusum_threshold)

        latest <- tail(cusum_result, 1)

        # Store the relevant values in the results list
        results_list[[i]] <- data.frame(
            year = latest$year,
            month = latest$month,
            epiweek = latest$epiweek,
            date = latest$date,
           # condition = latest$condition,
            day = latest$day,
            n = latest$n,
            iteration = i,
            latest_is_cusums_outbreak = latest$is_cusums_outbreak,
            latest_cusum_threshold = latest$cusums_threshold
        )
    }

    time_end <- Sys.time() # End timing

    # Print total time taken
    cat("\nTotal time taken:", time_end - time_start, "seconds\n")

    # Combine the list into a data frame
    results_df <- do.call(rbind, results_list)

    return(results_df)
}

