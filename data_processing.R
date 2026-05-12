# turkey_project/data_processing.R

## Preprocess raw turbine data ###############
#' @param df Raw data frame with exact column names
#' @return Processed data frame
preprocess_turbine_data = function(df) {
  
  # Create a copy
  data = df
  
  # STEP 1: Handle column names
  
  # Rename columns to standardized names
  colnames(data) = gsub('\'', '', colnames(data))
  colnames(data) = trimws(colnames(data))
  
  # Create a mapping of possible names to standard names
  col_mappings = list(
    'timestamp' = c('Date/Time'),
    'active_power_kw' = c('LV ActivePower \\(kW\\)'),
    'wind_speed_ms' = c('Wind Speed \\(m/s\\)'),
    'theoretical_power_kwh' = c('Theoretical_Power_Curve \\(KWh\\)'),
    'wind_direction_deg' = c('Wind Direction \\(°\\)')
  )
  
  # Function to find and rename columns
  find_and_rename = function(data, target_name, possible_names) {
    for(pattern in possible_names) {
      matches = grep(pattern, colnames(data), ignore.case = TRUE, value = TRUE)
      if(length(matches) > 0) {
        colnames(data)[colnames(data) == matches[1]] = target_name
        return(list(data = data, found = TRUE))
      }
    }
    return(list(data = data, found = FALSE))
  }
  
  # Apply mappings
  for(target_name in names(col_mappings)) {
    result = find_and_rename(data, target_name, col_mappings[[target_name]])
    data = result$data
    if(!result$found) {
      cat('WARNING: Could not find columns for', target_name, '\n')
    }
  }
  
  # STEP 2: Convert timestamp
  data$timestamp = parse_date_time(
    data$timestamp,
    orders = c("d m Y H:M", "Y-m-d H:M", "d/m/Y H:M", "m/d/Y H:M")
  )
  
  # STEP 3: Convert numeric columns
  numeric_columns = c('active_power_kw', 'wind_speed_ms',
                      'theoretical_power_kwh', 'wind_direction_deg')
  
  for(col in numeric_columns) {
    if(col %in% colnames(data)) {
      data[[col]] = as.numeric(data[[col]])
    }
  }
  
  # STEP 4: Remove NA values in important columns
  initial_rows = nrow(data)
  
  # Check which important columns exist
  important_cols = c()
  if('timestamp' %in% colnames(data)) important_cols = c(important_cols, 'timestamp')
  if('active_power_kw' %in% colnames(data)) important_cols = c(important_cols, 'active_power_kw')
  if('wind_speed_ms' %in% colnames(data)) important_cols = c(important_cols, 'wind_speed_ms')
  
  if(length(important_cols) >0) {
    # Create a logical vector of complete cases for important columns
    complete_cases = complete.cases(data[, important_cols, drop = FALSE])
    data= data[complete_cases, ]
    
    removed = initial_rows - nrow(data)
  } else {
    cat('WARNING: No important columns found!\n')
  }
  
  # STEP 5: Add calculated columns
  
  # Calculate performance ratio if we have theoretical power
  if('theoretical_power_kwh' %in% colnames(data) && 'active_power_kw' %in% colnames(data)) {
    data$performance_ratio = ifelse(
      data$theoretical_power_kwh > 0,
      data$active_power_kw / data$theoretical_power_kwh,
      NA
    )
    data$power_difference = data$active_power_kw - data$theoretical_power_kwh
  }
  
  # Add time-based features
  if('timestamp' %in% colnames(data)) {
    data$date = as.Date(data$timestamp)
    data$hour = lubridate::hour(data$timestamp)
    data$month = lubridate::month(data$timestamp, label = TRUE)
    data$day_of_week = weekdays(data$timestamp, abbreviate = TRUE)
  }
  
  # Add operational status
  if('active_power_kw' %in% colnames(data)) {
    data$is_operational = data$active_power_kw > 10
  }
  
  return(data)
}

## Create power curve bins with performance metrics ###############
#' @param df Processed data frame
#' @param bin_width Width of wind speed bins in m/s
#' @param min_points Minimum data points required per bin
#' @param performance_metric Metric to use ('ratio', 'diff', or 'percent')
create_power_curve_bins = function(df, bin_width = 0.5, min_points = 10, performance_metric = 'ratio') {
  
  if(is.null(df) || nrow(df) == 0) {
    warning('No data to bin')
    return(NULL)
  }
  
  # Create wind speed bins
  df$wind_speed_bin = floor(df$wind_speed_ms / bin_width) * bin_width + bin_width / 2
  
  # Group by bin
  binned_data = df %>%
    group_by(wind_speed_bin) %>%
    summarise(
      n = n(),
      avg_power = mean(active_power_kw, na.rm = TRUE),
      std_dev = sd(active_power_kw, na.rm = TRUE),
      min_power = min(active_power_kw, na.rm = TRUE),
      max_power = max(active_power_kw, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Add theoretical power if available
  if('theoretical_power_kwh' %in% colnames(df)) {
    theoretical_avg = df %>%
      group_by(wind_speed_bin) %>%
      summarise(
        avg_theoretical = mean(theoretical_power_kwh, na.rm = TRUE),
        .groups = 'drop'
      )
    
    binned_data = binned_data %>%
      left_join(theoretical_avg, by = 'wind_speed_bin')
    
    # Calculate performance based on selected metric
    if(performance_metric == 'ratio') {
      binned_data$performance_metric = ifelse(
        binned_data$avg_theoretical > 0,
        binned_data$avg_power / binned_data$avg_theoretical,
        NA
      )
    } else if(performance_metric == 'diff') {
      binned_data$performance_metric = binned_data$avg_power - binned_data$avg_theoretical
    } else if(performance_metric == 'percent') {
      binned_data$performance_metric = ifelse(
        binned_data$avg_theoretical > 0,
        ((binned_data$avg_power - binned_data$avg_theoretical) / binned_data$avg_theoretical) * 100,
        NA
      )
    } else {
      # Default to ratio
      binned_data$performance_metric = ifelse(
        binned_data$avg_theoretical > 0,
        binned_data$avg_power / binned_data$avg_theoretical,
        NA
      )
    }
  }
  
  # Calculate confidence intervals
  binned_data = binned_data %>%
    mutate(
      ci_lower = avg_power - 1.96 * std_dev / sqrt(n),
      ci_upper = avg_power + 1.96 * std_dev / sqrt(n)
    ) %>%
    filter(
      n >= min_points, 
      wind_speed_bin >= 0,
      wind_speed_bin <= 25
    ) %>%
    arrange(wind_speed_bin)
  
  # Add metric type for reference
  binned_data$metric_type = performance_metric
  
  return(binned_data)
}

## Calculate performance metrics ###############
#' @param df Processed data frame
#' @param binned_data Binned power curve data
#' @param performance_metric Metric to use ('ratio', 'diff', or 'percent')
#' @return List of performance metrics
calculate_performance_metrics = function(df, binned_data = NULL, performance_metric = 'ratio') {
  metrics = list()
  
  # Check if data exists
  if(is.null(df) | nrow(df) == 0) {
    metrics$data_points = 0
    metrics$avg_power = NA
    metrics$total_energy = 0
    metrics$avg_wind_speed = NA
    metrics$avg_efficiency = NA
    metrics$availability = NA
    metrics$max_power_output = NA
    metrics$optimal_wind_speed = NA
    metrics$performance_metric = performance_metric
    return(metrics)
  }
  
  # Basic statistics
  metrics$data_points = nrow(df)
  
  if('active_power_kw' %in% colnames(df)) {
    metrics$avg_power = mean(df$active_power_kw, na.rm = TRUE)
    metrics$total_energy = sum(df$active_power_kw, na.rm = TRUE) / 6 # 10-min intervals
  } else {
    metrics$avg_power = NA
    metrics$total_energy = 0
  }
  
  if('wind_speed_ms' %in% colnames(df)) {
    metrics$avg_wind_speed = mean(df$wind_speed_ms, na.rm = TRUE)
  } else {
    metrics$avg_wind_speed = NA
  }
  
  if('theoretical_power_kwh' %in% colnames(df)) {
    metrics$avg_theoretical = mean(df$theoretical_power_kwh, na.rm = TRUE)
  } 
  
  # Calculate performance based on selected metric
  if('performance_ratio' %in% colnames(df)) {
    if(performance_metric == 'ratio') {
      metrics$avg_efficiency = mean(df$performance_ratio, na.rm = TRUE)
    } else if(performance_metric == 'diff') {
      # Absolute difference
      metrics$avg_efficiency = mean(df$active_power_kw - df$theoretical_power_kwh, na.rm = TRUE)
    } else if(performance_metric == 'percent') {
      # Percentage difference
      metrics$avg_efficiency = mean(
        ifelse(df$theoretical_power_kwh > 0,
               (df$active_power_kw - df$theoretical_power_kwh) / df$theoretical_power_kwh * 100,
               NA),
        na.rm = TRUE
      )
    } else {
      # Default to ratio
      metrics$avg_efficiency = mean(df$performance_ratio, na.rm = TRUE)
    }
  }
  
  if('is_operational' %in% colnames(df)) {
    metrics$availability = mean(df$is_operational, na.rm = TRUE)
  } else {
    metrics$availability = NA
  }
  
  # Binned data metrics
  if(!is.null(binned_data) && nrow(binned_data) > 0) {
    if('avg_power' %in% colnames(binned_data)) {
      metrics$max_power_output = max(binned_data$avg_power, na.rm = TRUE)
      max_idx = which.max(binned_data$avg_power)
      if(length(max_idx) > 0) {
        metrics$optimal_wind_speed = binned_data$wind_speed_bin[max_idx]
      } else {
        metrics$optimal_wind_speed = NA
      }
    } else {
      metrics$max_power_output = NA
      metrics$optimal_wind_speed = NA
    }
  } else {
    metrics$max_power_output = NA
    metrics$optimal_wind_speed = NA
  }
  
  metrics$performance_metric = performance_metric
  
  return(metrics)
}



