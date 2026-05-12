# wind-turbine-dashboard/data_processing_fixed.R

#' Preprocess raw turbine data for your specific format
#' @param df Raw data frame with your exact column names
#' @return Processed data frame
preprocess_turbine_data <- function(df) {
  
  cat("=== STARTING DATA PROCESSING ===\n")
  cat("Original column names:", paste(colnames(df), collapse = ", "), "\n")
  cat("Number of rows:", nrow(df), "\n")
  
  # Create a copy
  data <- df
  
  # STEP 1: Handle column names - based on your sample data
  # Your columns are: Date/Time, LV ActivePower (kW), Wind Speed (m/s), Theoretical_Power_Curve (KWh), Wind Direction (°)
  
  # First, let's see what we actually have
  cat("\n--- Original Columns ---\n")
  for(i in 1:ncol(data)) {
    cat(i, ":", colnames(data)[i], "\n")
  }
  
  # Rename columns to standardized names
  # We need to be flexible because CSV might have different encoding
  colnames(data) <- gsub("\"", "", colnames(data))  # Remove quotes if present
  colnames(data) <- trimws(colnames(data))  # Remove whitespace
  
  cat("\n--- After cleaning column names ---\n")
  print(colnames(data))
  
  # Map columns based on your sample data
  # Create a mapping of possible names to our standard names
  col_mappings <- list(
    "timestamp" = c("Date/Time", "datetime", "timestamp", "Time", "Date"),
    "active_power_kw" = c("LV ActivePower \\(kW\\)", "ActivePower", "Power", "LV ActivePower"),
    "wind_speed_ms" = c("Wind Speed \\(m/s\\)", "WindSpeed", "WS", "wind_speed"),
    "theoretical_power_kwh" = c("Theoretical_Power_Curve \\(KWh\\)", "TheoreticalPower", "ExpectedPower"),
    "wind_direction_deg" = c("Wind Direction \\(°\\)", "WindDirection", "WD")
  )
  
  # Function to find and rename columns
  find_and_rename <- function(data, target_name, possible_names) {
    for(pattern in possible_names) {
      # Look for columns matching the pattern
      matches <- grep(pattern, colnames(data), ignore.case = TRUE, value = TRUE)
      if(length(matches) > 0) {
        cat("Found column for", target_name, ":", matches[1], "\n")
        colnames(data)[colnames(data) == matches[1]] <- target_name
        return(list(data = data, found = TRUE))
      }
    }
    return(list(data = data, found = FALSE))
  }
  
  # Apply mappings
  for(target_name in names(col_mappings)) {
    result <- find_and_rename(data, target_name, col_mappings[[target_name]])
    data <- result$data
    if(!result$found) {
      cat("WARNING: Could not find column for", target_name, "\n")
    }
  }
  
  cat("\n--- Final column names ---\n")
  print(colnames(data))
  
  # STEP 2: Convert timestamp
  cat("\n--- Converting timestamp ---\n")
  
  # Try different timestamp formats
  timestamp_formats <- c(
    "%d %m %Y %H:%M",      # "01 01 2018 00:00" - YOUR FORMAT
    "%d/%m/%Y %H:%M",      # "01/01/2018 00:00"
    "%Y-%m-%d %H:%M:%S",   # "2018-01-01 00:00:00"
    "%m/%d/%Y %H:%M",      # "01/01/2018 00:00" (US)
    "%Y%m%d %H:%M",        # "20180101 00:00"
    "%d.%m.%Y %H:%M"       # "01.01.2018 00:00"
  )
  
  # Store original timestamp for debugging
  if("timestamp" %in% colnames(data)) {
    cat("First timestamp value:", as.character(data$timestamp[1]), "\n")
    
    # Try each format
    converted <- NULL
    for(fmt in timestamp_formats) {
      tryCatch({
        temp_converted <- as.POSIXct(data$timestamp, format = fmt, tz = "UTC")
        if(!all(is.na(temp_converted))) {
          converted <- temp_converted
          cat("Successfully converted with format:", fmt, "\n")
          cat("Sample converted:", as.character(converted[1]), "\n")
          break
        }
      }, error = function(e) {})
    }
    
    if(!is.null(converted)) {
      data$timestamp <- converted
    } else {
      # Last resort: try automatic conversion
      data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
      cat("Used automatic conversion\n")
    }
  } else {
    cat("ERROR: No timestamp column found!\n")
  }
  
  # STEP 3: Convert numeric columns
  cat("\n--- Converting numeric columns ---\n")
  
  numeric_columns <- c("active_power_kw", "wind_speed_ms", 
                       "theoretical_power_kwh", "wind_direction_deg")
  
  for(col in numeric_columns) {
    if(col %in% colnames(data)) {
      data[[col]] <- as.numeric(data[[col]])
      cat(col, "converted to numeric. First value:", data[[col]][1], "\n")
    }
  }
  
  # STEP 4: Remove NA values in critical columns
  cat("\n--- Removing NA values ---\n")
  initial_rows <- nrow(data)
  
  # Check which critical columns exist
  critical_cols <- c()
  if("timestamp" %in% colnames(data)) critical_cols <- c(critical_cols, "timestamp")
  if("active_power_kw" %in% colnames(data)) critical_cols <- c(critical_cols, "active_power_kw")
  if("wind_speed_ms" %in% colnames(data)) critical_cols <- c(critical_cols, "wind_speed_ms")
  
  if(length(critical_cols) > 0) {
    # Create a logical vector of complete cases for critical columns
    complete_cases <- complete.cases(data[, critical_cols, drop = FALSE])
    data <- data[complete_cases, ]
    
    removed <- initial_rows - nrow(data)
    cat("Removed", removed, "rows with NA values in critical columns\n")
  } else {
    cat("WARNING: No critical columns found!\n")
  }
  
  # STEP 5: Add calculated columns
  cat("\n--- Adding calculated columns ---\n")
  
  # Calculate performance ratio if we have theoretical power
  if("theoretical_power_kwh" %in% colnames(data) && "active_power_kw" %in% colnames(data)) {
    data$performance_ratio <- ifelse(
      data$theoretical_power_kwh > 0,
      data$active_power_kw / data$theoretical_power_kwh,
      NA
    )
    data$power_difference <- data$active_power_kw - data$theoretical_power_kwh
    cat("Added performance ratio column\n")
  }
  
  # Add time-based features
  if("timestamp" %in% colnames(data)) {
    data$date <- as.Date(data$timestamp)
    data$hour <- lubridate::hour(data$timestamp)
    data$month <- lubridate::month(data$timestamp, label = TRUE)
    data$day_of_week <- weekdays(data$timestamp, abbreviate = TRUE)
    cat("Added time-based features\n")
  }
  
  # Add operational status
  if("active_power_kw" %in% colnames(data)) {
    data$is_operational <- data$active_power_kw > 10
  }
  
  cat("\n=== FINAL DATA SUMMARY ===\n")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", ncol(data), "\n")
  cat("Column names:", paste(colnames(data), collapse = ", "), "\n")
  cat("Date range:", if("timestamp" %in% colnames(data)) {
    paste(range(data$timestamp, na.rm = TRUE), collapse = " to ")
  }, "\n")
  
  return(data)
}

#' Create power curve bins with performance metrics
#' @param df Processed data frame
#' @param bin_width Width of wind speed bins in m/s
#' @param min_points Minimum data points required per bin
#' @param performance_metric Metric to use ("ratio", "diff", or "percent")
#' @return Binned data frame
create_power_curve_bins <- function(df, bin_width = 0.5, min_points = 10, performance_metric = "ratio") {
  
  if(is.null(df) || nrow(df) == 0) {
    warning("No data to bin")
    return(NULL)
  }
  
  cat("Creating power curve bins with metric:", performance_metric, "\n")
  
  # Check if we have required columns
  if(!"wind_speed_ms" %in% colnames(df)) {
    stop("wind_speed_ms column not found")
  }
  if(!"active_power_kw" %in% colnames(df)) {
    stop("active_power_kw column not found")
  }
  
  # Create wind speed bins
  df$wind_speed_bin <- floor(df$wind_speed_ms / bin_width) * bin_width + bin_width / 2
  
  # Group by bin
  binned_data <- df %>%
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
  if("theoretical_power_kwh" %in% colnames(df)) {
    theoretical_avg <- df %>%
      group_by(wind_speed_bin) %>%
      summarise(
        avg_theoretical = mean(theoretical_power_kwh, na.rm = TRUE),
        .groups = 'drop'
      )
    
    binned_data <- binned_data %>%
      left_join(theoretical_avg, by = "wind_speed_bin")
    
    # Calculate performance based on selected metric
    if(performance_metric == "ratio") {
      binned_data$performance_metric <- ifelse(
        binned_data$avg_theoretical > 0,
        binned_data$avg_power / binned_data$avg_theoretical,
        NA
      )
    } else if(performance_metric == "diff") {
      binned_data$performance_metric <- binned_data$avg_power - binned_data$avg_theoretical
    } else if(performance_metric == "percent") {
      binned_data$performance_metric <- ifelse(
        binned_data$avg_theoretical > 0,
        ((binned_data$avg_power - binned_data$avg_theoretical) / binned_data$avg_theoretical) * 100,
        NA
      )
    } else {
      # Default to ratio
      binned_data$performance_metric <- ifelse(
        binned_data$avg_theoretical > 0,
        binned_data$avg_power / binned_data$avg_theoretical,
        NA
      )
    }
  }
  
  # Calculate confidence intervals
  binned_data <- binned_data %>%
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
  binned_data$metric_type <- performance_metric
  
  cat("Created", nrow(binned_data), "bins\n")
  
  return(binned_data)
}

#' Calculate performance metrics
#' @param df Processed data frame
#' @param binned_data Binned power curve data
#' @param performance_metric Metric to use ("ratio", "diff", or "percent")
#' @return List of performance metrics
calculate_performance_metrics <- function(df, binned_data = NULL, performance_metric = "ratio") {
  
  cat("Calculating performance metrics with metric:", performance_metric, "\n")
  
  metrics <- list()
  
  # Basic statistics
  metrics$data_points <- nrow(df)
  
  if("active_power_kw" %in% colnames(df)) {
    metrics$avg_power <- mean(df$active_power_kw, na.rm = TRUE)
    metrics$total_energy <- sum(df$active_power_kw, na.rm = TRUE) / 6  # 10-minute intervals
  }
  
  if("wind_speed_ms" %in% colnames(df)) {
    metrics$avg_wind_speed <- mean(df$wind_speed_ms, na.rm = TRUE)
  }
  
  if("theoretical_power_kwh" %in% colnames(df)) {
    metrics$avg_theoretical <- mean(df$theoretical_power_kwh, na.rm = TRUE)
  }
  
  # Calculate performance based on selected metric
  if("performance_ratio" %in% colnames(df)) {
    if(performance_metric == "ratio") {
      metrics$avg_efficiency <- mean(df$performance_ratio, na.rm = TRUE)
    } else if(performance_metric == "diff") {
      # Absolute difference
      metrics$avg_efficiency <- mean(df$active_power_kw - df$theoretical_power_kwh, na.rm = TRUE)
    } else if(performance_metric == "percent") {
      # Percentage difference
      metrics$avg_efficiency <- mean(
        ifelse(df$theoretical_power_kwh > 0,
               (df$active_power_kw - df$theoretical_power_kwh) / df$theoretical_power_kwh * 100,
               NA),
        na.rm = TRUE
      )
    } else {
      # Default to ratio
      metrics$avg_efficiency <- mean(df$performance_ratio, na.rm = TRUE)
    }
  }
  
  if("is_operational" %in% colnames(df)) {
    metrics$availability <- mean(df$is_operational, na.rm = TRUE) * 100
  }
  
  # Binned data metrics
  if(!is.null(binned_data) && nrow(binned_data) > 0) {
    if("avg_power" %in% colnames(binned_data)) {
      metrics$max_power_output <- max(binned_data$avg_power, na.rm = TRUE)
      max_idx <- which.max(binned_data$avg_power)
      if(length(max_idx) > 0) {
        metrics$optimal_wind_speed <- binned_data$wind_speed_bin[max_idx]
      }
    }
  }
  
  metrics$performance_metric <- performance_metric
  
  return(metrics)
}
