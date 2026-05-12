# wind-turbine-dashboard/visualization.R

#' Plot enhanced power curve with multiple layers
#' @param binned_data Binned power curve data
#' @param raw_data Raw data points for scatter
#' @param performance_metric Metric to display
#' @return Plotly object
plot_power_curve_enhanced <- function(binned_data, raw_data = NULL, 
                                      performance_metric = "ratio") {
  
  # Base plot with binned averages
  p <- plot_ly(binned_data, 
               x = ~wind_speed_bin, 
               y = ~avg_power,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Measured (Binned)',
               line = list(color = '#2c3e50', width = 3),
               marker = list(size = 8, color = '#2c3e50'),
               text = ~paste(
                 "Wind Speed:", round(wind_speed_bin, 2), "m/s",
                 "<br>Avg Power:", round(avg_power, 1), "kW",
                 "<br>Theoretical:", round(avg_theoretical, 1), "kWh",
                 "<br>Efficiency:", round(efficiency * 100, 1), "%",
                 "<br>Data Points:", n
               ),
               hoverinfo = 'text'
  )
  
  # Add confidence interval
  p <- p %>%
    add_trace(
      x = ~wind_speed_bin,
      y = ~ci_upper,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'transparent'),
      showlegend = FALSE,
      hoverinfo = 'none'
    ) %>%
    add_trace(
      x = ~wind_speed_bin,
      y = ~ci_lower,
      type = 'scatter',
      mode = 'lines',
      fill = 'tonexty',
      fillcolor = 'rgba(52, 152, 219, 0.2)',
      line = list(color = 'transparent'),
      name = '95% Confidence Interval',
      hoverinfo = 'none'
    )
  
  # Add theoretical power curve
  p <- p %>%
    add_trace(
      x = ~wind_speed_bin,
      y = ~avg_theoretical,
      type = 'scatter',
      mode = 'lines',
      name = 'Theoretical Curve',
      line = list(color = '#e74c3c', width = 3, dash = 'dash'),
      text = ~paste(
        "Theoretical Power:", round(avg_theoretical, 1), "kWh"
      ),
      hoverinfo = 'text'
    )
  
  # Add raw data scatter if provided
  if (!is.null(raw_data)) {
    sample_size <- min(1000, nrow(raw_data))
    sampled_data <- raw_data[sample(nrow(raw_data), sample_size), ]
    
    p <- p %>%
      add_trace(
        data = sampled_data,
        x = ~wind_speed_ms,
        y = ~active_power_kw,
        type = 'scatter',
        mode = 'markers',
        name = 'Raw Data Points',
        marker = list(
          size = 4,
          color = '#3498db',
          opacity = 0.3
        ),
        text = ~paste(
          "Time:", format(timestamp, "%Y-%m-%d %H:%M"),
          "<br>Wind Speed:", round(wind_speed_ms, 2), "m/s",
          "<br>Power:", round(active_power_kw, 1), "kW",
          "<br>Direction:", round(wind_direction_deg, 1), "°"
        ),
        hoverinfo = 'text'
      )
  }
  
  # Customize layout
  p <- p %>%
    layout(
      title = list(
        text = "Wind Turbine Power Curve Analysis",
        font = list(size = 20)
      ),
      xaxis = list(
        title = "Wind Speed (m/s)",
        gridcolor = '#dfe6e9',
        zerolinecolor = '#dfe6e9',
        range = c(0, max(binned_data$wind_speed_bin, na.rm = TRUE) * 1.1)
      ),
      yaxis = list(
        title = "Power Output (kW)",
        gridcolor = '#dfe6e9',
        zerolinecolor = '#dfe6e9'
      ),
      plot_bgcolor = '#ffffff',
      paper_bgcolor = '#ffffff',
      hoverlabel = list(bgcolor = 'white'),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      margin = list(t = 50, b = 100)
    )
  
  return(p)
}

#' Plot performance histogram
#' @param df Processed data frame
#' @return Plotly object
plot_performance_histogram <- function(df) {
  
  plot_ly(
    x = ~df$performance_ratio,
    type = "histogram",
    nbinsx = 30,
    marker = list(
      color = '#3498db',
      line = list(color = '#2980b9', width = 1)
    ),
    text = ~paste(
      "Efficiency:", round(df$performance_ratio * 100, 1), "%"
    ),
    hoverinfo = 'x+y'
  ) %>%
    layout(
      title = "Performance Ratio Distribution",
      xaxis = list(
        title = "Performance Ratio (Measured / Theoretical)",
        tickformat = ".0%"
      ),
      yaxis = list(title = "Frequency"),
      bargap = 0.1
    )
}

plot_power_curve_analysis <- function(binned_data, raw_data = NULL) {
  
  # Check if binned_data exists and has data
  if(is.null(binned_data) || nrow(binned_data) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "No data available for power curve analysis"))
  }
  
  # Base plot with binned averages
  p <- plot_ly(binned_data) %>%
    add_trace(
      x = ~wind_speed_bin,
      y = ~avg_power,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Measured (Binned)',
      line = list(color = '#2c3e50', width = 3),
      marker = list(size = 8, color = '#2c3e50'),
      text = ~paste(
        "Wind Speed:", round(wind_speed_bin, 2), "m/s",
        "<br>Avg Power:", round(avg_power, 1), "kW",
        "<br>Data Points:", n
      ),
      hoverinfo = 'text'
    )
  
  # Add confidence interval if available
  if(all(c("ci_lower", "ci_upper") %in% colnames(binned_data))) {
    p <- p %>%
      add_trace(
        x = ~wind_speed_bin,
        y = ~ci_upper,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'transparent'),
        showlegend = FALSE,
        hoverinfo = 'none'
      ) %>%
      add_trace(
        x = ~wind_speed_bin,
        y = ~ci_lower,
        type = 'scatter',
        mode = 'lines',
        fill = 'tonexty',
        fillcolor = 'rgba(52, 152, 219, 0.2)',
        line = list(color = 'transparent'),
        name = '95% CI',
        showlegend = TRUE
      )
  }
  
  # Add theoretical power curve if available
  if("avg_theoretical" %in% colnames(binned_data)) {
    p <- p %>%
      add_trace(
        x = ~wind_speed_bin,
        y = ~avg_theoretical,
        type = 'scatter',
        mode = 'lines',
        name = 'Theoretical',
        line = list(color = '#e74c3c', width = 3, dash = 'dash'),
        text = ~paste(
          "Theoretical Power:", round(avg_theoretical, 1), "kWh"
        ),
        hoverinfo = 'text'
      )
  }
  
  # Add raw data scatter if provided
  if(!is.null(raw_data) && nrow(raw_data) > 0 && 
     "wind_speed_ms" %in% colnames(raw_data) &&
     "active_power_kw" %in% colnames(raw_data)) {
    
    # Sample data for better performance
    sample_size <- min(1000, nrow(raw_data))
    if(sample_size > 0) {
      sampled_data <- raw_data[sample(nrow(raw_data), sample_size), ]
      
      p <- p %>%
        add_trace(
          data = sampled_data,
          x = ~wind_speed_ms,
          y = ~active_power_kw,
          type = 'scatter',
          mode = 'markers',
          name = 'Raw Data',
          marker = list(
            size = 4,
            color = '#3498db',
            opacity = 0.3
          ),
          text = ~paste(
            "Wind Speed:", round(wind_speed_ms, 2), "m/s",
            "<br>Power:", round(active_power_kw, 1), "kW"
          ),
          hoverinfo = 'text'
        )
    }
  }
  
  # Customize layout
  p <- p %>%
    layout(
      title = list(
        text = "Power Curve Analysis",
        font = list(size = 18)
      ),
      xaxis = list(
        title = "Wind Speed (m/s)",
        gridcolor = '#dfe6e9',
        zerolinecolor = '#dfe6e9'
      ),
      yaxis = list(
        title = "Power Output (kW)",
        gridcolor = '#dfe6e9',
        zerolinecolor = '#dfe6e9'
      ),
      plot_bgcolor = '#ffffff',
      paper_bgcolor = '#ffffff',
      hoverlabel = list(bgcolor = 'white'),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      margin = list(t = 50, b = 100)
    )
  
  return(p)
}

#' Plot wind direction analysis
#' @param df Processed data frame
#' @return Plotly object
plot_wind_direction_analysis <- function(df) {
  
  # Check if required columns exist
  if(is.null(df) || nrow(df) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "No data available for wind direction analysis"))
  }
  
  # Check if wind direction column exists
  if(!"wind_direction_deg" %in% colnames(df)) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "Wind direction data not available in dataset"))
  }
  
  # Check if power column exists
  if(!"active_power_kw" %in% colnames(df)) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "Power data not available in dataset"))
  }
  
  # Create wind direction sectors
  df$wind_direction_sector <- cut(
    df$wind_direction_deg,
    breaks = seq(0, 360, by = 45),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
    include.lowest = TRUE
  )
  
  # Aggregate by wind direction sector
  sector_data <- df %>%
    group_by(wind_direction_sector) %>%
    summarise(
      avg_power = mean(active_power_kw, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    filter(!is.na(wind_direction_sector))
  
  # Create a complete set of directions (even if no data)
  all_directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  sector_data <- sector_data %>%
    right_join(
      data.frame(wind_direction_sector = factor(all_directions, levels = all_directions)),
      by = "wind_direction_sector"
    ) %>%
    mutate(
      avg_power = ifelse(is.na(avg_power), 0, avg_power),
      count = ifelse(is.na(count), 0, count)
    )
  
  # Create polar plot
  plot_ly(
    sector_data,
    type = 'scatterpolar',
    mode = 'lines+markers',
    r = ~avg_power,
    theta = ~wind_direction_sector,
    fill = 'toself',
    fillcolor = 'rgba(52, 152, 219, 0.5)',
    marker = list(
      size = 8,
      color = '#3498db',
      line = list(color = '#2980b9', width = 2)
    ),
    line = list(
      color = '#2980b9',
      width = 2
    ),
    text = ~paste(
      "Direction:", wind_direction_sector,
      "<br>Avg Power:", round(avg_power, 1), "kW",
      "<br>Data Points:", count
    ),
    hoverinfo = 'text'
  ) %>%
    layout(
      title = list(
        text = "Power Output by Wind Direction",
        font = list(size = 16)
      ),
      polar = list(
        radialaxis = list(
          title = "Average Power (kW)",
          gridcolor = '#dfe6e9',
          tickfont = list(size = 10)
        ),
        angularaxis = list(
          direction = "clockwise",
          rotation = 90,
          tickfont = list(size = 11)
        ),
        bgcolor = '#f8f9fa'
      ),
      showlegend = FALSE,
      margin = list(t = 50)
    )
}

#' Plot scatter comparison (measured vs theoretical)
#' @param df Processed data frame
#' @return Plotly object
plot_scatter_comparison <- function(df) {
  
  # Check if data is available
  if(is.null(df) || nrow(df) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No data available",
               annotations = list(
                 text = "Please upload and process data first",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Check if required columns exist
  required_cols <- c("active_power_kw", "theoretical_power_kwh")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if(length(missing_cols) > 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
               annotations = list(
                 text = "Check if your data contains theoretical power information",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Remove rows with NA in required columns
  df_clean <- df %>%
    filter(!is.na(active_power_kw) & !is.na(theoretical_power_kwh))
  
  if(nrow(df_clean) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No valid data points with both measured and theoretical power",
               annotations = list(
                 text = "Theoretical power column might be empty or all NAs",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Sample data for better performance if dataset is large
  sample_size <- min(2000, nrow(df_clean))
  if(sample_size > 0) {
    sampled_data <- df_clean[sample(nrow(df_clean), sample_size), ]
  } else {
    sampled_data <- df_clean
  }
  
  # Calculate performance ratio
  sampled_data$performance_ratio <- sampled_data$active_power_kw / sampled_data$theoretical_power_kwh
  
  # Cap performance ratio for better visualization
  sampled_data$performance_ratio <- pmin(pmax(sampled_data$performance_ratio, 0), 2)
  
  # Determine the maximum value for the perfect match line
  max_value <- max(c(max(sampled_data$active_power_kw, na.rm = TRUE), 
                     max(sampled_data$theoretical_power_kwh, na.rm = TRUE)), 
                   na.rm = TRUE)
  
  # Create the scatter plot
  p <- plot_ly(
    sampled_data,
    x = ~theoretical_power_kwh,
    y = ~active_power_kw,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 6,
      color = ~performance_ratio,
      colorscale = 'Viridis',
      showscale = TRUE,
      colorbar = list(
        title = "Performance<br>Ratio",
        tickformat = ".2f"
      ),
      opacity = 0.7,
      line = list(width = 0.5, color = 'darkgray')
    ),
    text = ~paste(
      "Theoretical Power:", round(theoretical_power_kwh, 1), "kWh",
      "<br>Measured Power:", round(active_power_kw, 1), "kW",
      "<br>Performance Ratio:", round(performance_ratio, 2),
      "<br>Wind Speed:", ifelse("wind_speed_ms" %in% colnames(df_clean), 
                                round(wind_speed_ms, 1), "N/A"), "m/s"
    ),
    hoverinfo = 'text',
    name = 'Data Points'
  )
  
  # Create a separate data frame for the perfect match line
  line_df <- data.frame(
    x = c(0, max_value),
    y = c(0, max_value)
  )
  
  # Add perfect match line (y = x) - CORRECTED
  p <- p %>%
    add_trace(
      data = line_df,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'red', dash = 'dash', width = 2),
      name = 'Perfect Match (y = x)',
      showlegend = TRUE,
      hoverinfo = 'none',
      inherit = FALSE  # Don't inherit aesthetics from main plot
    )
  
  # Add zero-power reference lines
  zero_line_x <- data.frame(x = c(0, max_value), y = c(0, 0))
  zero_line_y <- data.frame(x = c(0, 0), y = c(0, max_value))
  
  p <- p %>%
    add_trace(
      data = zero_line_x,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'gray', width = 1, dash = 'dot'),
      name = 'Zero Power',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    ) %>%
    add_trace(
      data = zero_line_y,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'gray', width = 1, dash = 'dot'),
      name = 'Zero Theoretical',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
  
  # Customize layout
  p <- p %>%
    layout(
      title = list(
        text = "Measured vs Theoretical Power Comparison",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Theoretical Power (kWh)",
        gridcolor = '#e0e0e0',
        zerolinecolor = '#e0e0e0',
        range = c(0, max_value * 1.05)
      ),
      yaxis = list(
        title = "Measured Power (kW)",
        gridcolor = '#e0e0e0',
        zerolinecolor = '#e0e0e0',
        range = c(0, max_value * 1.05)
      ),
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = '#f8f9fa',
      hoverlabel = list(
        bgcolor = 'white',
        font = list(size = 12)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center",
        bgcolor = 'rgba(255, 255, 255, 0.8)'
      ),
      margin = list(t = 50, b = 100, l = 80, r = 80)
    )
  
  return(p)
}

#' Plot detailed power curve
#' @param binned_data Binned power curve data
#' @return Plotly object
plot_detailed_power_curve <- function(binned_data) {
  
  if(is.null(binned_data) || nrow(binned_data) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No binned data available",
               annotations = list(
                 text = "Please run the analysis first",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Create the detailed plot
  p <- plot_ly(binned_data) %>%
    add_trace(
      x = ~wind_speed_bin,
      y = ~avg_power,
      type = 'bar',
      name = 'Average Power',
      marker = list(
        color = ~avg_power,
        colorscale = 'Blues',
        showscale = TRUE,
        colorbar = list(title = "Power (kW)")
      ),
      text = ~paste(
        "Wind Speed:", round(wind_speed_bin, 2), "m/s",
        "<br>Avg Power:", round(avg_power, 1), "kW",
        "<br>Data Points:", n
      ),
      hoverinfo = 'text'
    )
  
  # Add error bars if available
  if(all(c("ci_lower", "ci_upper") %in% colnames(binned_data))) {
    p <- p %>%
      add_trace(
        x = ~wind_speed_bin,
        y = ~avg_power,
        type = 'scatter',
        mode = 'markers',
        error_y = list(
          array = ~(ci_upper - avg_power),
          arrayminus = ~(avg_power - ci_lower),
          color = '#2c3e50',
          width = 3
        ),
        marker = list(
          size = 8,
          color = '#e74c3c',
          symbol = 'diamond'
        ),
        name = 'With 95% CI',
        showlegend = TRUE
      )
  }
  
  # Add theoretical curve if available
  if("avg_theoretical" %in% colnames(binned_data)) {
    p <- p %>%
      add_trace(
        x = ~wind_speed_bin,
        y = ~avg_theoretical,
        type = 'scatter',
        mode = 'lines',
        name = 'Theoretical',
        line = list(color = '#2ecc71', width = 3, dash = 'dot'),
        text = ~paste(
          "Theoretical:", round(avg_theoretical, 1), "kWh"
        ),
        hoverinfo = 'text'
      )
  }
  
  p <- p %>%
    layout(
      title = list(
        text = "Detailed Power Curve Analysis",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Wind Speed (m/s)",
        gridcolor = '#dfe6e9'
      ),
      yaxis = list(
        title = "Power Output (kW)",
        gridcolor = '#dfe6e9'
      ),
      barmode = 'group',
      plot_bgcolor = '#ffffff',
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      )
    )
  
  return(p)
}

#' Plot time series analysis
#' @param df Processed data frame
#' @return Plotly object
plot_timeseries_analysis <- function(df) {
  
  # Daily aggregation
  daily_data <- df %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(
      avg_power = mean(active_power_kw, na.rm = TRUE),
      avg_wind_speed = mean(wind_speed_ms, na.rm = TRUE),
      total_energy = sum(active_power_kw, na.rm = TRUE) / 6,
      avg_efficiency = mean(performance_ratio, na.rm = TRUE)
    )
  
  plot_ly(daily_data) %>%
    add_trace(
      x = ~date,
      y = ~avg_power,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Average Power',
      yaxis = 'y1',
      line = list(color = '#3498db', width = 2),
      marker = list(size = 6)
    ) %>%
    add_trace(
      x = ~date,
      y = ~avg_efficiency * 100,
      type = 'scatter',
      mode = 'lines',
      name = 'Efficiency (%)',
      yaxis = 'y2',
      line = list(color = '#2ecc71', width = 2, dash = 'dot')
    ) %>%
    layout(
      title = "Daily Performance Trends",
      xaxis = list(title = "Date"),
      yaxis = list(
        title = "Average Power (kW)",
        side = 'left',
        color = '#3498db'
      ),
      yaxis2 = list(
        title = "Efficiency (%)",
        overlaying = "y",
        side = "right",
        color = '#2ecc71',
        range = c(0, 100)
      ),
      plot_bgcolor = '#ffffff',
      hovermode = 'x unified'
    )
}

#' Plot data distribution
#' @param df Processed data frame
#' @return Plotly object
plot_data_distribution <- function(df) {
  
  # Create subplots
  p1 <- plot_ly(
    x = ~df$wind_speed_ms,
    type = 'histogram',
    nbinsx = 30,
    name = 'Wind Speed',
    marker = list(color = '#3498db')
  )
  
  p2 <- plot_ly(
    x = ~df$active_power_kw,
    type = 'histogram',
    nbinsx = 30,
    name = 'Active Power',
    marker = list(color = '#2ecc71')
  )
  
  p3 <- plot_ly(
    x = ~df$performance_ratio,
    type = 'histogram',
    nbinsx = 30,
    name = 'Performance Ratio',
    marker = list(color = '#e74c3c')
  )
  
  subplot(p1, p2, p3, nrows = 1, shareY = TRUE) %>%
    layout(
      title = "Data Distribution Analysis",
      showlegend = FALSE,
      xaxis = list(title = "Wind Speed (m/s)"),
      xaxis2 = list(title = "Active Power (kW)"),
      xaxis3 = list(title = "Performance Ratio"),
      yaxis = list(title = "Frequency")
    )
}

#' Plot missing data analysis
#' @param df Raw data frame
#' @return Plotly object
plot_missing_data <- function(df) {
  
  missing_summary <- data.frame(
    column = colnames(df),
    missing = sapply(df, function(x) sum(is.na(x))),
    total = nrow(df)
  ) %>%
    mutate(
      percentage = missing / total * 100,
      column = factor(column, levels = column[order(percentage)])
    )
  
  plot_ly(
    missing_summary,
    x = ~percentage,
    y = ~column,
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = ~percentage,
      colorscale = 'Reds',
      showscale = TRUE,
      colorbar = list(title = "% Missing")
    ),
    text = ~paste0(round(percentage, 1), "% (", missing, "/", total, ")"),
    textposition = 'outside'
  ) %>%
    layout(
      title = "Missing Data Analysis",
      xaxis = list(title = "Percentage Missing", range = c(0, 100)),
      yaxis = list(title = ""),
      bargap = 0.2
    )
}

#' Plot outlier analysis
#' @param df Processed data frame
#' @return Plotly object
plot_outlier_analysis <- function(df) {
  
  # Box plots for key variables
  plot_ly(
    type = 'box'
  ) %>%
    add_boxplot(
      y = ~df$wind_speed_ms,
      name = 'Wind Speed',
      boxpoints = 'suspectedoutliers',
      marker = list(color = '#3498db'),
      line = list(color = '#2980b9')
    ) %>%
    add_boxplot(
      y = ~df$active_power_kw,
      name = 'Active Power',
      boxpoints = 'suspectedoutliers',
      marker = list(color = '#2ecc71'),
      line = list(color = '#27ae60')
    ) %>%
    add_boxplot(
      y = ~df$performance_ratio,
      name = 'Performance Ratio',
      boxpoints = 'suspectedoutliers',
      marker = list(color = '#e74c3c'),
      line = list(color = '#c0392b')
    ) %>%
    layout(
      title = "Outlier Detection (Box Plots)",
      yaxis = list(title = "Value"),
      showlegend = FALSE
    )
}

#' Plot deviation analysis with selected metric
#' @param binned_data Binned power curve data
#' @param performance_metric Metric to use ("ratio", "diff", or "percent")
#' @return Plotly object
plot_deviation_analysis <- function(binned_data, performance_metric = "ratio") {
  
  if(is.null(binned_data) || nrow(binned_data) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "No data available for deviation analysis"))
  }
  
  # Check if theoretical power exists
  if(!"avg_theoretical" %in% colnames(binned_data)) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "Theoretical power data not available"))
  }
  
  # Get performance metric from binned_data if not provided
  if(is.null(performance_metric) && "metric_type" %in% colnames(binned_data)) {
    performance_metric <- binned_data$metric_type[1]
  }
  
  # Default to ratio if performance_metric is NULL
  if(is.null(performance_metric)) {
    performance_metric <- "ratio"
  }
  
  # Calculate deviation based on selected metric
  if(performance_metric == "ratio") {
    binned_data$deviation <- ifelse(
      binned_data$avg_theoretical > 0,
      (binned_data$avg_power / binned_data$avg_theoretical),
      NA
    )
    y_title <- "Performance Ratio"
    reference_line <- 1
    colorscale <- 'Viridis'
  } else if(performance_metric == "diff") {
    binned_data$deviation <- binned_data$avg_power - binned_data$avg_theoretical
    y_title <- "Power Difference (kW)"
    reference_line <- 0
    colorscale <- 'RdBu'
    reversescale <- TRUE
  } else if(performance_metric == "percent") {
    binned_data$deviation <- ifelse(
      binned_data$avg_theoretical > 0,
      ((binned_data$avg_power - binned_data$avg_theoretical) / binned_data$avg_theoretical) * 100,
      NA
    )
    y_title <- "Percentage Difference (%)"
    reference_line <- 0
    colorscale <- 'RdBu'
    reversescale <- TRUE
  } else {
    # Default to ratio
    binned_data$deviation <- ifelse(
      binned_data$avg_theoretical > 0,
      (binned_data$avg_power / binned_data$avg_theoretical),
      NA
    )
    y_title <- "Performance Ratio"
    reference_line <- 1
    colorscale <- 'Viridis'
    reversescale <- FALSE
  }
  
  # Remove NA deviations
  plot_data <- binned_data[!is.na(binned_data$deviation), ]
  
  if(nrow(plot_data) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(title = "No valid deviation data available"))
  }
  
  # Create the plot
  p <- plot_ly(
    plot_data,
    x = ~wind_speed_bin,
    y = ~deviation,
    type = 'bar',
    marker = list(
      color = ~deviation,
      colorscale = colorscale,
      reversescale = if(exists("reversescale")) reversescale else FALSE,
      showscale = TRUE,
      colorbar = list(title = y_title),
      line = list(color = 'black', width = 0.5)
    ),
    text = ~paste(
      "Wind Speed:", round(wind_speed_bin, 2), "m/s",
      "<br>Measured Power:", round(avg_power, 1), "kW",
      "<br>Theoretical Power:", round(avg_theoretical, 1), "kW"
    ),
    hoverinfo = 'text'
  )
  
  # Add tooltip text based on metric
  if(performance_metric == "ratio") {
    p <- p %>% add_trace(
      text = ~paste(
        "Wind Speed:", round(wind_speed_bin, 2), "m/s",
        "<br>Performance Ratio:", round(deviation, 2),
        "<br>Measured Power:", round(avg_power, 1), "kW",
        "<br>Theoretical Power:", round(avg_theoretical, 1), "kW",
        "<br>Difference:", round((deviation-1)*100, 1), "%"
      ),
      hoverinfo = 'text'
    )
  } else if(performance_metric == "diff") {
    p <- p %>% add_trace(
      text = ~paste(
        "Wind Speed:", round(wind_speed_bin, 2), "m/s",
        "<br>Power Difference:", round(deviation, 1), "kW",
        "<br>Measured Power:", round(avg_power, 1), "kW",
        "<br>Theoretical Power:", round(avg_theoretical, 1), "kW"
      ),
      hoverinfo = 'text'
    )
  } else if(performance_metric == "percent") {
    p <- p %>% add_trace(
      text = ~paste(
        "Wind Speed:", round(wind_speed_bin, 2), "m/s",
        "<br>Percentage Difference:", round(deviation, 1), "%",
        "<br>Measured Power:", round(avg_power, 1), "kW",
        "<br>Theoretical Power:", round(avg_theoretical, 1), "kW"
      ),
      hoverinfo = 'text'
    )
  }
  
  p <- p %>%
    layout(
      title = list(
        text = paste("Power Curve Deviations -", y_title),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Wind Speed (m/s)",
        gridcolor = '#dfe6e9'
      ),
      yaxis = list(
        title = y_title,
        gridcolor = '#dfe6e9'
      ),
      shapes = list(
        list(
          type = 'line',
          x0 = min(plot_data$wind_speed_bin, na.rm = TRUE),
          x1 = max(plot_data$wind_speed_bin, na.rm = TRUE),
          y0 = reference_line,
          y1 = reference_line,
          line = list(color = 'black', width = 2, dash = 'dot')
        )
      ),
      plot_bgcolor = '#ffffff',
      bargap = 0.1
    )
  
  return(p)
}

#' Plot wind speed vs direction matrix
#' @param df Processed data frame
#' @param analysis_type Type of analysis ("heatmap", "contour", or "density")
#' @param matrix_bins Number of bins for the analysis
#' @return Plotly object
plot_wind_matrix <- function(df, analysis_type = "heatmap", matrix_bins = 20) {
  
  # Check if data is available
  if(is.null(df) || nrow(df) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No data available",
               annotations = list(
                 text = "Please upload and process data first",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Check if required columns exist
  required_cols <- c("wind_speed_ms", "wind_direction_deg")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if(length(missing_cols) > 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
               annotations = list(
                 text = "Wind speed and direction data are required",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Remove rows with NA in required columns
  df_clean <- df %>%
    filter(!is.na(wind_speed_ms) & !is.na(wind_direction_deg))
  
  if(nrow(df_clean) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No valid data points with both wind speed and direction",
               annotations = list(
                 text = "Check your data for missing values",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Sample data for better performance if dataset is large
  sample_size <- min(5000, nrow(df_clean))
  if(sample_size > 0) {
    sampled_data <- df_clean[sample(nrow(df_clean), sample_size), ]
  } else {
    sampled_data <- df_clean
  }
  
  # Create plot based on analysis type
  if(analysis_type == "heatmap") {
    # Create 2D histogram for heatmap
    p <- plot_ly(
      sampled_data,
      x = ~wind_speed_ms,
      y = ~wind_direction_deg,
      type = 'histogram2d',
      colorscale = 'Viridis',
      nbinsx = matrix_bins,
      nbinsy = matrix_bins,
      colorbar = list(title = "Count")
    ) %>%
      layout(
        title = "Wind Speed vs Direction Heatmap",
        xaxis = list(title = "Wind Speed (m/s)"),
        yaxis = list(title = "Wind Direction (°)")
      )
    
  } else if(analysis_type == "contour") {
    # Create contour plot
    # First, create a 2D density estimate
    kde <- MASS::kde2d(
      sampled_data$wind_speed_ms,
      sampled_data$wind_direction_deg,
      n = matrix_bins
    )
    
    p <- plot_ly(
      x = kde$x,
      y = kde$y,
      z = kde$z,
      type = "contour",
      colorscale = 'Viridis',
      contours = list(
        showlabels = TRUE,
        labelfont = list(size = 12, color = 'white')
      ),
      colorbar = list(title = "Density")
    ) %>%
      layout(
        title = "Wind Speed vs Direction Contour Plot",
        xaxis = list(title = "Wind Speed (m/s)"),
        yaxis = list(title = "Wind Direction (°)")
      )
    
  } else if(analysis_type == "density") {
    # Create 2D density plot with scatter overlay
    kde <- MASS::kde2d(
      sampled_data$wind_speed_ms,
      sampled_data$wind_direction_deg,
      n = matrix_bins
    )
    
    # Create the density surface
    p <- plot_ly(
      x = kde$x,
      y = kde$y,
      z = kde$z,
      type = "surface",
      colorscale = 'Viridis',
      contours = list(
        z = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "#ff0000",
          project = list(z = TRUE)
        )
      ),
      colorbar = list(title = "Density")
    ) %>%
      layout(
        title = "Wind Speed vs Direction 3D Density",
        scene = list(
          xaxis = list(title = "Wind Speed (m/s)"),
          yaxis = list(title = "Wind Direction (°)"),
          zaxis = list(title = "Density"),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 1.5)
          )
        )
      )
  } else {
    # Default to scatter if analysis_type is not recognized
    p <- plot_ly(
      sampled_data,
      x = ~wind_speed_ms,
      y = ~wind_direction_deg,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 5,
        color = ~active_power_kw,
        colorscale = 'Viridis',
        showscale = TRUE,
        colorbar = list(title = "Power (kW)"),
        opacity = 0.6
      ),
      text = ~paste(
        "Wind Speed:", round(wind_speed_ms, 1), "m/s",
        "<br>Wind Direction:", round(wind_direction_deg, 1), "°",
        "<br>Power:", ifelse("active_power_kw" %in% colnames(sampled_data),
                             round(active_power_kw, 1), "N/A"), "kW"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Wind Speed vs Direction Scatter Plot",
        xaxis = list(title = "Wind Speed (m/s)"),
        yaxis = list(title = "Wind Direction (°)")
      )
  }
  
  # Add common layout elements
  p <- p %>%
    layout(
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = '#f8f9fa',
      margin = list(t = 50)
    )
  
  return(p)
}


#' Plot hourly performance patterns
#' @param df Processed data frame
#' @return Plotly object
plot_hourly_patterns <- function(df) {
  
  # Check if data is available
  if(is.null(df) || nrow(df) == 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = "No data available",
               annotations = list(
                 text = "Please upload and process data first",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Check if required columns exist
  required_cols <- c("timestamp", "active_power_kw")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if(length(missing_cols) > 0) {
    return(plotly_empty(type = "scatter") %>%
             layout(
               title = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
               annotations = list(
                 text = "Timestamp and active power columns are required",
                 xref = "paper",
                 yref = "paper",
                 x = 0.5,
                 y = 0.5,
                 showarrow = FALSE
               )
             ))
  }
  
  # Extract hour from timestamp
  if(!"hour" %in% colnames(df)) {
    df$hour <- lubridate::hour(df$timestamp)
  }
  
  # Group by hour and calculate statistics
  hourly_data <- df %>%
    group_by(hour) %>%
    summarise(
      avg_power = mean(active_power_kw, na.rm = TRUE),
      avg_wind = ifelse("wind_speed_ms" %in% colnames(df), 
                        mean(wind_speed_ms, na.rm = TRUE), 
                        NA),
      avg_efficiency = ifelse("performance_ratio" %in% colnames(df),
                              mean(performance_ratio, na.rm = TRUE),
                              NA),
      count = n(),
      .groups = 'drop'
    ) %>%
    arrange(hour)
  
  # Create a list to store plots
  plots <- list()
  
  # Create power plot
  p1 <- plot_ly(hourly_data) %>%
    add_trace(
      x = ~hour,
      y = ~avg_power,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Avg Power',
      line = list(color = '#3498db', width = 3),
      marker = list(size = 8, color = '#3498db'),
      text = ~paste(
        "Hour:", hour,
        "<br>Avg Power:", round(avg_power, 1), "kW",
        "<br>Data Points:", count
      ),
      hoverinfo = 'text'
    ) %>%
    layout(
      yaxis = list(title = "Avg Power (kW)", gridcolor = '#dfe6e9'),
      xaxis = list(title = "Hour of Day", showticklabels = FALSE)
    )
  
  plots[[1]] <- p1
  
  # Create wind speed plot if data exists
  if(!all(is.na(hourly_data$avg_wind))) {
    p2 <- plot_ly(hourly_data) %>%
      add_trace(
        x = ~hour,
        y = ~avg_wind,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Avg Wind',
        line = list(color = '#2ecc71', width = 3),
        marker = list(size = 8, color = '#2ecc71'),
        text = ~paste(
          "Hour:", hour,
          "<br>Avg Wind Speed:", round(avg_wind, 2), "m/s"
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        yaxis = list(title = "Avg Wind (m/s)", gridcolor = '#dfe6e9'),
        xaxis = list(title = "Hour of Day", showticklabels = FALSE)
      )
    
    plots[[2]] <- p2
  }
  
  # Create efficiency plot if data exists
  if(!all(is.na(hourly_data$avg_efficiency))) {
    p3 <- plot_ly(hourly_data) %>%
      add_trace(
        x = ~hour,
        y = ~avg_efficiency * 100,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Efficiency',
        line = list(color = '#e74c3c', width = 3),
        marker = list(size = 8, color = '#e74c3c'),
        text = ~paste(
          "Hour:", hour,
          "<br>Avg Efficiency:", round(avg_efficiency * 100, 1), "%"
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        yaxis = list(title = "Efficiency (%)", gridcolor = '#dfe6e9'),
        xaxis = list(title = "Hour of Day")
      )
    
    if(length(plots) == 1) {
      plots[[2]] <- p3
    } else {
      plots[[3]] <- p3
    }
  }
  
  # Determine number of rows for subplot
  nrows <- length(plots)
  
  # Create subplot
  subplot(plots, nrows = nrows, shareX = TRUE, titleY = TRUE) %>%
    layout(
      title = list(
        text = "Hourly Performance Patterns",
        font = list(size = 18)
      ),
      showlegend = FALSE,
      hovermode = 'x unified',
      plot_bgcolor = '#ffffff',
      margin = list(t = 50)
    )
}
