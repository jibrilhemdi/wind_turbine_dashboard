# wind-turbine-dashboard/app.R

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)
library(shinyWidgets)
library(shinycssloaders)

# Source helper functions
source("data_processing.R", local = TRUE, encoding = "UTF-8")
source("visualization.R", local = TRUE, encoding = "UTF-8")

# UI Definition using shinydashboard
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "Turkey Wind Turbine Performance Dashboard",
    titleWidth = 450,
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "Data analysis complete",
        icon = icon("check-circle"),
        status = "success"
      )
    )
  ),

  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Power Curve Analysis", tabName = "powercurve", icon = icon("chart-line")),
      menuItem("Data Quality", tabName = "dataquality", icon = icon("search")),
      menuItem("Advanced Analysis", tabName = "advanced", icon = icon("cogs")),
      menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
    ),
    
    hr(),
    
    # File upload
    box(
      title = "Data Upload",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      fileInput("data_file", "Upload CSV File",
                accept = ".csv",
                buttonLabel = "Browse...",
                placeholder = "No file selected"),

      # Date range
      dateRangeInput("date_range", "Date Range:",
                     start = "2018-01-01",
                     end = "2018-01-31"),

      # Filters
      sliderInput("wind_speed_range", "Wind Speed (m/s):",
                  min = 0, max = 25, value = c(0, 25), step = 0.5),
      sliderInput("power_range", "Active Power (kW):",
                  min = 0, max = 10000, value = c(0, 10000), step = 50)
    ),

    # Analysis parameters
    box(
      title = "Analysis Parameters",
      width = 12,
      solidHeader = TRUE,
      status = "info",
      collapsible = TRUE,
      collapsed = TRUE,
      numericInput("bin_width", "Wind Speed Bin Width (m/s):",
                   value = 0.5, min = 0.1, max = 2, step = 0.1),
      numericInput("min_data_points", "Min Points per Bin:",
                   value = 10, min = 5, max = 100),
      selectInput("performance_metric", "Performance Metric:",
                  choices = c("Power Ratio" = "ratio",
                              "Absolute Difference" = "diff",
                              "Percentage" = "percent"),
                  selected = "ratio"),

      br(),
      actionButton("analyze", "Run Analysis",
                   class = "btn-primary",
                   icon = icon("play"),
                   width = "90%")
    ),

    # Export options
    box(
      title = "Export",
      width = 12,
      solidHeader = TRUE,
      status = "success",
      collapsible = TRUE,
      collapsed = TRUE,
      downloadButton("download_data", "Download Processed Data",
                     class = "btn-success",
                     style = "width:100%; margin-bottom:10px;"),
      downloadButton("download_report", "Generate Report",
                     class = "btn-info",
                     style = "width:100%;")
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Value boxes
          valueBoxOutput("avg_power_box", width = 3),
          valueBoxOutput("avg_efficiency_box", width = 3),
          valueBoxOutput("total_energy_box", width = 3),
          valueBoxOutput("data_points_box", width = 3)
        ),
        
        fluidRow(
          valueBoxOutput("avg_wind_speed_box", width = 3),
          valueBoxOutput("availability_box", width = 3),
          valueBoxOutput("max_power_box", width = 3),
          valueBoxOutput("optimal_wind_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Power Curve Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("power_curve_plot", height = "500px") %>% 
              withSpinner(color = "#0dc5c1")
          )
        ),
        
        fluidRow(
          box(
            title = "Performance Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("performance_histogram", height = "300px")
          ),
          box(
            title = "Wind Direction Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("wind_direction_plot", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Time Series Performance",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("timeseries_plot", height = "300px")
          )
        )
      ),
      
      # Power Curve Analysis tab
      tabItem(
        tabName = "powercurve",
        fluidRow(
          box(
            title = "Measured vs Theoretical Power",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("scatter_comparison", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Power Curve with Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("detailed_power_curve", height = "400px")
          ),
          
          box(
            title = "Bin Statistics",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            DTOutput("bin_summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Power Curve Deviations",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("deviation_plot", height = "400px")
          )
        )
      ),
      
      # Data Quality tab
      tabItem(
        tabName = "dataquality",
        fluidRow(
          box(
            title = "Data Distribution Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("data_distribution", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Missing Data Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("missing_data_plot", height = "300px")
          ),
          
          box(
            title = "Outlier Detection",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("outlier_plot", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Quality Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("quality_metrics_table")
          )
        )
      ),
      
      # Advanced Analysis tab
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Wind Speed vs Direction Matrix",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("wind_matrix", height = "400px")
          ),
          
          box(
            title = "Analysis Parameters",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            radioButtons("analysis_type", "Analysis Type:",
                         choices = c("Density" = "density",
                                     "Contour" = "contour",
                                     "Heatmap" = "heatmap"),
                         selected = "heatmap"),
            sliderInput("matrix_bins", "Number of Bins:",
                        min = 10, max = 50, value = 20, step = 5)
          )
        ),
        
        fluidRow(
          box(
            title = "Hourly Performance Patterns",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hourly_patterns", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Statistical Summary",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("statistical_summary")
          )
        )
      ),
      
      # Raw Data tab
      tabItem(
        tabName = "rawdata",
        box(
          title = "Raw Data Viewer",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("raw_data_table") %>% 
            withSpinner(color = "#0dc5c1")
        ),
        
        box(
          title = "Data Summary",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput("data_summary")
        ),
        
        fluidRow(
          box(
            title = "Data Debug Info",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            verbatimTextOutput("data_debug")
          )
        )
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  
  # Reactive values for storing data
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    binned_data = NULL,
    analysis_results = NULL,
    analysis_triggered = FALSE
  )
  
  # Load data when file is uploaded
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Reset previous data
    rv$raw_data <- NULL
    rv$processed_data <- NULL
    rv$binned_data <- NULL
    rv$analysis_results <- NULL
    rv$analysis_triggered <- FALSE  # Reset analysis flag
    
    tryCatch({
      # Show loading notification
      showNotification(
        "Loading data... Please wait.",
        type = "default",
        duration = NULL,
        id = "loading_msg"
      )
      
      # Read CSV with specific settings for your data
      df <- read.csv(
        input$data_file$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8"
      )
      
      # Remove loading notification
      removeNotification(id = "loading_msg")
      
      # Store raw data
      rv$raw_data <- df
      
      showNotification(
        paste("Loaded", nrow(df), "rows with", ncol(df), "columns"),
        type = "default",
        duration = 5
      )
      
      # Process data
      showNotification(
        "Processing data...",
        type = "default",
        duration = NULL,
        id = "processing_msg"
      )
      
      # Use the preprocessing function
      processed <- tryCatch({
        preprocess_turbine_data(df)
      }, error = function(e) {
        showNotification(
          paste("Error in preprocessing:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
      
      # Remove processing notification
      removeNotification(id = "processing_msg")
      
      if(is.null(processed) || nrow(processed) == 0) {
        showNotification(
          "Data processing resulted in empty dataset",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      rv$processed_data <- processed
      
      # Update date range
      if("timestamp" %in% colnames(processed)) {
        dates <- as.Date(processed$timestamp)
        updateDateRangeInput(
          session,
          "date_range",
          start = min(dates, na.rm = TRUE),
          end = max(dates, na.rm = TRUE)
        )
      }
      
      # Auto-run analysis immediately after data is processed
      rv$analysis_triggered <- TRUE
      auto_run_analysis()
      
      showNotification(
        paste("Successfully processed", nrow(processed), "rows of data"),
        type = "default",
        duration = 5
      )
      
    }, error = function(e) {
      removeNotification(id = "loading_msg")
      removeNotification(id = "processing_msg")
      
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      
      cat("ERROR in data loading:\n")
      cat("Message:", e$message, "\n")
    })
  })
  
  # Function to auto-run analysis
  auto_run_analysis <- function() {
    req(rv$processed_data)
    
    showNotification(
      "Running initial analysis...",
      type = "default",
      duration = NULL,
      id = "auto_analysis_msg"
    )
    
    tryCatch({
      # Use default or current UI parameters for initial analysis
      bin_width <- if(!is.null(input$bin_width)) input$bin_width else 0.5
      min_points <- if(!is.null(input$min_data_points)) input$min_data_points else 10
      
      # Create power curve bins
      rv$binned_data <- create_power_curve_bins(
        df = rv$processed_data,
        bin_width = bin_width,
        min_points = min_points,
        performance_metric = input$performance_metric
      )
      
      # Calculate performance metrics
      rv$analysis_results <- calculate_performance_metrics(
        df = rv$processed_data,
        binned_data = rv$binned_data,
        performance_metric = input$performance_metric
      )
      
      removeNotification(id = "auto_analysis_msg")
      showNotification(
        "Initial analysis complete!",
        type = "default",
        duration = 3
      )
      
    }, error = function(e) {
      removeNotification(id = "auto_analysis_msg")
      showNotification(
        paste("Initial analysis error:", e$message),
        type = "warning",
        duration = 5
      )
    })
  }
  
  # Manual analysis when Analyze button is clicked
  observeEvent(input$analyze, {
    req(rv$processed_data)
    
    showNotification(
      "Running analysis with current filters...",
      type = "default",
      duration = NULL,
      id = "analysis_msg"
    )
    
    tryCatch({
      # Apply filters based on UI inputs
      filtered_data <- rv$processed_data
      
      # Filter by date range
      if("timestamp" %in% colnames(filtered_data)) {
        filtered_data <- filtered_data %>%
          filter(
            timestamp >= as.POSIXct(input$date_range[1]),
            timestamp <= as.POSIXct(input$date_range[2])
          )
      }
      
      # Filter by wind speed
      if("wind_speed_ms" %in% colnames(filtered_data)) {
        filtered_data <- filtered_data %>%
          filter(
            wind_speed_ms >= input$wind_speed_range[1],
            wind_speed_ms <= input$wind_speed_range[2]
          )
      }
      
      # Filter by power range
      if("active_power_kw" %in% colnames(filtered_data)) {
        filtered_data <- filtered_data %>%
          filter(
            active_power_kw >= input$power_range[1],
            active_power_kw <= input$power_range[2]
          )
      }
      
      cat("Filtered data rows:", nrow(filtered_data), "\n")
      
      # Create power curve bins with current parameters
      if(nrow(filtered_data) > 0) {
        rv$binned_data <- create_power_curve_bins(
          df = filtered_data,
          bin_width = input$bin_width,
          min_points = input$min_data_points,
          performance_metric = input$performance_metric
        )
        
        # Calculate performance metrics
        rv$analysis_results <- calculate_performance_metrics(
          df = filtered_data,
          binned_data = rv$binned_data,
          performance_metric = input$performance_metric
        )
        
        removeNotification(id = "analysis_msg")
        showNotification(
          paste("Analysis complete! Created", nrow(rv$binned_data), "bins."),
          type = "default",
          duration = 5
        )
      } else {
        removeNotification(id = "analysis_msg")
        showNotification(
          "No data available after filtering. Please adjust your filters.",
          type = "warning",
          duration = 5
        )
      }
      
    }, error = function(e) {
      removeNotification(id = "analysis_msg")
      showNotification(
        paste("Error in analysis:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Add reactive expression to check if data is ready for Power Curve tab
  data_for_power_curve <- reactive({
    list(
      binned_data = rv$binned_data,
      processed_data = rv$processed_data,
      analysis_triggered = rv$analysis_triggered
    )
  })
  
  # New
  output$processing_info <- renderPrint({
    req(rv$processed_data)
    
    cat("Processed Data Info:\n")
    cat("Rows:", nrow(rv$processed_data), "\n")
    cat("Columns:", ncol(rv$processed_data), "\n")
    cat("\nColumn names:\n")
    print(colnames(rv$processed_data))
    cat("\nData types:\n")
    print(sapply(rv$processed_data, class))
    cat("\nFirst few rows:\n")
    print(head(rv$processed_data))
  })
  
  output$column_check <- renderDT({
    req(rv$raw_data)
    
    # Create a summary of columns
    col_summary <- data.frame(
      Column_Name = colnames(rv$raw_data),
      Data_Type = sapply(rv$raw_data, class),
      Non_NA_Count = sapply(rv$raw_data, function(x) sum(!is.na(x))),
      NA_Count = sapply(rv$raw_data, function(x) sum(is.na(x))),
      Unique_Values = sapply(rv$raw_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(
      col_summary,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Value boxes
  output$avg_power_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Average Power", icon = icon("bolt"), color = "blue"))
    }
    
    # # Determine label and value based on performance metric
    # metric_label = switch(
    #   rv$analysis_results$performance_metric,
    #   'ratio' = 'Average Power Output',
    #   'diff' = 'Avg Power Difference',
    #   'percent' = 'Avg % Difference'
    # )
    # 
    # metric_value = rv$analysis_result$avg_power
    # 
    # # Format the value
    # if(rv$analysis_results$performance_metric == 'ratio') {
    #   display_value = paste0(round(metric_value))
    # }
    
    valueBox(
      value = paste0(round(rv$analysis_results$avg_power, 1), " kW"),
      subtitle = "Average Power Output",
      icon = icon("bolt"),
      color = "blue"
    )
  })
  
  output$avg_efficiency_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Average Performance", icon = icon("percentage"), color = "green"))
    }
    
    # Determine label and value based on performance metric
    metric_label <- switch(
      rv$analysis_results$performance_metric,
      "ratio" = "Average Efficiency",
      "diff" = "Avg Power Difference",
      "percent" = "Avg % Difference"
    )
    
    metric_value <- rv$analysis_results$avg_efficiency
    
    # Format the value appropriately
    if(rv$analysis_results$performance_metric == "ratio") {
      display_value <- paste0(round(metric_value * 100, 1), "%")
    } else if(rv$analysis_results$performance_metric == "diff") {
      display_value <- paste0(round(metric_value, 1), " kW")
    } else { # percent
      display_value <- paste0(round(metric_value, 1), "%")
    }
    
    valueBox(
      value = display_value,
      subtitle = metric_label,
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$total_energy_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Total Energy", icon = icon("charging-station"), color = "yellow"))
    }
    
    valueBox(
      value = paste0(round(rv$analysis_results$total_energy, 1), " kWh"),
      subtitle = "Total Energy Production",
      icon = icon("charging-station"),
      color = "yellow"
    )
  })
  
  output$data_points_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Data Points", icon = icon("database"), color = "purple"))
    }
    
    valueBox(
      value = format(rv$analysis_results$data_points, big.mark = ","),
      subtitle = "Data Points Analyzed",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$avg_wind_speed_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Avg Wind Speed", icon = icon("wind"), color = "teal"))
    }
    
    valueBox(
      value = paste0(round(rv$analysis_results$avg_wind_speed, 2), " m/s"),
      subtitle = "Average Wind Speed",
      icon = icon("wind"),
      color = "teal"
    )
  })
  
  output$availability_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Availability", icon = icon("check-circle"), color = "red"))
    }
    
    valueBox(
      value = paste0(round(rv$analysis_results$availability * 100, 1), "%"),
      subtitle = "Turbine Availability",
      icon = icon("check-circle"),
      color = "red"
    )
  })
  
  output$max_power_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Max Power", icon = icon("arrow-up"), color = "orange"))
    }
    
    valueBox(
      value = paste0(round(rv$analysis_results$max_power_output, 1), " kW"),
      subtitle = "Maximum Power Output",
      icon = icon("arrow-up"),
      color = "orange"
    )
  })
  
  output$optimal_wind_box <- renderValueBox({
    if (is.null(rv$analysis_results)) {
      return(valueBox("--", "Optimal Wind", icon = icon("tachometer-alt"), color = "light-blue"))
    }
    
    valueBox(
      value = paste0(round(rv$analysis_results$optimal_wind_speed, 1), " m/s"),
      subtitle = "Optimal Wind Speed",
      icon = icon("tachometer-alt"),
      color = "light-blue"
    )
  })
  
  # Plots
  output$power_curve_plot <- renderPlotly({
    req(rv$binned_data)  # Only render when binned_data exists
    
    tryCatch({
      plot_power_curve_analysis(
        binned_data = rv$binned_data,
        raw_data = rv$processed_data
      )
    }, error = function(e) {
      # Show error message in plot
      plotly_empty(type = "scatter") %>%
        layout(
          title = paste("Error in power curve analysis:", e$message),
          annotations = list(
            text = "Please check your data and try again",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    })
  })
  
  output$performance_histogram <- renderPlotly({
    req(rv$processed_data)
    plot_performance_histogram(rv$processed_data)
  })
  
  output$wind_direction_plot <- renderPlotly({
    req(rv$processed_data)  # Only render when processed_data exists
    
    tryCatch({
      plot_wind_direction_analysis(rv$processed_data)
    }, error = function(e) {
      # Show error message in plot
      plotly_empty(type = "scatter") %>%
        layout(
          title = paste("Error in wind direction analysis:", e$message),
          annotations = list(
            text = "Wind direction data may be missing or invalid",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    })
  })
  
  output$timeseries_plot <- renderPlotly({
    req(rv$processed_data)
    plot_timeseries_analysis(rv$processed_data)
  })
  
  output$scatter_comparison <- renderPlotly({
    # Check if processed data exists
    if(is.null(rv$processed_data) || nrow(rv$processed_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "Please upload a data file first",
                   xref = "paper",
                   yref = "paper",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE
                 )
               ))
    }
    
    # Debug: Print column names to console
    cat("Columns in processed data:", paste(colnames(rv$processed_data), collapse = ", "), "\n")
    
    # Try to create the plot
    tryCatch({
      plot_scatter_comparison(rv$processed_data)
    }, error = function(e) {
      cat("Error in scatter_comparison:", e$message, "\n")
      plotly_empty(type = "scatter") %>%
        layout(
          title = paste("Error:", e$message),
          annotations = list(
            text = paste("Error details:", e$message, "\n\nPlease check if theoretical power data exists in your CSV file."),
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 10)
          )
        )
    })
  })
  
  output$detailed_power_curve <- renderPlotly({
    data <- data_for_power_curve()
    
    if(is.null(data$binned_data) || !data$analysis_triggered) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No analysis results",
                 annotations = list(
                   text = "Please run analysis first or wait for auto-analysis",
                   xref = "paper",
                   yref = "paper",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE
                 )
               ))
    }
    
    tryCatch({
      plot_detailed_power_curve(data$binned_data)
    }, error = function(e) {
      plotly_empty(type = "scatter") %>%
        layout(
          title = paste("Error:", e$message),
          annotations = list(
            text = "Power curve data may be incomplete",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    })
  })
  
  output$deviation_plot <- renderPlotly({
    data <- data_for_power_curve()
    
    if(is.null(data$binned_data) || !data$analysis_triggered) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No analysis results",
                 annotations = list(
                   text = "Please run analysis first or wait for auto-analysis",
                   xref = "paper",
                   yref = "paper",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE
                 )
               ))
    }
    
    tryCatch({
      plot_deviation_analysis(data$binned_data, input$performance_metric)
    }, error = function(e) {
      plotly_empty(type = "scatter") %>%
        layout(
          title = paste("Error:", e$message),
          annotations = list(
            text = "Theoretical power data required for deviation analysis",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    })
  })
  
  output$data_distribution <- renderPlotly({
    req(rv$processed_data)
    plot_data_distribution(rv$processed_data)
  })
  
  output$missing_data_plot <- renderPlotly({
    req(rv$raw_data)
    plot_missing_data(rv$raw_data)
  })
  
  output$outlier_plot <- renderPlotly({
    req(rv$processed_data)
    plot_outlier_analysis(rv$processed_data)
  })
  
  output$wind_matrix <- renderPlotly({
    req(rv$processed_data)
    
    # Get the analysis parameters from UI inputs
    analysis_type <- input$analysis_type
    matrix_bins <- input$matrix_bins
    
    # Call the function with parameters
    plot_wind_matrix(
      df = rv$processed_data,
      analysis_type = analysis_type,
      matrix_bins = matrix_bins
    )
  })
  
  output$hourly_patterns <- renderPlotly({
    req(rv$processed_data)
    
    # Get the analysis parameters from UI inputs
    analysis_type <- input$analysis_type
    matrix_bins <- input$matrix_bins
    
    plot_hourly_patterns(rv$processed_data)
  })
  
  # Tables
  output$bin_summary_table <- renderDT({
    req(rv$binned_data)
    
    # Determine column name based on performance metric
    metric_name <- switch(
      input$performance_metric,
      "ratio" = "Efficiency Ratio",
      "diff" = "Power Diff (kW)",
      "percent" = "% Difference"
    )
    
    # Format the performance metric value
    format_metric <- function(value, metric_type) {
      if(metric_type == "ratio") {
        return(paste0(round(value, 3)))
      } else {
        return(round(value, 1))
      }
    }
    
    table_data <- rv$binned_data %>%
      select(
        `Wind Speed (m/s)` = wind_speed_bin,
        `Data Points` = n,
        `Avg Power (kW)` = avg_power,
        `Theoretical (kWh)` = avg_theoretical
      )
    
    # Add the performance metric column if it exists
    if("performance_metric" %in% colnames(rv$binned_data)) {
      table_data[[metric_name]] <- sapply(
        rv$binned_data$performance_metric,
        function(x) format_metric(x, input$performance_metric)
      )
    }
    
    # Add standard deviation
    table_data$`Std Dev` = round(rv$binned_data$std_dev, 1)
    
    table_data <- table_data %>%
      mutate(
        `Avg Power (kW)` = round(`Avg Power (kW)`, 1),
        `Theoretical (kWh)` = round(`Theoretical (kWh)`, 1)
      )
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  output$quality_metrics_table <- renderDT({
    req(rv$processed_data)
    
    quality_metrics <- data.frame(
      Metric = c("Total Records", "Complete Records", "Missing Values", 
                 "Average Wind Speed", "Average Power", "Data Coverage"),
      Value = c(
        nrow(rv$processed_data),
        sum(complete.cases(rv$processed_data)),
        sum(is.na(rv$processed_data)),
        paste0(round(mean(rv$processed_data$wind_speed_ms, na.rm = TRUE), 2), " m/s"),
        paste0(round(mean(rv$processed_data$active_power_kw, na.rm = TRUE), 1), " kW"),
        paste0(round(sum(complete.cases(rv$processed_data)) / nrow(rv$processed_data) * 100, 1), "%")
      )
    )
    
    datatable(
      quality_metrics,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  output$raw_data_table <- renderDT({
    req(rv$raw_data)
    
    datatable(
      rv$raw_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Text outputs
  output$statistical_summary <- renderPrint({
    req(rv$processed_data)
    
    cat("Statistical Summary\n")
    cat("===================\n\n")
    
    cat("Wind Speed Statistics (m/s):\n")
    print(summary(rv$processed_data$wind_speed_ms))
    cat("\n")
    
    cat("Active Power Statistics (kW):\n")
    print(summary(rv$processed_data$active_power_kw))
    cat("\n")
    
    cat("Performance Ratio Statistics:\n")
    print(summary(rv$processed_data$performance_ratio))
  })
  
  output$data_summary <- renderPrint({
    req(rv$raw_data)
    
    cat("Data Structure:\n")
    str(rv$raw_data)
    cat("\n\nColumn Names:\n")
    print(colnames(rv$raw_data))
    cat("\n\nFirst Few Rows:\n")
    print(head(rv$raw_data))
  })
  
  output$data_debug <- renderPrint({
    req(rv$processed_data)
    
    cat("=== Data Debug Info ===\n\n")
    cat("Number of rows:", nrow(rv$processed_data), "\n")
    cat("Column names:", paste(colnames(rv$processed_data), collapse = ", "), "\n\n")
    
    cat("--- Required columns for scatter plot ---\n")
    cat("active_power_kw exists:", "active_power_kw" %in% colnames(rv$processed_data), "\n")
    cat("theoretical_power_kwh exists:", "theoretical_power_kwh" %in% colnames(rv$processed_data), "\n\n")
    
    if("theoretical_power_kwh" %in% colnames(rv$processed_data)) {
      cat("--- Theoretical Power Summary ---\n")
      cat("Non-NA values:", sum(!is.na(rv$processed_data$theoretical_power_kwh)), "\n")
      cat("NA values:", sum(is.na(rv$processed_data$theoretical_power_kwh)), "\n")
      cat("Zero values:", sum(rv$processed_data$theoretical_power_kwh == 0, na.rm = TRUE), "\n")
      cat("Range:", range(rv$processed_data$theoretical_power_kwh, na.rm = TRUE), "\n\n")
    }
    
    cat("--- First few rows with theoretical power ---\n")
    if("theoretical_power_kwh" %in% colnames(rv$processed_data)) {
      print(head(rv$processed_data[, c("timestamp", "active_power_kw", "theoretical_power_kwh")], 10))
    }
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("processed_turbine_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$processed_data, file, row.names = FALSE)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("turbine_analysis_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      showNotification(
        "Generating report... This may take a moment.",
        type = "default",
        duration = NULL,
        id = "report_msg"
      )
      
      # Get current data
      processed_data <- rv$processed_data
      binned_data <- rv$binned_data
      analysis_results <- rv$analysis_results
      
      if(is.null(processed_data)) {
        showNotification(
          "No data available to generate report.",
          type = "warning",
          duration = 5
        )
        removeNotification(id = "report_msg")
        return()
      }
      
      # Create a simple report
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Very simple Rmd content
      writeLines(c(
        "---",
        "title: 'Wind Turbine Performance Report'",
        "date: '`r Sys.Date()`'",
        "output: html_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        "",
        "# Wind Turbine Performance Report",
        "",
        "Report generated on `r Sys.time()`",
        "",
        "## Summary Statistics",
        "",
        "```{r}",
        "cat('Total data points analyzed:', nrow(processed_data), '\\n\\n')",
        "",
        "if('active_power_kw' %in% colnames(processed_data)) {",
        "  cat('**Active Power Statistics:**\\n')",
        "  print(summary(processed_data$active_power_kw))",
        "  cat('\\nAverage Power:', round(mean(processed_data$active_power_kw, na.rm = TRUE), 1), 'kW\\n')",
        "}",
        "",
        "if('wind_speed_ms' %in% colnames(processed_data)) {",
        "  cat('\\n**Wind Speed Statistics:**\\n')",
        "  print(summary(processed_data$wind_speed_ms))",
        "  cat('\\nAverage Wind Speed:', round(mean(processed_data$wind_speed_ms, na.rm = TRUE), 2), 'm/s\\n')",
        "}",
        "```",
        "",
        "## Power Curve",
        "",
        "```{r power-curve, fig.width=10, fig.height=6}",
        "if(!is.null(binned_data) && nrow(binned_data) > 0) {",
        "  library(ggplot2)",
        "  ggplot(binned_data, aes(x = wind_speed_bin, y = avg_power)) +",
        "    geom_line(color = 'blue', size = 1.5) +",
        "    labs(title = 'Power Curve Analysis',",
        "         x = 'Wind Speed (m/s)',",
        "         y = 'Average Power (kW)') +",
        "    theme_minimal()",
        "} else {",
        "  cat('No power curve data available.')",
        "}",
        "```"
      ), temp_rmd)
      
      tryCatch({
        # Create an environment with our data
        env <- new.env()
        env$processed_data <- processed_data
        env$binned_data <- binned_data
        
        rmarkdown::render(
          temp_rmd,
          output_file = file,
          envir = env
        )
        
        removeNotification(id = "report_msg")
        showNotification(
          "Report generated successfully!",
          type = "success",
          duration = 5
        )
        
      }, error = function(e) {
        removeNotification(id = "report_msg")
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  )
}



# Run the application
shinyApp(ui = ui, server = server)