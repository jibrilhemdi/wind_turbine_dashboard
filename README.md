# Turkey Wind Turbine Performance Dashboard

This Shiny dashboard provides a comprehensive analysis of wind turbine SCADA data, focusing on performance monitoring, power curve evaluation, and data quality assessment.

## Data Source

The dashboard is designed to work with the **Wind Turbine SCADA Dataset** from Kaggle:  
[https://www.kaggle.com/datasets/berkerisen/wind-turbine-scada-dataset](https://www.kaggle.com/datasets/berkerisen/wind-turbine-scada-dataset)

The dataset contains 10‑minute SCADA records including:
- Timestamp
- Active power (kW)
- Wind speed (m/s)
- Theoretical power curve (kWh)
- Wind direction (°)

## Features

- **Interactive Dashboard** – Key performance indicators (average power, total energy, efficiency, availability, etc.)
- **Power Curve Analysis** – Measured vs theoretical power curves with confidence intervals
- **Performance Distribution** – Histograms, wind direction polar plots, time series trends
- **Advanced Analysis** – Wind speed / direction matrix (heatmap, contour, 3D density), hourly patterns
- **Data Quality** – Missing data analysis, outlier detection, completeness metrics
- **Raw Data Viewer** – Sortable, searchable table with export options
- **Dynamic Filtering** – Date range, wind speed, and power output filters
- **Export** – Download processed data or generate an HTML report

## Requirements

The application requires R (≥4.0) with the following packages:

```r
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)
library(scales)
```

## Running the Application

1. Clone or download the project files (`app.R`, `data_processing.R`, `visualization.R`) into a folder.
2. Open `app.R` in RStudio (or your R environment).
3. Install any missing packages using `install.packages("package_name")`.
4. Run the app by clicking **Run App** in RStudio or executing:

   ```r
   shiny::runApp()
   ```

5. Upload the CSV file from the Kaggle dataset using the file input in the sidebar. The dashboard will automatically preprocess the data and display the analysis.

## File Structure

- `app.R` – Main Shiny application (UI and server logic).
- `data_processing.R` – Data cleaning, column mapping, binning, and metric calculations.
- `visualization.R` – Plotting functions for all charts (power curve, histograms, polar plots, matrices, etc.).

## Notes

- The app expects the exact column names from the Kaggle dataset (e.g., `LV ActivePower (kW)`, `Wind Speed (m/s)`, `Theoretical_Power_Curve (KWh)`). Automatic name mapping is included.
- For large datasets, plots may sample raw points for performance.
- Adjust bin width, minimum data points per bin, and performance metrics (ratio / difference / percent) in the sidebar.