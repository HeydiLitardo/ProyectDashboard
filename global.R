# Load all R packages
library(shiny)
library(modules)
library(dplyr)
library(tidyr)
library(lubridate)
library(dygraphs)
library(sass)
library(glue)
library(ggplot2)
library(RColorBrewer)
library(xts)
library(echarts4r)
library(htmltools)
library(readxl)
library(readr)

# Constants
consts <- use("constants.R")

# Modules
metric_summary <- use("modules/metric_summary.R")
map_chart <- use("./modules/map_chart.R")
time_chart <- use("./modules/time_chart.R")
breakdown_chart <- use("./modules/breakdown_chart.R")
vertical_bar_chart <- use("./modules/vertical_bar_chart.R")