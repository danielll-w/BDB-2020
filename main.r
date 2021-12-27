# NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# January 2021 (last updated December 2021)

# Main R-Runner File 

# Load packages
library(tidyverse)
library(na.tools)
library(ggimage)
library(plm)
library(foreign)
library(ggthemes)
library(magick)
library(fastDummies)
library(stringr)
library(broom)
library(magrittr)
library(Hmisc)
library(psych)
library(png)
library(jpeg)
library(grid)
library(ggforce)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
MainDir = getwd()

# Switches to load saved data or rerun the coverages and defender analysis table construction
run_coverage_id_code_sw <- 0
run_defender_analysis_variable_construction_sw <- 0

# Importing

# Import data supplied by the BDB competition (combine into one data frame with week variable)
source(paste(MainDir , "/Import/import_competition_data.r" , sep = ""))

# Analysis

# Perform coverage identification table (setup switch to load dataset if you wish; subset by week)
source(paste(MainDir , "/Analysis/identify_coverages.R" , sep = ""))


