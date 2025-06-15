# This is the code used to analyse the data on breast cancer from Sarajevo cancer hospital
# The code includes data cleaning, statistical calculations, analysis and plotting
# 28/04/25
rm(list = ls())

# Required packages: "readxl", ggplot2
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)

################################################################
##################################################################
## Load the data
##################################################################

file_path <- "C:/Users/cm1om/Documents/GIT Projects/Breast_cancer_BiH/Named_dataset.xlsx"
data <- read_excel(file_path, skip=3)
# Find the position of the column "Palliative"
col_pos <- which(names(data) == "Palliative")
# Keep only columns up to "Palliative"
data <- data[, 1:col_pos]
head(data)
nrow(data)

# First source all functions
source("Functions.R")

##########################################
# check for missing variables and do descriptive for patients
################################################
source("data_clean.R")

##########################################
## Explore delay in diagnosis
#########################################
source("Delays_in_diagnosis.R")
source("Diagnostics_received.R")
