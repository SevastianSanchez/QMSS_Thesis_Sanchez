library(tidyverse)
#library(car)
#library(tidyr)
#library(dplyr)
#library(readr) 
#library(readxl)
#library(broom)
library(devtools)
library(vdemdata) # call vdem package 
library(ERT) # call ERT package

#SOURCES 

#Vdem package from github 
vdem <- vdemdata::vdem  #loading data 

#ERT package
ert <- read.csv("data/ert.csv")

#spi csv from github 
url <- "https://raw.githubusercontent.com/worldbank/SPI/refs/heads/master/03_output_data/SPI_index.csv"
spi <- read_csv(url)

#sdg excel from directory 
sdg <- read_excel("data/SDR2024-data.xlsx", sheet = "Backdated SDG Index")

#sci csv from directory 
sci_df <- read_csv("data/SCI_All_Dim_TS.csv")

#GDP per capita, Up to date 
gdppc_df <- read_csv("data/gdppc_df_long.csv")

#Information Capacity 
info_cap <- read_csv("data/information_capacity.csv")

#ODIN 
