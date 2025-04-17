library(tidyverse)
library(readxl)
library(devtools)
library(vdemdata) # call vdem package 
library(ERT) # call ERT package

# set working directory 
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

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

#WB GNI Classifications 
gni_class <- read_csv("Output_CSVs /world_bank_income_classifications.csv")

#EIU Democracy Index 
di <- read_csv("data/democracy-index-eiu.csv")

#GINI Coefficient - Income 
gini <- read.csv("https://ourworldindata.org/grapher/economic-inequality-gini-index.csv?v=1&csvType=full&useColumnShortNames=true")

#ODIN

