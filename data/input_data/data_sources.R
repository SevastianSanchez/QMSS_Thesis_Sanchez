library(tidyverse)
library(readxl)
library(devtools)
library(vdemdata) # call vdem package 
library(ERT) # call ERT package
library(WDI) # call WDI package for GINI coefficient

#calls packages 
source("packages.R")

# set working directory 
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")
##### SOURCES #####

#Vdem package from github [API]
vdem <- vdemdata::vdem %>% 
  filter(year >= 2000)
#write.csv(vdem, "data/misc/perp_vdem.csv")

#ERT package
ert <- read.csv("data/input_data/ert.csv") %>% 
  filter(year >= 2000)
#write.csv(ert, "data/misc/perp_ert.csv")

#spi csv from github [API]
url <- "https://raw.githubusercontent.com/worldbank/SPI/refs/heads/master/03_output_data/SPI_index.csv"
spi <- read_csv(url) 
#write.csv(spi, "data/misc/perp_spi.csv")

#sdg excel from directory 
sdg <- read_excel("data/input_data/SDR2024-data.xlsx", sheet = "Backdated SDG Index") %>% 
  filter(year >= 2000)
#write.csv(sdg, "data/misc/perp_sdg.csv")

#sci csv from directory 
sci_df <- read_csv("data/input_data/SCI_All_Dim_TS.csv")
#write.csv(sci_df, "data/misc/perp_sci_df.csv")

#GDP per capita
gdppc_df <- read_csv("data/input_data/gdppc_df_long.csv") %>% 
  filter(year >= 2000)
#write.csv(gdppc_df, "data/misc/perp_gdppc.csv")

#Information Capacity 
info_cap <- read_csv("data/input_data/information_capacity.csv") %>% 
  filter(year >= 2000)
#write.csv(info_cap, "data/misc/perp_info.csv")

#WB GNI Classifications 
gni_class <- read_csv("data/input_data/world_bank_income_classifications.csv") %>% 
  filter(year >= 2000)
#write.csv(gni_class, "data/misc/perp_gni_cl.csv")

#EIU Democracy Index 
di <- read_csv("data/input_data/democracy-index-eiu.csv") %>% 
  filter(year >= 2000)
#write.csv(di, "data/misc/perp_di.csv")

#GINI Coefficient - Income [API]
gini <- WDI(country = "all", indicator = "SI.POV.GINI", start = 2000, end = NULL)

#previously:
#read.csv("https://ourworldindata.org/grapher/economic-inequality-gini-index.csv?v=1&csvType=full&useColumnShortNames=true") 
#write.csv(gini, "data/misc/perp_gini.csv")

### OTHER INTERESTING/RELATED WDI VARIABLES ###
# Access to electricity
#WDIsearch(string='1.1_ACCESS.ELECTRICITY.TOT', field='indicator')

# % rural population
#WDIsearch(string='SP.RUR.TOTL.ZS', field='indicator')

# % urban population
#WDIsearch(string='SP.URB.TOTL.IN.ZS', field='indicator')

# total population 
#WDIsearch(string='SP.POP.TOTL', field='indicator')

# % population with secondary education
#WDIsearch(string='CC.SE.CAT3.ZS', field='indicator')

# % population with post-secondary education
#WDIsearch(string='CC.SE.CAT4.ZS', field='indicator')

# Research and development expenditure (% of GDP)
#WDIsearch(string='GB.XPD.RSDV.GD.ZS', field='indicator')

# GNI total (in USD)
#WDIsearch(string='C1.7', field='indicator')

# GNI per capita 
#WDIsearch(string='C1.8', field='indicator')

# GINI Coefficient
#WDIsearch(string='3.0.Gini', field='indicator')
# GINI Coefficient - Index?
#WDIsearch(string='SI.POV.GINI', field='indicator')

### Example code to pull any of the above indicators
# x <- WDI(country = "all", indicator = "var", start = 2000, end = NULL)

