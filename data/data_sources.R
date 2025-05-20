library(tidyverse)
library(readxl)
library(devtools)
library(vdemdata) # call vdem package 
library(ERT) # call ERT package

#calls packages 
source("packages.R")

# set working directory 
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")
##### SOURCES #####

#Vdem package from github 
vdem <- vdemdata::vdem %>% 
  filter(year >= 2000)
#write.csv(vdem, "data/misc/perp_vdem.csv")

#ERT package
ert <- read.csv("data/ert.csv") %>% 
  filter(year >= 2000)
#write.csv(ert, "data/misc/perp_ert.csv")

#spi csv from github 
url <- "https://raw.githubusercontent.com/worldbank/SPI/refs/heads/master/03_output_data/SPI_index.csv"
spi <- read_csv(url) 
#write.csv(spi, "data/misc/perp_spi.csv")

#sdg excel from directory 
sdg <- read_excel("data/SDR2024-data.xlsx", sheet = "Backdated SDG Index") %>% 
  filter(year >= 2000)
#write.csv(sdg, "data/misc/perp_sdg.csv")

#sci csv from directory 
sci_df <- read_csv("data/SCI_All_Dim_TS.csv")
#write.csv(sci_df, "data/misc/perp_sci_df.csv")

#GDP per capita, Up to date 
gdppc_df <- read_csv("data/gdppc_df_long.csv") %>% 
  filter(year >= 2000)
#write.csv(gdppc_df, "data/misc/perp_gdppc.csv")

#Information Capacity 
info_cap <- read_csv("data/information_capacity.csv") %>% 
  filter(year >= 2000)
#write.csv(info_cap, "data/misc/perp_info.csv")

#WB GNI Classifications 
gni_class <- read_csv("Output_CSVs/world_bank_income_classifications.csv") %>% 
  filter(year >= 2000)
#write.csv(gni_class, "data/misc/perp_gni_cl.csv")

#EIU Democracy Index 
di <- read_csv("data/democracy-index-eiu.csv") %>% 
  filter(year >= 2000)
#write.csv(di, "data/misc/perp_di.csv")

#GINI Coefficient - Income 
gini <- WDI(country = "all", indicator = "SI.POV.GINI", start = 2000, end = 2024)

#previously:
#read.csv("https://ourworldindata.org/grapher/economic-inequality-gini-index.csv?v=1&csvType=full&useColumnShortNames=true") 
#write.csv(info_cap, "data/misc/perp_gini.csv")

