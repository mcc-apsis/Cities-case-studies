rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)
library(xlsx)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

############### PNAS/GEA data ############### 

data_GEA <- read.xlsx(file="Data/city PNAS data.xlsx",sheetName="gea_data",encoding="UTF-8") %>%
  select(cities,total_final_consumption_per_capita,country,population,gdp_per_cap,emission_intensity_2009)
data_GEA <- data_GEA[!is.na(data_GEA$cities),]


############### K Seto data ###############

data_Seto <- read.xlsx(file="Data/SI.2 - Moran Spatial Footprints Data Appendix.xlsx",sheetName = "S.2.3a - Top 500 Cities",
                   encoding="UTF-8") %>%
  rename(cities="Urban.Cluster",co2pc="Footprint.cap..t.CO2.",co2="Footprint..t.CO2.",pop="Population") %>%
  select(cities,Country,co2pc,co2,pop)


############### UN City POP Data ############### 

data_UN <- read.csv('Data/UNdata_Export_20180124_111051139.csv',encoding="UTF-8") %>%
  subset(select=-c(Area,Sex)) %>%
  rename(Country = Country.or.Area)

# tidy up city names
data_UN$City <- gsub( " *\\(.*?\\) *", "", data_UN$City)
data_UN$City <- tolower(data_UN$City) %>%
  sapply(simpleCap)
data_UN$Year <- as.numeric(as.character(data_UN$Year))

# take only latest year
data_UN <- data_UN %>%
  group_by(City,City.type,Country) %>%
  filter(Year==max(Year)) %>%
  ungroup() %>%
  group_by(City,Country) %>%
  filter(Value==max(Value)) %>%
  filter(row_number()==n())

# Minor edits to city names
data_UN$City <- gsub("Washington","Washington, D.C.", data_UN$City)
data_UN$City <- gsub("Mexico, Ciudad De","Mexico City", data_UN$City)

############### OECD Income data ############### 

data_OECD <- read.xlsx('Data/OECD city income.xlsx',encoding="UTF-8",sheetName = "Sheet1") %>%
  rename(cities="Metropolitan.areas",year="Year",unit="Unit",value="Value") %>%
  select(cities,year,unit,value)

############### save ###############

save(data_GEA,data_Seto,data_UN,data_OECD,file="Data/city_data.RData")

############### Funny spatial data ############### 

# cdat <- read.xlsx(file = "Data/city data.xlsx",sheetName = "d_final", encoding="UTF-8") %>%
#   select(city,City.Short.Name..CDP.,country,Current.Population..CDP.,Median.GDP...BN..external.,Total.City.wide.Emissions..metric.tonnes.CO2e...CDP.) %>%
#   rename(city_short=City.Short.Name..CDP.,pop=Current.Population..CDP.,gdp=Median.GDP...BN..external.,co2=Total.City.wide.Emissions..metric.tonnes.CO2e...CDP.)
# 
# #plot distribution of pop
# ggplot(cdat,aes(x=reorder(city,-pop),y=pop)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# #plot distribution of gdp
# ggplot(cdat,aes(x=reorder(city,-gdp/pop),y=gdp/pop)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# #plot distribution of co2
# ggplot(cdat,aes(x=reorder(city,-co2/pop),y=co2/pop)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# save(cdat,file="Data/city_data_spatial.RData")

