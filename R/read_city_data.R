rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)
library(xlsx)

cdat <- read.xlsx(file = "Data/city data.xlsx",sheetName = "d_final", encoding="UTF-8") %>%
  select(city,City.Short.Name..CDP.,country,Current.Population..CDP.,Median.GDP...BN..external.,Total.City.wide.Emissions..metric.tonnes.CO2e...CDP.) %>%
  rename(city_short=City.Short.Name..CDP.,pop=Current.Population..CDP.,gdp=Median.GDP...BN..external.,co2=Total.City.wide.Emissions..metric.tonnes.CO2e...CDP.)

#plot distribution of pop
ggplot(cdat,aes(x=reorder(city,-pop),y=pop)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#plot distribution of gdp
ggplot(cdat,aes(x=reorder(city,-gdp/pop),y=gdp/pop)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#plot distribution of co2
ggplot(cdat,aes(x=reorder(city,-co2/pop),y=co2/pop)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

save(cdat,file="Data/city_data.RData")

##### how about the pnas data
cpnas <- read.xlsx(file="Data/city PNAS data.xlsx",sheetName="gea_data",encoding="UTF-8") %>%
  select(cities,total_final_consumption_per_capita,country,population,gdp_per_cap,emission_intensity_2009)
cpnas <- cpnas[!is.na(cpnas$cities),]
save(cpnas,file="Data/city_data_pnas.RData")