rm(list = ls())
library(tidyverse)
library(svglite)
library(xlsx)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

############### PNAS/GEA data ############### 

data_GEA <- read.xlsx(file="Data/city PNAS data.xlsx",sheetName="gea_data",encoding="UTF-8") %>%
  select(cities,total_final_consumption_per_capita,country,population,gdp_per_cap,emission_intensity_2009) %>%
  rename(City=cities)
data_GEA <- data_GEA[!is.na(data_GEA$City),]


############### K Seto data ###############

data_Seto <- read.xlsx(file="Data/SI.2 - Moran Spatial Footprints Data Appendix.xlsx",sheetName = "S.2.3a - Top 500 Cities",
                   encoding="UTF-8") %>%
  rename(City="Urban.Cluster",co2pc="Footprint.cap..t.CO2.",co2="Footprint..t.CO2.",pop="Population") %>%
  select(City,Country,co2pc,co2,pop)


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
data_UN$City <- gsub("New York","New York City", data_UN$City)
data_UN$City <- gsub("Mexico, Ciudad De","Mexico City", data_UN$City)

data_UN <- data_UN %>%
  rename(UN_year=Source.Year,UN_type=City.type,UN_pop=Value)


############### OECD Income data ############### 

data_OECD <- read.xlsx('Data/OECD city income.xlsx',encoding="UTF-8",sheetName = "Sheet1") %>%
  rename(City="Metropolitan.areas",year="Year",unit="Unit",value="Value") %>%
  select(City,year,unit,value)

data_OECD$City <- gsub("Washington","Washington, D.C.", data_OECD$City)
data_OECD$City <- gsub("New York","New York City", data_OECD$City)

############### Aggregated Income data ###############

data_income <- data_GEA %>%
  rename(GEA_income="gdp_per_cap") %>%
  select(City,GEA_income)

data_income <- full_join(data_income,data_OECD) %>%
  rename(OECD_income="value") %>%
  select(-c(year,unit))

#fit <- lm(GEA_income~OECD_income,data = data_income)

# data_income %>%
#   ggplot(.,aes(x=GEA_income,y=OECD_income)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")
# 
# data_income %>%
#   ggplot(.,aes(x=GEA_income,y=OECD_income+fit$coefficients[1])) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")

data_income <- data_income %>%
  gather(gdp,value,GEA_income,OECD_income) %>%
  group_by(City) %>%
  filter(is.na(value)==FALSE) %>%
  filter(row_number()==1)


############### UN Urban Projection Data ###############

data_proj <- read.xlsx(file="Data/WUP2014-F17a-City_Size_Class.xls",sheetName="DATA",encoding="UTF-8",startRow=17,header=TRUE)

data_proj <- data_proj %>%
  rename(area='Major.area..region..country.or.area',size="Size.class.of.urban.settlement",size_code="Size.class.code",type="Type.of.data") %>%
  select(-c(Index,Country.Code))

names(data_proj) <- gsub("X","",names(data_proj))

data_proj <- data_proj %>%
  gather(year,value,"1950":"2030") %>%
  mutate(year=as.numeric(year)) %>%
  arrange(size_code)

levels <- unique(data_proj$size)
data_proj$size <- factor(data_proj$size, levels = levels)


############### GEONames Database ############### 

# data_geo <- read.xlsx(file="Data/cities15000.xlsx",sheetName="Sheet1",encoding="UTF-8",header=TRUE) 
# save(data_geo,file="Data/geo_data.RData")

rm(list = ls())
library(tidyverse)
library(xlsx)
load(file = "Data/geo_data.RData")
data_geo <- data_geo %>%
  rename(city=name..............,country=country.code,pop=population........) %>%
  select(city,country,pop) %>%
  filter(pop>15000)

regions <- read.xlsx(file="C:\\Users\\lamw\\Google Drive\\Work\\Code\\MATLAB\\Data shop\\Region definitions\\regions.xls",sheetName = "Sheet1") %>%
  select(ISO.Code,IAM10,UN6)

isos <- read.xlsx(file="C:\\Users\\lamw\\Google Drive\\Work\\Code\\MATLAB\\Handy code\\ISOcodes.xls",sheetName="3 letter codes",startRow = 3,colIndex=2:3,header=FALSE)
names(isos) = c("2 letter","3 letter")

data_geo <- left_join(data_geo,isos,by=c("country"="2 letter"))
data_geo <- left_join(data_geo,regions,by=c("3 letter"="ISO.Code")) %>%
  select(-country)

save(data_geo,file="Data/geo_data.RData")

############### save ###############

save(data_GEA,data_Seto,data_UN,data_OECD,data_income,data_proj,file="Data/city_data.RData") 

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

