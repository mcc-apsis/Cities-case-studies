rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)
library(xlsx)

ctab <-
  read.csv("Data/cities_places_topics.csv",sep=",",header=TRUE, encoding="UTF-8") %>%
  subset(select=-c(X)) %>%
  select(cities,year,title,abstract,doi,authors,everything())


############### duplicate rows where there is more than one city ###############

for (i in 1:length(ctab[,1])) {
  temp <- as.character(ctab$cities[i])
  if(grepl(';',temp)=="TRUE") {
    temp_cities <- strsplit(temp,';')
    for (j in 1:length(temp_cities[[1]])) {
      rowtobind <- ctab[i,]
      rowtobind$cities <- temp_cities[[1]][j]
      ctab <- rbind(ctab,rowtobind)
    }
  }
}
ctab <- ctab[!(grepl(';',ctab$cities)),] %>%
  mutate(cities=trimws(cities))


############### Extract country names ############### 

# Get a list of country names and ISOs
isos <- read.xlsx(file="C:\\Users\\lamw\\Google Drive\\Work\\Code\\MATLAB\\Handy code\\ISOcodes.xls",
                  sheetName="3 letter codes",colIndex=7:8,startRow=3,header=FALSE,encoding="UTF-8") %>%
  rename(countries=X7,isos=X8) %>%
  filter(!countries %in% c("US","World","Jersey"),!isos %in% c("ZZZZ","ZZZA"))

ctab$country <-NA
for (i in 1:length(ctab$abstract)) {
  for (j in 1:length(isos$countries)) {
    if (grepl(isos$countries[j],ctab$abstract[i],ignore.case=TRUE)==TRUE)
      ctab$country[i] <- paste0(ctab$country[i],", ",isos$isos[j])
  }
}
ctab$country <- gsub("NA,","",ctab$country)
ctab <- ctab %>%
  select("cities","country",everything())

ctab$cities <- gsub("New York City","New York",ctab$cities)

############### Summary figures ############### 

# Rank cities by no. papers
ctop <- ctab %>%
  count(vars=cities) %>%
  arrange(desc(n))

# Plot top cities in descending order
g_count <- ctop %>%
  filter(n>5) %>%
  ggplot(.,aes(x=reorder(vars,-n),y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Set a topic threshold and count papers on each topic 
csum <- tidyr::gather(ctab,topic,value,Active.travel:e.Vehicles)
for (i in 1:length(csum[,1])) {
  if (csum$value[i]>0.005) {
    csum$value[i] <- 1
  }
  else {
    csum$value[i] <- 0
  }
}

csum <- csum %>%
  filter(value>0) %>%
  group_by(cities,topic) %>% 
  summarise_each(funs(sum),value)

z<-csum %>% 
  group_by(cities) %>% 
  summarise_each(funs(sum),value) %>% 
  arrange(desc(value))

z <- rename(z,total=value)
csum$city_ordered <- factor(csum$cities,levels=z$cities)

g_heat <- csum %>% 
  left_join(ctop,by=c("cities" = "vars")) %>%
  filter(n>9) %>%
  group_by(cities,topic) %>% 
  mutate(value = value/n) %>%
  ggplot(., aes(city_ordered,topic)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##
ggsave(file = "Plots/City_studies.pdf",plot = g_count)
ggsave(file = "Plots/City_topics.pdf",plot = g_heat)

save(ctab,ctop,file="Data/city_studies.RData")

