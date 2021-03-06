rm(list = ls())
library(tidyverse)
library(svglite)
library(xlsx)

ctab <-
  read.csv("Data/cities_places_topics_v3.csv",sep=",",header=TRUE, encoding="UTF-8") %>%
  subset(select=-c(X)) %>%
  select(cities,countries,year,title,abstract,doi,authors,everything()) %>%
  rename(geo_city=cities,geo_country=countries,geo_pop=population)


############### remove proceedings ###############

ctab <- ctab %>%
  filter(!grepl("Conference",title)) %>%
  filter(!grepl("conference",title)) %>%
  filter(!grepl("Proceedings",title)) %>%
  filter(!grepl("proceedings",title)) %>% 
  filter(!grepl("proceedings",abstract))

############### duplicate rows where there is more than one city ###############


for (i in 1:length(ctab[,1])) {
  temp_cities <- as.character(ctab$geo_city[i])
  temp_countries <- as.character(ctab$geo_country[i])
  temp_pop <- as.character(ctab$geo_pop[i])
  if(grepl(';',temp_cities)=="TRUE") {
    temp_cities <- strsplit(temp_cities,';')
    temp_countries <- strsplit(temp_countries,';')
    temp_pop <- strsplit(temp_pop,';')
    for (j in 1:length(temp_cities[[1]])) {
      rowtobind <- ctab[i,]
      rowtobind$geo_city <- temp_cities[[1]][j]
      rowtobind$geo_country <- temp_countries[[1]][j]
      rowtobind$geo_pop <- temp_pop[[1]][j]
      ctab <- rbind(ctab,rowtobind)
    }
  }
}

ctab <- ctab[!(grepl(';',ctab$geo_city)),] %>%
  mutate(geo_city=trimws(geo_city))
ctab <- ctab[!(grepl(';',ctab$geo_country)),] %>%
  mutate(geo_country=trimws(geo_country))
ctab <- ctab[!(grepl(';',ctab$geo_pop)),] %>%
  mutate(geo_pop=trimws(geo_pop))


############### if a study is duplicated, divide the total citations among each duplication ###############

ctab <- ctab %>%
  group_by(title) %>%
  mutate(citations = citations/n())

############### Annoyances ###############


ctab <- ctab %>%
  mutate(geo_country=ifelse(grepl("Newcastle upon Tyne",abstract) & geo_city=="Newcastle","GBR",geo_country)) %>%
  mutate(geo_country=ifelse(grepl("Australia",abstract) & geo_city=="Newcastle","AUS",geo_country))

############### Attach topics names ###############

topic_names <- read.xlsx(file="Data/topic names update.xlsx",sheetIndex = "topic_table") %>%
  arrange(Stemmed.Keywords)

ctab <- setNames(ctab,c(names(ctab)[1:12],as.character(topic_names$Topic.Name)))


############### Attach regions ############### 


regions <- read.xlsx(file="C:\\Users\\lamw\\Google Drive\\Work\\Code\\MATLAB\\Data shop\\Region definitions\\regions.xls",sheetName = "Sheet1") %>%
  select(ISO.Code,IAM10,UN6)
regions$UN6 <- gsub("LATIN AMERICA AND THE CARIBBEAN","LATIN AMERICA",regions$UN6)
regions$UN6 <- gsub("NORTHERN AMERICA","NORTH AMERICA",regions$UN6)

ctab <- ctab %>%
  left_join(regions,by=c("geo_country"="ISO.Code")) %>%
  select(geo_city,geo_country,IAM10,UN6,geo_pop,citations,everything()) %>%
  mutate(geo_pop=as.numeric(geo_pop))



############### How many nan citations ###############

z<- ctab %>%
  filter(is.na(citations)==TRUE)



############### save and done ###############

ctop <- ctab %>%
     count(geo_city,sort=TRUE)

save(ctab,file="Data/city_studies.RData")


############### export for submission ###############
rm(list = ls())
library(tidyverse)
library(svglite)
library(xlsx)

load("Data/city_studies.RData")
ctab <- ctab %>% 
  select(-IAM10,-tags,-geo_pop)


write.xlsx(as.data.frame(ctab),file='Writing/cases.xlsx')

############### Summary figures ############### 

# # Rank cities by no. papers
# ctop <- ctab %>%
#   count(vars=cities,sort=TRUE)
# 
# # Plot top cities in descending order
# g_count <- ctop %>%
#   filter(n>5) %>%
#   ggplot(.,aes(x=reorder(vars,-n),y=n)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# # Set a topic threshold and count papers on each topic 
# csum <- tidyr::gather(ctab,topic,value,Active.travel:e.Vehicles)
# for (i in 1:length(csum[,1])) {
#   if (csum$value[i]>0.005) {
#     csum$value[i] <- 1
#   }
#   else {
#     csum$value[i] <- 0
#   }
# }
# 
# csum <- csum %>%
#   filter(value>0) %>%
#   group_by(cities,topic) %>% 
#   summarise_each(funs(sum),value)
# 
# z<-csum %>% 
#   group_by(cities) %>% 
#   summarise_each(funs(sum),value) %>% 
#   arrange(desc(value))
# 
# z <- rename(z,total=value)
# csum$city_ordered <- factor(csum$cities,levels=z$cities)
# 
# g_heat <- csum %>% 
#   left_join(ctop,by=c("cities" = "vars")) %>%
#   filter(n>9) %>%
#   group_by(cities,topic) %>% 
#   mutate(value = value/n) %>%
#   ggplot(., aes(city_ordered,topic)) +
#   geom_tile(aes(fill = value),colour = "white") +
#   scale_fill_gradient(low = "white",high = "steelblue") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# ##
# ggsave(file = "Plots/City_studies.pdf",plot = g_count)
# ggsave(file = "Plots/City_topics.pdf",plot = g_heat)
