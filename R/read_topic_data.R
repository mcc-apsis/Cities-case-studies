rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)

ctab <-
  read.csv("Data/cities_places_topics.csv",sep=",",header=TRUE, encoding="UTF-8") %>%
  subset(select=-c(X)) %>%
  select(cities,year,title,abstract,doi,authors,everything())

#duplicate rows where there is more than one city
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

#constrain to at least 10 studies per city
ctop <- ctab %>%
  count(vars=cities) %>%
  arrange(desc(n))

#plot studies by city
g_count <- ctop %>%
  filter(n>5) %>%
  ggplot(.,aes(x=reorder(vars,-n),y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#sum correlations
ctab <- ctab[,-c(2:7)]
ctab <- tidyr::gather(ctab,topic,value,-cities)
for (i in 1:length(ctab[,1])) {
  if (ctab$value[i]>0.005) {
    ctab$value[i] <- 1
  }
  else {
    ctab$value[i] <- 0
  }
}
ctab <- ctab %>%
  filter(value>0) %>%
  group_by(cities,topic) %>% 
  summarise_each(funs(sum),value)

z<-ctab %>% 
  group_by(cities) %>% 
  summarise_each(funs(sum),value) %>% 
  arrange(desc(value))

z <- rename(z,total=value)

ctab$city_ordered <- factor(ctab$cities,levels=z$cities)

g_heat <- ctab %>% 
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

save(ctop,file="Data/city_studies.RData")
save(ctab,file="Data/city_topics.RData")
