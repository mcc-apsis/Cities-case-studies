rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)

ctab <-
  read.csv("Data/cities_places_topics.csv",sep=",",header=TRUE) %>%
  subset(select=-c(X)) %>%
  select(cities,year,title,abstract,doi,authors,everything())

#constrain to at least 10 studies per city
ctop <-
  ctab %>%
  count(vars=cities) %>%
  arrange(desc(n)) %>%
  filter(n>9)

#plot studies by city
g_count <- ggplot(ctop,aes(x=reorder(vars,-n),y=n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#heatmap of cities and topics
cht <- data.frame()
for (i in 1:length(ctop$vars)){
  cht <- rbind(cht,ctab[ctab$cities==ctop$vars[i],])
}
#check for duplicates
cht <- cht[!duplicated(cht$UT),]
cht <- cht[,-c(2:7)]
#sum correlations
cht <- tidyr::gather(cht,topic,value,-cities)
for (i in 1:length(cht[,1])) {
  if (cht$value[i]>0.005) {
    cht$value[i] <- 1
  }
  else {
    cht$value[i] <- 0
  }
}
cht <- cht %>% group_by(cities,topic) %>% summarise_each(funs(sum),value)
z<-cht %>% group_by(cities) %>% summarise_each(funs(sum),value) %>% arrange(desc(value))
cht$city_ordered <- factor(cht$cities,levels=z$cities)

g_heat <- ggplot(data = cht, aes(city_ordered,topic)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##
ggsave(file = "Plots/City_studies.svg",plot = g_count)
ggsave(file = "Plots/City_topics.svg",plot = g_heat)
