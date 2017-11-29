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
ctab <- ctab[,-c(2:7)]
#sum correlations
ctab <- tidyr::gather(ctab,topic,value,-cities)
for (i in 1:length(ctab[,1])) {
  if (ctab$value[i]>0.005) {
    ctab$value[i] <- 1
  }
  else {
    ctab$value[i] <- 0
  }
}

ctab <- ctab %>% group_by(cities,topic) %>% summarise_each(funs(sum),value)

z<-ctab %>% group_by(cities) %>% summarise_each(funs(sum),value) %>% arrange(desc(value))
z <- rename(z,total=value)

ctab$city_ordered <- factor(ctab$cities,levels=z$cities)
ctab <- ctab %>% left_join(ctop,by=c("cities" = "vars")) %>%
  filter(n>9)



## normalise
ctab <- ctab %>% group_by(cities,topic) %>% mutate(value = value/n)

g_heat <- ggplot(data = ctab, aes(city_ordered,topic)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##
ggsave(file = "Plots/City_studies.svg",plot = g_count)
ggsave(file = "Plots/City_topics.svg",plot = g_heat)

#rm(ctab,ctop,z)
