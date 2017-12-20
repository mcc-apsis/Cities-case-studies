rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)


load(file = "Data/city_studies.RData")
load(file = "Data/city_data_seto.RData")
gdpdat <- read.csv(file = "Data/gdp.csv",encoding="UTF-8")

#attempt a bind
ctop <- left_join(ctop,cseto, by = c("vars" = "cities"))
ctop <- left_join(ctop,gdpdat, by = c("Country" = "Country"))

ctop <- ctop %>%
  group_by(country_gdppc = cut(ctop$GDPpc,breaks = c(1005,3955,12235,1e6),include.lowest=TRUE)) %>% 
  summarise(no_cities=n(),sum_articles=sum(n),mean_articles=mean(n),city_names=paste(vars, collapse=",")) 

ctop %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(dr,-n),y=n),stat="identity") +
  scale_x_discrete(labels = comma) +
  xlab("Cities, grouped by country GDP") +
  ylab("Total number of articles")


############ GEA data (gdppc)


rm(ctop)
load(file = "Data/city_studies.RData")
load(file = "Data/city_data_pnas.RData")

ctop <- left_join(ctop,cpnas, by = c("vars" = "cities"))
ctop <- ctop %>%
  group_by(gdp_pc = cut(ctop$gdp_per_cap,breaks = c(1005,3955,12235,1e6),include.lowest=TRUE)) %>% 
  summarise(no_cities=n(),sum_articles=sum(n),mean_articles=mean(n),city_names=paste(vars, collapse=", ")) %>% 
  ungroup() %>% arrange(desc(gdp_pc))

ctop %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(gdp_pc,-no_cities),y=mean_articles),stat="identity") +
  scale_x_discrete(labels = c(">$12,235","$3,956-$12,235","$1,006-$3,955","NA")) +
  xlab("Cities, grouped by GDP per capita") +
  ylab("Average articles")
