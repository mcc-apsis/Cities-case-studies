rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)

load(file = "Data/city_studies.RData")
load(file = "Data/city_data.RData")

#attempt a bind
ctop <- left_join(ctop,cdat, by = c("vars" = "city"))

studies_vs_pop <- ctop %>%
  filter(n>5) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=pop),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(pop,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$pop,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city population (bar)") +
  xlab("Cities, ordered by article count")

studies_vs_gdp <- ctop %>%
  filter(n>5) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=gdp/pop),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(gdp/pop,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$gdp/ctop$pop,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city GDP/POP (bar)") +
  xlab("Cities, ordered by article count")

studies_vs_co2 <- ctop %>%
  filter(n>9) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=co2/pop),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(co2/pop,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$co2/ctop$pop,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city CO2/POP (bar)") +
  xlab("Cities, ordered by article count")

ggsave(file = "Plots/City_studies_pop.pdf",plot = studies_vs_pop)
ggsave(file = "Plots/City_studies_gdp.pdf",plot = studies_vs_gdp)
ggsave(file = "Plots/City_studies_co2.pdf",plot = studies_vs_co2)

###################

rm(cdat,ctop)
load(file = "Data/city_studies.RData")
load(file = "Data/city_data_pnas.RData")

#attempt a bind
ctop <- left_join(ctop,cpnas, by = c("vars" = "cities"))

#population
ctop %>%
  filter(n>2) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=population),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(population,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$population,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city population (bar)") +
  xlab("Cities, ordered by article count")

#gdp
ctop %>%
  filter(n>2) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=gdp_per_cap),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(gdp_per_cap,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$gdp_per_cap,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city gdp_per_cap (bar)") +
  xlab("Cities, ordered by article count")

#emission_intensity_2009
ctop %>%
  filter(n>2) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=emission_intensity_2009),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(emission_intensity_2009,na.rm=TRUE)),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$emission_intensity_2009,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Article count (line) vs. city gdp_per_cap (bar)") +
  xlab("Cities, ordered by article count")
