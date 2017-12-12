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
ctop <- left_join(ctop,cdat, by = c("vars" = "city")) %>%
  select(vars,n,pop,gdp,co2) %>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n)))

st_bt_pop <-
  ctop %>%
  filter(!is.na(pop)) %>%
  mutate(right="articles",left="pop") %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=pop,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(pop,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$pop,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(pop="#4292c6"),labels="Population") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles (normalised 10-147)")  +
  xlab("Cities, ordered by article count")   +
  ylab("Total Population (bottom-up urban data)")

st_bt_gdp <-
  ctop %>%
  filter(!is.na(gdp)) %>%
  mutate(right="articles",left="gdp") %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=gdp,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(gdp,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$gdp,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(gdp="#4292c6"),labels="GDP") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles (normalised 10-147)")  +
  xlab("Cities, ordered by article count")   +
  ylab("Total GDP (bottom-up urban data)")

st_bt_co2 <-
  ctop %>%
  filter(!is.na(co2)) %>%
  mutate(right="articles",left="co2") %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=co2,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(co2,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$co2,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(co2="#4292c6"),labels="CO2") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles (normalised 10-147)")  +
  xlab("Cities, ordered by article count")   +
  ylab("Total CO2 (bottom-up urban data)")

plot(st_bt_pop)
plot(st_bt_gdp)
plot(st_bt_co2)

ggsave(file = "Plots/cities_studies__bt_pop.pdf",plot = st_bt_pop)
ggsave(file = "Plots/cities_studies_bt_gdp.pdf",plot = st_bt_gdp)
ggsave(file = "Plots/cities_studies_bt_co2.pdf",plot = st_bt_co2)

###################

rm(cdat,ctop)
load(file = "Data/city_studies.RData")
load(file = "Data/city_data_pnas.RData")

#attempt a bind
ctop <- left_join(ctop,cpnas, by = c("vars" = "cities")) %>%
  select(vars,n,pop=population,gdp_pc=gdp_per_cap,co2_int=emission_intensity_2009)%>%
  mutate(n=scale(n,center=min(n),scale=max(n)-min(n))) %>%
  mutate(right="articles",left="pop")

# population
st_gea_pop <-
  ctop %>%
  filter(!is.na(pop)) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=pop,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(pop,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$pop,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(pop="#4292c6"),labels="Population") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles")  +
  xlab("Cities, ordered by article count")   +
  ylab("Total Population (GEA data)")

# gdp_pc
st_gea_gdppc <-
  ctop %>%
  filter(!is.na(gdp_pc)) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=gdp_pc,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(gdp_pc,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$gdp_pc,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(pop="#4292c6"),labels="GDP PC") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles (normalised 10-147)")  +
  xlab("Cities, ordered by article count")   +
  ylab("GDP per capita (GEA data)")

# co2_int
st_gea_co2int <-
  ctop %>%
  filter(!is.na(co2_int)) %>%
  ggplot(.) +
  geom_bar(aes(x=reorder(vars,-n),y=co2_int,fill=left),stat="identity") +
  geom_line(aes(x=reorder(vars,-n),y=n*max(co2_int,na.rm=TRUE),color=right),stat="identity",group=1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(ctop$co2_int,na.rm=TRUE))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        legend.position=c(1,1),legend.justification=c(1,1),legend.title=element_blank()) +
  scale_fill_manual(values=c(pop="#4292c6"),labels="CO2 Intensity") +
  scale_color_manual(values=c(articles="#e6550d"),labels="Articles (normalised 10-147)")  +
  xlab("Cities, ordered by article count")   +
  ylab("CO2 Intensity (GEA data)")

plot(st_gea_pop)
plot(st_gea_gdppc)
plot(st_gea_co2int)

ggsave(file = "Plots/cities_studies_gea_pop.pdf",plot = st_gea_pop)
ggsave(file = "Plots/cities_studies_gea_gdppc.pdf",plot = st_gea_gdppc)
ggsave(file = "Plots/cities_studies_gea_co2int.pdf",plot = st_gea_co2int)
