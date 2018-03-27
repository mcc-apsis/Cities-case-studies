rm(list = ls())
library(tidyverse)
load(file = "Data/city_studies.RData")

all_docs <- read.csv("Data/Query 2724.csv") %>%
  rename(title=wosarticle__ti,abstract=wosarticle__ab,year=wosarticle__py,authors=docauthinst__AU,doi=wosarticle__di,type=wosarticle__dt,journal=wosarticle__so) %>%
  select(type,everything())


searchTerm <- ("scenario | 2020 | 2025 | 2030 | 2050 | future" )
searchTerm <- ("sprawl | congestion | dense" )
searchTerm <- ("meta-analysis | meta-survey | systematic review")


## Look for it
ctab$search <- ifelse(
  grepl(searchTerm,ctab$abstract) | 
    grepl(searchTerm,ctab$title),
  1,
  0
)

all_docs$search <- ifelse(
  grepl(searchTerm,all_docs$abstract) |
    grepl(searchTerm,all_docs$title),
  1,
  0
)


## Filter docs that match the search term
searchMatches_cases <- ctab %>%
  filter(search==1)

searchMatches_all <- all_docs %>%
  filter(search==1)


searchMatches <- searchMatches %>%
  group_by(UN6) %>%
  summarise(n=n())
