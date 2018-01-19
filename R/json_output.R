rm(list = ls())
library(rjson)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
library(svglite)


json_file <- jsonlite::fromJSON(txt = "Map/1603.json",simplifyDataFrame = FALSE)

for (i in 1:length(json_file$geometries)) {
  
  handle <- grepl(json_file$geometries[[i]]$properties$name,cpnas$cities)
  
  if (any(handle)==TRUE) {
    json_file$geometries[[i]]$properties$GEA_pop = cpnas$population[handle]*1000
    json_file$geometries[[i]]$properties$GEA_gdppc = cpnas$gdp_per_cap[handle]
  }
  
  else {
    json_file$geometries[[i]]$properties$GEA_pop = NA
    json_file$geometries[[i]]$properties$GEA_gdppc = NA
  }
  
  handle <- grepl(json_file$geometries[[i]]$properties$name,cseto$cities)
  
  if (any(handle)==TRUE) {
    json_file$geometries[[i]]$properties$SETO_pop = cseto$pop[handle]
    json_file$geometries[[i]]$properties$SETO_co2pc = cseto$co2pc[handle]
  }
  
  else {
    json_file$geometries[[i]]$properties$SETO_pop = NA
    json_file$geometries[[i]]$properties$SETO_co2pc = NA
  }
}


#send back to json
json_file <- toJSON(json_file)
write(json_file,file="Map/1604.json")