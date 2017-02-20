#devtools::install_github('PhilChodrow/compx')

library(compx)
library(tidyverse)  
library(maptools)   
library(RColorBrewer) 
library(rgdal)
library(rgeos)
# https://philchodrow.github.io/compx/vignette_metric.html#introduction
# https://www.impots.gouv.fr/portail/statistiques

setwd("~/Documents/MultiScale_Income_Seg")


##############
##  FRANCE  ##
##############

fr_raw = read.csv("data/Tax_and_income_by_bracket_france_2011.csv", sep=",", dec=".")
fr_raw$GEOID = fr_raw$C_INSEE
all_communes = readOGR("data/COMMUNE.shp", layer = "COMMUNE")
all_communes@data$GEOID = paste0("C", all_communes@data$INSEE_COM)

# all_communes@data = data.frame(all_communes@data, fr_raw[match(all_communes@data$GEOID, fr_raw$GEOID),])
# head(all_communes@data)
# all_communes@data$color = ifelse(all_communes@data$NBHH_B1 == "", "red", "blue")

fr = fr_raw[fr_raw$AUCode != "", c("GEOID", paste0("NBHH_B", 1:8), "AUCode")]
colnames(fr) = c("tract", paste0("I", 1:8), "ID_c")
fr$t = 2011

fr_2011 <- gather_(fr, "group", "n", paste0("I", 1:8))
fr_2011 <- fr_2011 %>%
  filter(t == 2011) %>%
  select(-t)  
head(fr_2011)

c = "AU001"
city_fr = fr_2011 %>%
  filter(ID_c == c) %>%
  select(-ID_c)  
head(city_fr)
city_fr$n = as.numeric(city_fr$n)
communes_to_use <- city_fr %>% 
  group_by(tract, group) %>%
  summarise(n = sum(n)) %>%
  filter(n > 10) 
head(communes_to_use)

communes <- all_communes[all_communes@data$GEOID %in% communes_to_use$tract, ]
head(communes@data)
plot(communes)
f_communes <- communes %>%         # for plotting later
  fortify(region = 'GEOID')


city_fr = city_fr[!is.na(city_fr$n),]
summary(city_fr)
metric_df <- compute_metric(communes,             # use these tracts
                            city_fr,          # and these data
                            km = T,             # convert spatial units to km
                            sigma = 100,        # for numerical derivatives
                            hessian = DKL_,     # use the KL divergence to compare distributions
                            smooth = T)         # spatial smoothing (avoids singularities)
