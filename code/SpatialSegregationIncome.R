#devtools::install_github('PhilChodrow/compx')

library(compx)
library(tidyverse)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(scales)
# https://philchodrow.github.io/compx/vignette_metric.html#introduction
# https://www.impots.gouv.fr/portail/statistiques

# change this for your own code
setwd("/Users/phil/projects/MultiScaleIncomeSeg")

##############
##  FRANCE  ##
##############

fr_raw = read.csv("data/France/Tax_and_income_by_bracket_france_2011.csv", sep=",", dec=".") %>% tbl_df()
fr_raw$GEOID = fr_raw$C_INSEE
all_communes = readOGR("data/France/COMMUNE.shp", layer = "COMMUNE")
all_communes@data$GEOID = paste0("C", all_communes@data$INSEE_COM)

all_communes@data = data.frame(all_communes@data, fr_raw[match(all_communes@data$GEOID, fr_raw$GEOID),])
# head(all_communes@data)
# all_communes@data$color = ifelse(all_communes@data$NBHH_B1 == "", "red", "blue")

fr = fr_raw[fr_raw$AUCode != "", c("GEOID", paste0("NBHH_B", 1:8), "AUCode")]
colnames(fr) = c("tract", paste0("I", 1:8), "ID_c")
fr$t = 2011

fr_2011 <- gather_(fr, "group", "n", paste0("I", 1:8))
fr_2011 <- fr_2011 %>%
  filter(t == 2011) %>%
  select(-t) %>%
	mutate(ID_c = as.character(ID_c))
head(fr_2011)

# --------
# Now we're doing all the normal stuff, but iterating over lists of data frames and
# polygons instead.

# modify this vector to change which cities are analyzed
commune_ids <- c("AU001", "AU002", "AU003", "AU004")

# ------
# Prepare the data

city_fr <- fr_2011 %>%
  filter(ID_c %in% commune_ids) %>%
	mutate(n = as.numeric(n))

communes_data <- city_fr %>%
  group_by(tract, group) %>%
  filter(sum(n) > 10) %>%
	ungroup() %>%
	complete(group, nesting(ID_c, tract), fill = list('n' = 0)) %>%
	mutate(ID_c = as.character(ID_c),
		   tract = as.character(tract))

communes <- all_communes[all_communes@data$GEOID %in% communes_data$tract, ]

id_lookup <- communes_data %>%
	select(tract, ID_c) %>%
	distinct(tract, .keep_all = T)

communes@data <- communes@data %>%
	left_join(id_lookup, by = c('GEOID' = 'tract'))

# -----
# Define iterator lists

# holds a list of data frames with data
data_list <- communes_data %>%
	split(.$ID_c) %>%
	map(~select(., -ID_c))

# holds a list of spdfs
communes_list <- commune_ids %>%
	purrr::map(~ communes[communes@data$ID_c == .,])

# Compute the metric df for each city and rbind them together.
metric_df <- map2(communes_list,
				  data_list,
				  compute_metric,
				  km = T,
				  sigma = .1,
				  hessian = euc_,
				  smooth = T) %>%
	reduce(rbind)

# compute the local volume element
metric_df <- metric_df %>%
	mutate(vol = map_dbl(g, . %>% det %>% abs %>% sqrt))

# fortify the polygons and rbind them
f_communes <- communes_list %>%
	map(fortify, region = 'GEOID') %>%
	map2(commune_ids, ~ mutate(.x, ID_c = .y)) %>%
	reduce(rbind)

# plot!
f_communes %>%
	left_join(metric_df, by = c('id' = 'geoid')) %>%
	tbl_df() %>%
	ggplot() +
	aes(x = long, y = lat, fill = vol, group = group) +
	geom_polygon() +
	viridis::scale_fill_viridis(option = 'magma', trans = 'log10', oob = squish) +
	ggthemes::theme_map() +
	facet_wrap(~ID_c, scales = 'free')
