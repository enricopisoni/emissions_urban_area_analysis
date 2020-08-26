rm(list=ls())
library(sf)
library(tidyverse)
library(readxl)
library(tmap)
library(viridis)
library(RColorBrewer)

#load shp file
shp <- st_read('./countries/countries/gadm2_cntry.shp')
shp <- sf::st_simplify(shp, preserveTopology = TRUE, dTolerance = .01)

#load share per country and create dataframe
pat1 <- rep('numeric', 4)
pat2 <- rep('skip', 2)
pattern <- c('text', pat1, pat2, pat1, pat2,pat1, pat2,pat1, pat2,pat1, pat2,pat1)

########################################################################################################################
#load and process the year 2015
shares_by_cou_2015 <- read_excel('./Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/for maps shares by country.xlsx', sheet = '2015 shares', col_types = pattern)
shp_with_shares_2015 <- left_join(shp, shares_by_cou_2015, by = c("ISO" = "Country_code_A3") )

#create maps with 2015 shares
fig_name_vector <- names(shp_with_shares_2015)[2:(ncol(shp_with_shares_2015)-1)]
for (fig_name in fig_name_vector) {
  ggplot() +
    geom_sf(data = shp_with_shares_2015, aes_string(fill = fig_name), colour='white') +
    scale_fill_viridis(option="inferno", limits = c(0, 1)) +
    theme_minimal() 
  nf <- paste0('./output_step1/maps_', fig_name, '.png')
  ggsave(nf, width=14)
}
########################################################################################################################
#same for 1970
shares_by_cou_1970 <- read_excel('./Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/for maps shares by country.xlsx', sheet = '1970 shares', col_types = pattern)
shp_with_shares_1970 <- left_join(shp, shares_by_cou_1970, by = c("ISO" = "Country_code_A3") )

#create maps with 1970 shares
fig_name_vector <- names(shp_with_shares_1970)[2:(ncol(shp_with_shares_1970)-1)]
for (fig_name in fig_name_vector) {
  ggplot() +
    geom_sf(data = shp_with_shares_1970, aes_string(fill = fig_name), colour='white') +
    scale_fill_viridis(option="inferno", limits = c(0, 1)) +
    theme_minimal() 
  nf <- paste0('./output_step1/maps_', fig_name, '.png')
  ggsave(nf, width=14)
  }
########################################################################################################################
#change from 1970 to 2015: in particular: (2015-1970)/1970*100
shares_joined <- left_join(shares_by_cou_1970, shares_by_cou_2015, by = "Country_code_A3")
shares_joined_1970 <- select(shares_joined, ends_with("1970")) 
shares_joined_2015 <- select(shares_joined, ends_with("2015")) 
shares_deltas <- (shares_joined_2015 - shares_joined_1970)
names(shares_deltas) <- paste0(str_remove(names(shares_deltas),'2015'), 'deltas')
shares_deltas$Country_code_A3 <- shares_joined$Country_code_A3
shp_with_shares_deltas <- left_join(shp, shares_deltas, by = c("ISO" = "Country_code_A3") )

fig_name_vector <- names(shp_with_shares_deltas)[2:(ncol(shp_with_shares_deltas)-1)]
for (fig_name in fig_name_vector) {
  ggplot() +
    geom_sf(data = shp_with_shares_deltas, aes_string(fill = fig_name), colour='white') +
    scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red") +
    theme_minimal() 
  nf <- paste0('./output_step1/maps_', fig_name, '.png')
  ggsave(nf, width=14)
}

#ALL MAPS STILL USING CHANGING COLOR SCALE

#point 2:
#load stats per sector and create dataframe
#create barplots
#create heatmaps

#point 3:
#work on FUAs, extracting total per pollutant and per sector, for 1970 and 2015
