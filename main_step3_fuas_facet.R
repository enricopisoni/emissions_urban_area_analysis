#get fuas shp files
rm(list=ls())
library(sf)
library(tidyverse)
library(raster)
library(exactextractr)

#get crs
nf <- "./edgar_grids/v50_CO_1970.0.1x0.1/v50_CO_1970.0.1x0.1.nc"
r <- raster(nf)
fuas <- st_read('./fuas_shp/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg')
fuas <- st_transform(fuas, crs(r))
names(fuas)
fuas_subset <- filter(fuas, FUA_p_2015>1000000)
#fuas$FUA_p_2015
#fuas_subset$eFUA_name

#get edgar files, per co, co2, nh3, nox, PM10, so2, for 1970 and 2015
files <- list.files('./edgar_grids/')
res <- list()
for (ff in files) {
  nf <- paste0('./edgar_grids/', ff, '/', ff, '.nc')
  print(nf)
  r <- raster(nf)
  extent(r) <- c(-180,180,-90,90)
  ar <- area(r) # area in km2
  values(r) <- values(r)*31536000 #to have kg/m2 from kg/m2/s
  values(r) <- values(r)*1000000 #to get kg/km2
  values(r) <- values(r)/1000 #to get tons/km2
  r <- r*ar #to get tons
  
  df_extracted <- exact_extract(r, fuas_subset, 'sum')  
  
  poll <- str_split(ff, '_', simplify=TRUE)[[2]]
  print(poll)
  year <- str_sub(str_split(ff, '_', simplify=TRUE)[[3]],1,4)
  print(year)
  res[[ff]] <- data.frame('emissions'=df_extracted, 'city'=fuas_subset$eFUA_name, 'country'=fuas_subset$Cntry_name, 
                          'population'=fuas_subset$FUA_p_2015, 'area'=fuas_subset$FUA_area, 'pollutant'=poll,
                          'year'=year)
}

final_res <- bind_rows(res)
#save emissions in tons
write_csv(final_res, './Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/city_emissions.csv')
final_res <- final_res %>%
  mutate(emissionDensity=emissions/area)

polls <- as.vector(unique(final_res$pollutant))
for (pol in polls) {
  final_res %>%
    dplyr::filter(pollutant==pol) %>%
    dplyr::select(city,pollutant,year,emissionDensity) %>%
    tidyr::pivot_wider(names_from = year, values_from = emissionDensity,  values_fn = list(emissionDensity = mean)) %>%
    mutate(city = fct_reorder(city, `2015`)) %>%
    ggplot() +
    geom_segment( aes(x=city, xend=city, y=`1970`, yend=`2015`), color="grey") +
    geom_point( aes(x=city, y=`1970`, color='1970'), size=2) +
    geom_point( aes(x=city, y=`2015`, color='2015'), size=2) +
    scale_colour_manual(name='', values=c('1970'=rgb(0.2,0.7,0.1,0.5), '2015'=rgb(0.7,0.2,0.1,0.5))) +
    coord_flip() + 
    scale_y_log10() +
    xlab("") +
    ylab(paste0("Change of emission density per city, from 1970 to 2015, ", pol)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  nf <- paste0('./output_step3/lollipops_emidens_bycity_',pol,'.png')
  ggsave(nf, width=10, height=14)
}

polls <- as.vector(unique(final_res$pollutant))
for (pol in polls) {
  final_res %>%
    dplyr::filter(pollutant==pol) %>%
    dplyr::select(city,pollutant,year,emissions) %>%
    tidyr::pivot_wider(names_from = year, values_from = emissions,  values_fn = list(emissions = mean)) %>%
    mutate(city = fct_reorder(city, `2015`)) %>%
    ggplot() +
    geom_segment( aes(x=city, xend=city, y=`1970`, yend=`2015`), color="grey") +
    geom_point( aes(x=city, y=`1970`, color='1970'), size=2) +
    geom_point( aes(x=city, y=`2015`, color='2015'), size=2) +
    scale_colour_manual(name='', values=c('1970'=rgb(0.2,0.7,0.1,0.5), '2015'=rgb(0.7,0.2,0.1,0.5))) +
    coord_flip() + 
    scale_y_log10() +
    xlab("") +
    ylab(paste0("Change of emission per city, from 1970 to 2015, ", pol)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  nf <- paste0('./output_step3/lollipops_emi_bycity_',pol,'.png')
  ggsave(nf, width=10, height=14)
}



