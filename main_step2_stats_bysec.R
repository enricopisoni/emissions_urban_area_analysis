rm(list=ls())
library(sf)
library(tidyverse)
library(readxl)
library(tmap)
library(viridis)
library(RColorBrewer)

########################################################################################################################
#load and process the year 1970
polltoread <- c('CO2', 'NOx', 'PM10', 'CO', 'NH3', 'SO2')
tmplist <- list()
for (poll in polltoread) {
  tmpdf <- read_excel('./Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/1970_all_stats_by_Sector_MC.xlsx', sheet = poll)
  tmplist[[poll]] <- tmpdf %>%
    select(-c(...1)) %>%
    mutate(pollutant = poll, year = 1970) %>%
    pivot_longer(cols = starts_with("emi"), names_to = "geographicalAggregation", values_to = "emissionsValue")
}
final_df_1970 <- bind_rows(tmplist)

#read 2015
polltoread <- c('CO2', 'NOx', 'PM10', 'CO', 'NH3', 'SO2')
tmplist <- list()
for (poll in polltoread) {
  tmpdf <- read_excel('./Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/2015_all_stats_by_Sector_MC.xlsx', sheet = poll)
  tmplist[[poll]] <- tmpdf %>%
    select(-c(...1)) %>%
    select(-3) %>%
    mutate(pollutant = poll, year = 2015) %>%
    pivot_longer(cols = starts_with("emi"), names_to = "geographicalAggregation", values_to = "emissionsValue")
}
final_df_2015 <- bind_rows(tmplist)

#join 1970 and 2015
final_df <- rbind(final_df_1970, final_df_2015)

#load continent and region - join to final_df
cou_def <- read_excel('./Re__qualche_idea_per_paper_su_edgar_e_contributo_citta/2015_all_stats_by_Sector_MC.xlsx', sheet = 'country definition')
final_df_with_cou <- left_join(final_df, cou_def, by=c("Country_code_A3" = "ISO_A3"))

#barplot for geog areas
final_df_with_cou %>%
  filter(sector!='TOTAL') %>%
  group_by(continent, geographicalAggregation, pollutant, year) %>%
  summarize(emissions = sum(emissionsValue)) %>%
  mutate(year=as.character(year)) %>%
  drop_na %>%
  ggplot(aes(fill=geographicalAggregation, y=emissions, x=continent)) +
    geom_bar(position="stack", stat="identity") +
    coord_flip() +
    facet_grid(year ~ pollutant, scales='free') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_brewer(palette='RdYlBu')
ggsave('./output_step2/barplot_areas.png', width=14)

#barplot for sectors
final_df_with_cou %>%
  filter(sector!='TOTAL') %>%
  group_by(continent, sector, pollutant, year) %>%
  summarize(emissions = sum(emissionsValue)) %>%
  mutate(year=as.character(year)) %>%
  drop_na %>%
  ggplot(aes(fill=sector, y=emissions, x=continent)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  facet_grid(year ~ pollutant, scales='free') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette='BrBG')
ggsave('./output_step2/barplot_sectors.png', width=14)

#lollipop graph per sector
final_df_with_cou %>%
  #filter(sector!='TOTAL', geographicalAggregation=='emi_in_urban_centres') %>%
  filter(sector!='TOTAL') %>%
  group_by(continent, sector, pollutant, year) %>%
  summarize(emissions = sum(emissionsValue)) %>%
  mutate(year=as.character(year)) %>%
  drop_na %>%
  pivot_wider(names_from = year, values_from = emissions) %>%
  ggplot() +
  geom_segment( aes(x=sector, xend=sector, y=`1970`, yend=`2015`), color="grey") +
  geom_point( aes(x=sector, y=`1970`), color=rgb(0.2,0.7,0.1,0.5), size=1 ) +
  geom_point( aes(x=sector, y=`2015`), color=rgb(0.7,0.2,0.1,0.5), size=1 ) +
  coord_flip() +
  facet_grid(continent ~ pollutant, scales='free') +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Change of total emissions, per sector, from 1970 to 2015") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
ggsave('./output_step2/lollipops_bysector.png', width=10, height=8)

#lollipop graph per geog area
final_df_with_cou %>%
  filter(sector!='TOTAL') %>%
  group_by(continent, geographicalAggregation, pollutant, year) %>%
  summarize(emissions = sum(emissionsValue)) %>%
  mutate(year=as.character(year)) %>%
  drop_na %>%
  pivot_wider(names_from = year, values_from = emissions) %>%
  ggplot() +
  geom_segment( aes(x=geographicalAggregation, xend=geographicalAggregation, y=`1970`, yend=`2015`), color="grey") +
  geom_point( aes(x=geographicalAggregation, y=`1970`), color=rgb(0.2,0.7,0.1,0.5), size=1 ) +
  geom_point( aes(x=geographicalAggregation, y=`2015`), color=rgb(0.7,0.2,0.1,0.5), size=1 ) +
  coord_flip() +
  facet_grid(continent ~ pollutant, scales='free') +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Change of total emissions, per geographical area, from 1970 to 2015") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
ggsave('./output_step2/lollipops_bygeogarea.png', width=10, height=8)

#lollipop graph per sector, only urban areas
final_df_with_cou %>%
  filter(sector!='TOTAL', geographicalAggregation=='emi_in_urban_centres') %>%
  group_by(continent, sector, pollutant, year) %>%
  summarize(emissions = sum(emissionsValue)) %>%
  mutate(year=as.character(year)) %>%
  drop_na %>%
  pivot_wider(names_from = year, values_from = emissions) %>%
  ggplot() +
  geom_segment( aes(x=sector, xend=sector, y=`1970`, yend=`2015`), color="grey") +
  geom_point( aes(x=sector, y=`1970`), color=rgb(0.2,0.7,0.1,0.5), size=1 ) +
  geom_point( aes(x=sector, y=`2015`), color=rgb(0.7,0.2,0.1,0.5), size=1 ) +
  coord_flip() +
  facet_grid(continent ~ pollutant, scales='free') +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Change of total emissions, per sector (only urban centres), from 1970 to 2015") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
ggsave('./output_step2/lollipops_bysector_onlyUC.png', width=10, height=8)


#point 2:
#load stats per sector and create dataframe
#create barplots
#create heatmaps

#point 3:
#work on FUAs, extracting total per pollutant and per sector, for 1970 and 2015
