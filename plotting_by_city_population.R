# 28 August 2020
# E.Solazzo JRC

require('httr')
library(httr)
library(dplyr)
w.dir <- 'C:\\Users\\efsolazzo\\Documents\\Efisio\\2020\\EDGAR\\spatial_analysis\\'

path2data <- 'https://raw.githubusercontent.com/enricopisoni/emissions_urban_area_analysis/master/input/city_emissions_all_cities.csv'

city.df <- read.csv(path2data, sep=',', header=T, stringsAsFactors = F)

countries <- c('Germany','China', 'UnitedStates')


#---- start the pipe until plotting
test2.plot <- city.df %>% filter(country %in% countries & pollutant=='CO2' & area > 50) %>%
  
  mutate(population_group=cut(population, breaks = c(50000,100000,250000,500000,1000000,2500000,5000000,Inf))) %>% 
  
  select(c(emissions,country,year,population_group, population)) %>%
  group_by(country,year,population_group) %>%   
  # option 1: normlaise by total population 
  summarise(emi.city.group=sum(emissions)/sum(population)) %>% # for each city group, normalise by the total population
  # option 2: normalise by the number of cities in each group
  # summarise(emi.city.group=sum(emissions)/n()) %>% # for each city group, normalise by their number
  data.frame()%>% group_by(country,population_group) %>%
  # normalise by values in 1970 by country and population group
  mutate(emi.city.group.norm=emi.city.group/emi.city.group[year==1970])  %>% 
  
  # now plot 
  
  ggplot()+geom_line(aes(x=year,y=emi.city.group.norm, color=population_group)) + scale_color_grey(start = 0.2, end = .7) + 
  ylab('CO2 emi per capita/CO2 emi per capita 1970') + 
  geom_line(data= data.frame(CO2.country.total), aes(x=year, y=emi.norm, linetype=Total), color='red', size=2)+
  facet_wrap(.~country, scale='free') + theme(axis.title.x =element_blank())
  
  
  # get total country data
  
  d.dir <- 'D:\\work\\GHG_move\\EDGAR\\IPCC_food\\data\\'
  CO2.TS <- read.csv(file=paste0(d.dir,'EDGARv5.0_FT2018_fossil_CO2_GHG_booklet2019.csv'), header=T, sep=',', stringsAsFactors = F)
  CO2.TS$country_name[CO2.TS$country_name=='United States'] <- countries[3] # harmonise country names
  
  # extract data neeed,  melt, group and normalise by 1970 CO2 emission values
  CO2.country.total <- CO2.TS %>% filter(country_name %in% countries) %>% setnames(c('country', seq(1970,2015,1)))%>% 
    melt() %>% setnames(c('country', 'year','emissions')) %>% group_by(country) %>% mutate(emi.norm=emissions/emissions[year==1970])
  
  CO2.country.total$Total <- 'Country Total'
  CO2.country.total$year <- as.numeric(as.character(CO2.country.total$year))
  
  # combine the two plots and save
  
  test2 <- test2.plot +geom_line(data= CO2.country.total, aes(x=year, y=emi.norm, linetype=Total), color='red', size=2) +
    ggtitle('CO2 per capita emissions by urban population')
  
  ggsave(filename=paste0(w.dir, 'figs\\CO2_per_capita_test2.png'),
         device='png', width = 12, height = 7.5, units = "in",
         dpi = 420, test2)
  