In this repository you find data and code to analyse emissions per city, from 1970 to 2015.
Data are elaborated from EDGAR v5.0 emissions and GHSL FUAs definition.

***

R CODE

1) main_step1_global_maps.R
This code create global maps, per pollutant and year, with share of emissions belonging to urban, rural, etc... It also provide delta maps, of % change of share from 1970 to 2015

2) main_step2_stats_bysec.R
It creates figures with sectoral or geographical share of emissions, per continent - pollutant - year

3) main_step3_fuas_facet.R
It performs analysis on the change of emissions from 1970 to 2015, for a subset of big cities. 

***

INPUT FILES

- 'city_emissions.csv': it contains emissions for 1970 and 2015 (in tons) computed starting from EDGAR grids, and summing up emissions for each FUAs (from GHSL). More specifically, the file contains the following columns: city, country, population, area, year, emissions. It only considers cities with more than 1M inhabitants (but this values of 1M can be changed).

- 'for maps shares by country.xls': it contains, for 1970 and 2015, shares of emissions associated to rural, villages, urban centres, suburbs....for the various pollutants

- '1970_all_stats_by_Sector_MC.xls' and '2015_all_stats_by_Sector_MC.xls': they contain, for each pollutant, emissions on the various sectors (waste, residential, other, agriculture, energy-industry, transport) and geographical aggregations (urban, suburban, towns, village, dispersed areas, uninhabitated, rest)

***

OUTPUT FILES

In the output directories you find the figures produced by the 3 R codes, related to the aforementioned step1, step2, step3.

***
