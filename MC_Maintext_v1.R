library(ncdf4)
# library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
# library(broom)
library(usmap)
library(RANN)
library(ggtext)
library(ggpubr)

#### 0.1 convert AQM results to county level results ####
resultsInMAP_ref <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsInMAP_ref.csv", stringsAsFactors = F)
resultsInMAP_ref50 <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsInMAP_ref50.csv", stringsAsFactors = F)
resultsInMAP_nz <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsInMAP_nz.csv", stringsAsFactors = F)
resultsInMAP_race2 <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/NZ_2050_race2_pm2p5.csv", stringsAsFactors = F)
resultsISRM_ref <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsISRM_ref.csv", stringsAsFactors = F)
resultsISRM_ref50 <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsISRM_ref50.csv", stringsAsFactors = F)
resultsISRM_nz <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/resultsISRM_nz.csv", stringsAsFactors = F)
resultsISRM_race2 <- read.csv("~/Documents/PSU/2023 Spring/ModelsComparison/ISRM_NZ_2050_race2_pm2p5.csv", stringsAsFactors = F)

county_shp <- readOGR(dsn = "~/Documents/PSU/2023 Spring/ModelsComparison/tl_2021_us_county/tl_2021_us_county.shp")
county_shp_sf <- st_as_sf(county_shp)
state_shp <- readOGR(dsn = "~/Documents/PSU/2023 Spring/ModelsComparison/tl_2021_us_state/tl_2021_us_state.shp")
state_shp_sf <- st_as_sf(state_shp)

lat_jie <- nc_open("~/Documents/Princeton/2025/ComplexitySA/monthly_12km_2025-05-27/wrfout_d01_x2017_NEI2017_annual_2017.nc") %>% ncvar_get("XLAT")
lon_jie <- nc_open("~/Documents/Princeton/2025/ComplexitySA/monthly_12km_2025-05-27/wrfout_d01_x2017_NEI2017_annual_2017.nc") %>% ncvar_get("XLONG")

resultsInMAP_ref_cty <- st_as_sf(resultsInMAP_ref, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsInMAP_ref_cty <- st_join(county_shp_sf, resultsInMAP_ref_cty)
resultsInMAP_ref_cty <- resultsInMAP_ref_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsInMAP_ref_cty  <- resultsInMAP_ref_cty %>% st_drop_geometry()
names(resultsInMAP_ref_cty)[4] <- "fips"
resultsInMAP_ref_cty <- resultsInMAP_ref_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsInMAP_ref50_cty <- st_as_sf(resultsInMAP_ref50, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsInMAP_ref50_cty <- st_join(county_shp_sf, resultsInMAP_ref50_cty)
resultsInMAP_ref50_cty <- resultsInMAP_ref50_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsInMAP_ref50_cty  <- resultsInMAP_ref50_cty %>% st_drop_geometry()
names(resultsInMAP_ref50_cty)[4] <- "fips"
resultsInMAP_ref50_cty <- resultsInMAP_ref50_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsInMAP_nz_cty <- st_as_sf(resultsInMAP_nz, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsInMAP_nz_cty <- st_join(county_shp_sf, resultsInMAP_nz_cty)
resultsInMAP_nz_cty <- resultsInMAP_nz_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsInMAP_nz_cty  <- resultsInMAP_nz_cty %>% st_drop_geometry()
names(resultsInMAP_nz_cty)[4] <- "fips"
resultsInMAP_nz_cty <- resultsInMAP_nz_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsInMAP_race2_cty <- st_as_sf(resultsInMAP_race2, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsInMAP_race2_cty <- st_join(county_shp_sf, resultsInMAP_race2_cty)
resultsInMAP_race2_cty <- resultsInMAP_race2_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsInMAP_race2_cty  <- resultsInMAP_race2_cty %>% st_drop_geometry()
names(resultsInMAP_race2_cty)[4] <- "fips"
resultsInMAP_race2_cty <- resultsInMAP_race2_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsISRM_ref_cty <- st_as_sf(resultsISRM_ref, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsISRM_ref_cty <- st_join(county_shp_sf, resultsISRM_ref_cty)
resultsISRM_ref_cty <- resultsISRM_ref_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsISRM_ref_cty  <- resultsISRM_ref_cty %>% st_drop_geometry()
names(resultsISRM_ref_cty)[4] <- "fips"
resultsISRM_ref_cty <- resultsISRM_ref_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsISRM_ref50_cty <- st_as_sf(resultsISRM_ref50, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsISRM_ref50_cty <- st_join(county_shp_sf, resultsISRM_ref50_cty)
resultsISRM_ref50_cty <- resultsISRM_ref50_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsISRM_ref50_cty  <- resultsISRM_ref50_cty %>% st_drop_geometry()
names(resultsISRM_ref50_cty)[4] <- "fips"
resultsISRM_ref50_cty <- resultsISRM_ref50_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsISRM_nz_cty <- st_as_sf(resultsISRM_nz, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsISRM_nz_cty <- st_join(county_shp_sf, resultsISRM_nz_cty)
resultsISRM_nz_cty <- resultsISRM_nz_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsISRM_nz_cty  <- resultsISRM_nz_cty %>% st_drop_geometry()
names(resultsISRM_nz_cty)[4] <- "fips"
resultsISRM_nz_cty <- resultsISRM_nz_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

resultsISRM_race2_cty <- st_as_sf(resultsISRM_race2, coords = c("long", "lat"), crs = st_crs(4269)) %>% 
  group_by(group) %>% summarise(geometry = st_combine(geometry), TotalPM25 = mean(TotalPM25)) %>% st_cast("POLYGON")
resultsISRM_race2_cty <- st_join(county_shp_sf, resultsISRM_race2_cty)
resultsISRM_race2_cty <- resultsISRM_race2_cty %>% group_by(across(-c(group, TotalPM25))) %>% summarise_all(list(mean)) %>% ungroup()
resultsISRM_race2_cty  <- resultsISRM_race2_cty %>% st_drop_geometry()
names(resultsISRM_race2_cty)[4] <- "fips"
resultsISRM_race2_cty <- resultsISRM_race2_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78))) %>% select(-group)

jie_wrf_nz <- nc_open("~/Documents/PSU/2023 Spring/ModelsComparison/wrfout_d01_x2017_NZ2050_annual_2017.nc") %>% ncvar_get("pm25")
jie_wrf_nz <- data.frame(c(t(jie_wrf_nz)))
jie_wrf_nz$lat <- c(t(lat_jie))
jie_wrf_nz$lon <- c(t(lon_jie))
names(jie_wrf_nz)[1] <- "TotalPM25"
jie_wrf_nz_cty <- st_as_sf(jie_wrf_nz, coords = c("lon", "lat"), crs = st_crs(4269))
jie_wrf_nz_cty <- as_Spatial(jie_wrf_nz_cty)  
jie_wrf_nz_cty <- sp::over(county_shp, jie_wrf_nz_cty, fn = mean)  
jie_wrf_nz_cty <- bind_cols(county_shp@data, jie_wrf_nz_cty)
jie_wrf_nz_cty <- jie_wrf_nz_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78)))

na_counties_mc <- jie_wrf_nz_cty %>% filter(is.na(TotalPM25)) %>% select(GEOID)

jie_wrf_nz_cty_na <- county_shp@data %>% filter(GEOID %in% na_counties_mc$GEOID)
jie_wrf_nz_cty_nns <- nn2(data = jie_wrf_nz %>% select(lat, lon), query = jie_wrf_nz_cty_na %>% select(INTPTLAT, INTPTLON), k = 1)
jie_wrf_nz_cty_na$`TotalPM25` <- jie_wrf_nz[jie_wrf_nz_cty_nns$nn.idx, "TotalPM25"]
jie_wrf_nz_cty <- jie_wrf_nz_cty %>% filter(!is.na(TotalPM25)) %>% bind_rows(jie_wrf_nz_cty_na)
names(jie_wrf_nz_cty)[4] <- "fips"

jie_wrf_ref <- nc_open("~/Documents/PSU/2023 Spring/ModelsComparison/wrfout_d01_x2017_NEI2017_annual_2017.nc") %>% ncvar_get("pm25")
jie_wrf_ref <- data.frame(c(t(jie_wrf_ref)))
jie_wrf_ref$lat <- c(t(lat_jie))
jie_wrf_ref$lon <- c(t(lon_jie))
names(jie_wrf_ref)[1] <- "TotalPM25"
jie_wrf_ref_cty <- st_as_sf(jie_wrf_ref, coords = c("lon", "lat"), crs = st_crs(4269))
jie_wrf_ref_cty <- as_Spatial(jie_wrf_ref_cty)  
jie_wrf_ref_cty <- sp::over(county_shp, jie_wrf_ref_cty, fn = mean)  
jie_wrf_ref_cty <- bind_cols(county_shp@data, jie_wrf_ref_cty)
jie_wrf_ref_cty <- jie_wrf_ref_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78)))
jie_wrf_ref_cty_na <- county_shp@data %>% filter(GEOID %in% na_counties_mc$GEOID)
jie_wrf_ref_cty_nns <- nn2(data = jie_wrf_ref %>% select(lat, lon), query = jie_wrf_ref_cty_na %>% select(INTPTLAT, INTPTLON), k = 1)
jie_wrf_ref_cty_na$`TotalPM25` <- jie_wrf_ref[jie_wrf_ref_cty_nns$nn.idx, "TotalPM25"]
jie_wrf_ref_cty <- jie_wrf_ref_cty %>% filter(!is.na(TotalPM25)) %>% bind_rows(jie_wrf_ref_cty_na)
names(jie_wrf_ref_cty)[4] <- "fips"
# 
# jie_wrf_ref50 <- nc_open("~/Documents/PSU/2023 Spring/ModelsComparison/wrfout_d01_x2017_NEI2017_annual_2017.nc") %>% ncvar_get("pm25")
# jie_wrf_ref50 <- data.frame(c(t(jie_wrf_ref50)))
# jie_wrf_ref50$lat <- c(t(lat_jie))
# jie_wrf_ref50$lon <- c(t(lon_jie))
# names(jie_wrf_ref50)[1] <- "TotalPM25"
# jie_wrf_ref50_cty <- st_as_sf(jie_wrf_ref50, coords = c("lon", "lat"), crs = st_crs(4269))
# jie_wrf_ref50_cty <- as_Spatial(jie_wrf_ref50_cty)  
# jie_wrf_ref50_cty <- sp::over(county_shp, jie_wrf_ref50_cty, fn = mean)  
# jie_wrf_ref50_cty <- bind_cols(county_shp@data, jie_wrf_ref50_cty)
# jie_wrf_ref50_cty <- jie_wrf_ref50_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78)))
# jie_wrf_ref50_cty_na <- county_shp@data %>% filter(GEOID %in% na_counties_mc$GEOID)
# jie_wrf_ref50_cty_nns <- nn2(data = jie_wrf_ref50 %>% select(lat, lon), query = jie_wrf_ref50_cty_na %>% select(INTPTLAT, INTPTLON), k = 1)
# jie_wrf_ref50_cty_na$`TotalPM25` <- jie_wrf_ref50[jie_wrf_ref50_cty_nns$nn.idx, "TotalPM25"]
# jie_wrf_ref50_cty <- jie_wrf_ref50_cty %>% filter(!is.na(TotalPM25)) %>% bind_rows(jie_wrf_ref50_cty_na)
# names(jie_wrf_ref50_cty)[4] <- "fips"

jie_wrf_race2 <- nc_open("~/Documents/PSU/2023 Spring/ModelsComparison/wrfout_county_level_annual_2017_x2017_new_NZ2050.nc") %>% ncvar_get("PM25")
jie_wrf_race2 <- data.frame(c(t(jie_wrf_race2)))
jie_wrf_race2$lat <- c(t(lat_jie))
jie_wrf_race2$lon <- c(t(lon_jie))
names(jie_wrf_race2)[1] <- "TotalPM25"
jie_wrf_race2_cty <- st_as_sf(jie_wrf_race2, coords = c("lon", "lat"), crs = st_crs(4269))
jie_wrf_race2_cty <- as_Spatial(jie_wrf_race2_cty)  
jie_wrf_race2_cty <- sp::over(county_shp, jie_wrf_race2_cty, fn = mean)  
jie_wrf_race2_cty <- bind_cols(county_shp@data, jie_wrf_race2_cty)
jie_wrf_race2_cty <- jie_wrf_race2_cty %>% filter(!STATEFP %in% c("02", "15", as.character(57:78)))
jie_wrf_race2_cty_na <- county_shp@data %>% filter(GEOID %in% na_counties_mc$GEOID)
jie_wrf_race2_cty_nns <- nn2(data = jie_wrf_race2 %>% select(lat, lon), query = jie_wrf_race2_cty_na %>% select(INTPTLAT, INTPTLON), k = 1)
jie_wrf_race2_cty_na$`TotalPM25` <- jie_wrf_race2[jie_wrf_race2_cty_nns$nn.idx, "TotalPM25"]
jie_wrf_race2_cty <- jie_wrf_race2_cty %>% filter(!is.na(TotalPM25)) %>% bind_rows(jie_wrf_race2_cty_na)
names(jie_wrf_race2_cty)[4] <- "fips"

save(list = c("jie_wrf_ref_cty", "jie_wrf_nz_cty", "jie_wrf_race2_cty", 
              "resultsInMAP_ref_cty", "resultsInMAP_ref50_cty", "resultsInMAP_nz_cty", "resultsInMAP_race2_cty", 
              "resultsISRM_ref_cty", "resultsISRM_nz_cty"),
     file = "~/Documents/PSU/2023 Spring/ModelsComparison/model_results_cty_v1.rdata")

# read in the county data again! 
load(file = "~/Documents/PSU/2023 Spring/ModelsComparison/model_results_cty_v1.rdata")

#### 0.2 Health Inpact Assessment (HIA) ####

y0 <- read.csv("~/Documents/PSU/2023 Spring/HIA_County/baseline_mortality_benmap.csv", stringsAsFactors = F)
y0 <- y0 %>% mutate(Column = sprintf("%02d", Column), Row = sprintf("%03d", Row))
# In 2015, Shannon County (FIPS 46113) is renamed to Oglala Lakota County (46102)
y0 <- y0 %>% mutate(Row = ifelse(Column == "46" & Row == "113", "102", Row))

pop_2015_for_y0 <- read.csv("~/Documents/PSU/2023 Spring/HIA_County/population_2017_y0_age_groups.csv", stringsAsFactors = F)
pop_2015_for_y0 <- pop_2015_for_y0 %>% mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY))
pop_ssps_for_y0 <- read.csv("~/Documents/PSU/2023 Spring/HIA_County/population_ssps_y0_age_groups.csv", stringsAsFactors = F)
pop_ssps_for_y0 <- pop_ssps_for_y0 %>% mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY))
pop_ssps_for_y0 <- pop_ssps_for_y0 %>% mutate(COUNTY = ifelse(STATE == "46" & COUNTY == "113", "102", COUNTY))
pop_ssps_for_y0 <- pop_ssps_for_y0 %>% mutate(TOT_POP = ifelse(STATE == "46" & COUNTY == "102", 13672, TOT_POP)) # population in 46102 in 2020

RR_10 <- 1.06 # from Krewski et al., 2019. This value means that RR = 1.06 when c(PM2.5) increases by 10 ug/m3. range: 1.016 - 1.053
beta <- log(RR_10) / 10 # beta ~= 0.00583

# 2050 Di
beta_di_nhwa <- 0.0061 # NHWA, adjusted by TOM, other betas are the same, 0.0001
beta_di_hispanic <- 0.0110 # HWA, 0.0008, se
beta_di_black <- 0.0189 # BA, 0.0004
beta_di_asian <- 0.0092 # AA, 0.0010
beta_di_native <- 0.0095 # IA, 0.0019
# Lower
# beta_di_nhwa <- 0.0058 # NHWA, adjusted by TOM, other betas are the same, 0.0001
# beta_di_hispanic <- 0.0058 # HWA, 0.0008
# beta_di_black <- 0.0181 # BA, 0.0004
# beta_di_asian <- 0.0072 # AA, 0.0010
# beta_di_native <- 0.0095 # IA, 0.0019
# Upper
# beta_di_nhwa <- 0.0063 # NHWA, adjusted by TOM, other betas are the same, 0.0001
# beta_di_hispanic <- 0.0131 # HWA, 0.0008
# beta_di_black <- 0.0196 # BA, 0.0004
# beta_di_asian <- 0.0111 # AA, 0.0010
# beta_di_native <- 0.0095 # IA, 0.0019


jie_wrf_ref_cty <- jie_wrf_ref_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
jie_wrf_ref_HIA <- jie_wrf_ref_cty %>% left_join(pop_2015_for_y0, by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
jie_wrf_ref_HIA <- jie_wrf_ref_HIA %>% left_join(y0 %>% filter(year == 2015), 
                                         by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
jie_wrf_ref_HIA <- jie_wrf_ref_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                      AF = (RR - 1) / RR, 
                                      dr = Value * AF, 
                                      deaths = dr * TOT_POP)


jie_wrf_nz_cty <- jie_wrf_nz_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
jie_wrf_nz_HIA <- jie_wrf_nz_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                       by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
jie_wrf_nz_HIA <- jie_wrf_nz_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                       by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
jie_wrf_nz_HIA <- jie_wrf_nz_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                    AF = (RR - 1) / RR, 
                                    dr = Value * AF, 
                                    deaths = dr * TOT_POP)

jie_wrf_race2_cty <- jie_wrf_race2_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
jie_wrf_race2_HIA <- jie_wrf_race2_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                               by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
jie_wrf_race2_HIA <- jie_wrf_race2_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                               by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
jie_wrf_race2_HIA <- jie_wrf_race2_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                            AF = (RR - 1) / RR, 
                                            dr = Value * AF, 
                                            deaths = dr * TOT_POP)

resultsInMAP_ref_cty <- resultsInMAP_ref_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
InMAP_ref_HIA <- resultsInMAP_ref_cty %>% left_join(pop_2015_for_y0, by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_ref_HIA <- InMAP_ref_HIA %>% left_join(y0 %>% filter(year == 2015), 
                                             by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_ref_HIA <- InMAP_ref_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                          AF = (RR - 1) / RR, 
                                          dr = Value * AF, 
                                          deaths = dr * TOT_POP)

resultsInMAP_ref50_cty <- resultsInMAP_ref50_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
InMAP_ref50_HIA <- resultsInMAP_ref50_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                                        by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_ref50_HIA <- InMAP_ref50_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                                 by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_ref50_HIA <- InMAP_ref50_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                              AF = (RR - 1) / RR, 
                                              dr = Value * AF, 
                                              deaths = dr * TOT_POP)

resultsInMAP_nz_cty <- resultsInMAP_nz_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
InMAP_nz_HIA <- resultsInMAP_nz_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                                  by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_nz_HIA <- InMAP_nz_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                           by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_nz_HIA <- InMAP_nz_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                        AF = (RR - 1) / RR, 
                                        dr = Value * AF, 
                                        deaths = dr * TOT_POP)

resultsInMAP_race2_cty <- resultsInMAP_race2_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
InMAP_race2_HIA <- resultsInMAP_race2_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                                  by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_race2_HIA <- InMAP_race2_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                           by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_race2_HIA <- InMAP_race2_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                        AF = (RR - 1) / RR, 
                                        dr = Value * AF, 
                                        deaths = dr * TOT_POP)

resultsInMAP_nz_cty <- resultsInMAP_nz_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
InMAP_nz_HIA_ssp3 <- resultsInMAP_nz_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp3", year == "2050") %>% select(-year), 
                                                       by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_nz_HIA_ssp3 <- InMAP_nz_HIA_ssp3 %>% left_join(y0 %>% filter(year == 2050), 
                                                     by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_nz_HIA_ssp3 <- InMAP_nz_HIA_ssp3 %>% mutate(RR = exp(beta * TotalPM25),
                                                  AF = (RR - 1) / RR, 
                                                  dr = Value * AF, 
                                                  deaths = dr * TOT_POP)

InMAP_nz_HIA_ssp5 <- resultsInMAP_nz_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp5", year == "2050") %>% select(-year), 
                                                       by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
InMAP_nz_HIA_ssp5 <- InMAP_nz_HIA_ssp5 %>% left_join(y0 %>% filter(year == 2050), 
                                                     by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
InMAP_nz_HIA_ssp5 <- InMAP_nz_HIA_ssp5 %>% mutate(RR = exp(beta * TotalPM25),
                                                  AF = (RR - 1) / RR, 
                                                  dr = Value * AF, 
                                                  deaths = dr * TOT_POP)


resultsISRM_nz_cty <- resultsISRM_nz_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
ISRM_nz_HIA <- resultsISRM_nz_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                                by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
ISRM_nz_HIA <- ISRM_nz_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                         by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
ISRM_nz_HIA <- ISRM_nz_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                      AF = (RR - 1) / RR, 
                                      dr = Value * AF, 
                                      deaths = dr * TOT_POP)

resultsISRM_race2_cty <- resultsISRM_race2_cty %>% filter(STATEFP <= 56, STATEFP != "02", STATEFP != "15")
ISRM_race2_HIA <- resultsISRM_race2_cty %>% left_join(pop_ssps_for_y0 %>% filter(ssp == "ssp2", year == "2050") %>% select(-year), 
                                                by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))
ISRM_race2_HIA <- ISRM_race2_HIA %>% left_join(y0 %>% filter(year == 2050), 
                                         by = c("STATEFP" = "Column", "COUNTYFP" = "Row", "AGEGRP" = "Start.Age"))
ISRM_race2_HIA <- ISRM_race2_HIA %>% mutate(RR = exp(beta * TotalPM25),
                                      AF = (RR - 1) / RR, 
                                      dr = Value * AF, 
                                      deaths = dr * TOT_POP)

#### 0.3 Some demographic calcs ####

wa_2017 <- read.csv("~/Documents/PSU/2023 Spring/HIA_County/cc-est2019-alldata.csv")
wa_2017 <- wa_2017 %>% filter(YEAR == 10, AGEGRP == 0) %>% 
  mutate(wa = NHWA_MALE + NHWA_FEMALE, ba = NHBA_MALE + NHBA_FEMALE, 
         his = H_MALE + H_FEMALE, other = TOT_POP - wa - ba - his, all_other = ba + his + other, pop = TOT_POP, wa_ratio = wa/pop) %>% 
  select(STATE, COUNTY, STNAME, CTYNAME, AGEGRP, TOT_POP, wa, ba, his, other, all_other, wa_ratio) %>% 
  filter(STATE <= 56, STATE != "02", STATE != "15")
wa_2017 <- wa_2017 %>% mutate(STATE = sprintf("%02d", STATE), COUNTY = sprintf("%03d", COUNTY))

# White alone in 2020 - 2100
wa_ssps <- read_xlsx("~/Documents/PSU/2023 Spring/HIA_County/SEDAC_georeferenced_county_population_proj_excel files/hauer_county_whiteNH_pop_SSPs.xlsx")
ba_ssps <- read_xlsx("~/Documents/PSU/2023 Spring/HIA_County/SEDAC_georeferenced_county_population_proj_excel files/hauer_county_blackNH_pop_SSPs.xlsx")
his_ssps <- read_xlsx("~/Documents/PSU/2023 Spring/HIA_County/SEDAC_georeferenced_county_population_proj_excel files/hauer_county_hispanic_pop_SSPs.xlsx")
oth_ssps <- read_xlsx("~/Documents/PSU/2023 Spring/HIA_County/SEDAC_georeferenced_county_population_proj_excel files/hauer_county_other_race_pop_SSPs.xlsx")
tot_ssps <- read_xlsx("~/Documents/PSU/2023 Spring/HIA_County/SEDAC_georeferenced_county_population_proj_excel files/hauer_county_totpop_SSPs.xlsx")

wa_ssps <- wa_ssps[, c(1, 2, 4, 6, 19:103)]
wa_ssps <- wa_ssps %>% gather(key = sspyear, value = wa, -STATEFP10, -COUNTYFP10, -GEOID10, -NAMELSAD10)
wa_ssps <- wa_ssps %>% mutate(ssp = substr(sspyear, 1, 4), year = substr(sspyear, 5, 8)) %>% select(-sspyear)
wa_ssps <- wa_ssps %>% arrange(GEOID10, ssp, year)

ba_ssps <- ba_ssps[, c(1, 2, 4, 6, 19:103)]
ba_ssps <- ba_ssps %>% gather(key = sspyear, value = ba, -STATEFP10, -COUNTYFP10, -GEOID10, -NAMELSAD10)
ba_ssps <- ba_ssps %>% mutate(ssp = substr(sspyear, 1, 4), year = substr(sspyear, 5, 8)) %>% select(-sspyear)
ba_ssps <- ba_ssps %>% arrange(GEOID10, ssp, year)

his_ssps <- his_ssps[, c(1, 2, 4, 6, 19:103)]
his_ssps <- his_ssps %>% gather(key = sspyear, value = his, -STATEFP10, -COUNTYFP10, -GEOID10, -NAMELSAD10)
his_ssps <- his_ssps %>% mutate(ssp = substr(sspyear, 1, 4), year = substr(sspyear, 5, 8)) %>% select(-sspyear)
his_ssps <- his_ssps %>% arrange(GEOID10, ssp, year)

oth_ssps <- oth_ssps[, c(1, 2, 4, 6, 19:103)]
oth_ssps <- oth_ssps %>% gather(key = sspyear, value = oth, -STATEFP10, -COUNTYFP10, -GEOID10, -NAMELSAD10)
oth_ssps <- oth_ssps %>% mutate(ssp = substr(sspyear, 1, 4), year = substr(sspyear, 5, 8)) %>% select(-sspyear)
oth_ssps <- oth_ssps %>% arrange(GEOID10, ssp, year)

tot_ssps <- tot_ssps[, c(1, 2, 4, 6, 19:103)]
tot_ssps <- tot_ssps %>% gather(key = sspyear, value = pop, -STATEFP10, -COUNTYFP10, -GEOID10, -NAMELSAD10)
tot_ssps <- tot_ssps %>% mutate(ssp = substr(sspyear, 1, 4), year = substr(sspyear, 5, 8)) %>% select(-sspyear)
tot_ssps <- tot_ssps %>% arrange(GEOID10, ssp, year)

wa_ssps <- data.frame(wa_ssps[, c(1:4, 6, 7, 5)], 
                      ba = ba_ssps$ba, his = his_ssps$his, other = oth_ssps$oth, 
                      all_other = ba_ssps$ba + his_ssps$his + oth_ssps$oth, 
                      pop = tot_ssps$pop)
wa_ssps$wa_ratio <- wa_ssps$wa / wa_ssps$pop


#### 1. Figure 2a. National PM2.5-attributable deaths ####

deaths_InMAP_ref50 <- sum(InMAP_ref50_HIA$deaths)
deaths_InMAP_nz <- sum(InMAP_nz_HIA$deaths) 
A1 = sum(InMAP_nz_HIA$deaths) * sum(InMAP_nz_HIA_ssp3$TOT_POP) / sum(InMAP_nz_HIA$TOT_POP)
B1 = sum(InMAP_nz_HIA$Value * InMAP_nz_HIA_ssp3$TOT_POP * InMAP_nz_HIA$AF)
C1 = sum(InMAP_nz_HIA_ssp3$TOT_POP * InMAP_nz_HIA_ssp3$Value * InMAP_nz_HIA$AF) * 1.2  # derived from IFs

DA_mc <- data.frame(ref50 = deaths_InMAP_ref50,
                    nz = deaths_InMAP_nz, 
                    A = A1, # population effect
                    B = B1, # ageing effect
                    C = C1 # baseline mortality rate effect
)





A2 = sum(InMAP_nz_HIA$deaths) * sum(InMAP_nz_HIA_ssp5$TOT_POP) / sum(InMAP_nz_HIA$TOT_POP)
B2 = sum(InMAP_nz_HIA$Value * InMAP_nz_HIA_ssp5$TOT_POP * InMAP_nz_HIA$AF)
C2 = sum(InMAP_nz_HIA_ssp5$TOT_POP * InMAP_nz_HIA_ssp5$Value * InMAP_nz_HIA$AF) * 0.8


DA_mc[2, ] <- data.frame(ref50 = deaths_InMAP_ref50,
                         nz = deaths_InMAP_nz, 
                         A = A2, # population effect
                         B = B2, # ageing effect
                         C = C2 # baseline mortality rate effect
                         
)

rownames(DA_mc) <- c("SSP3", "SSP5")
# DA_mc <- DA_mc %>% gather(key = "component", value = "value", -scenario)

deaths_InMAP_ref <- sum(InMAP_ref_HIA$deaths)
deaths_InMAP_ref50 <- sum(InMAP_ref50_HIA$deaths)
w = .3
p2 <- ggplot() + 
  annotate(geom = "rect", xmin = 1, ymin = 0, xmax = 2.5, ymax = 150, fill = "lightgreen", alpha = .3) +
  annotate(geom = "rect", xmin = 2.5, ymin = 0, xmax = 5.45, ymax = 150, fill = "lightblue", alpha = .3) +
  geom_rect(aes(xmin = 0.2 - w/3, xmax = 0.2 + w/3, ymin = 0, ymax = deaths_InMAP_ref/1e3), 
            color = "black", fill = "white", size = 1) +
  geom_rect(aes(xmin = 0.8 - w/3, xmax = 0.8 + w/3, ymin = 0, ymax = deaths_InMAP_ref50/1e3), 
            color = "black", fill = "white", size = 1) +
  geom_rect(aes(xmin = 1.4 - w/3, xmax = 1.4 + w/3, ymin = deaths_InMAP_nz/1e3, ymax = deaths_InMAP_ref50/1e3), 
            color = "black", fill = "white") +
  geom_rect(aes(xmin = 2 - w/3, xmax = 2 + w/3, ymin = 0, ymax = deaths_InMAP_nz/1e3), 
            color = "black", fill = "white", size = 1) +
  geom_rect(aes(xmin = c(3-w/2, 4-w/2, 5-w/2, 6-w/2) - w/3, 
                xmax = c(3-w/2, 4-w/2, 5-w/2, 6-w/2) + w/3, 
                ymin = c(DA_mc["SSP3", "nz"], DA_mc["SSP3", "A"], DA_mc["SSP3", "B"], 0) / 1e3, 
                ymax = c(DA_mc["SSP3", "A"], DA_mc["SSP3", "B"], DA_mc["SSP3", "C"],  DA_mc["SSP3", "C"])/1e3, 
                fill = rep("SSP3", 4)
  ), 
  color = "black", size = c(0.5, 0.5, 0.5, 1)) + 
  geom_rect(aes(xmin = c(3+w/2, 4+w/2, 5+w/2, 6+w/2) - w/3, 
                xmax = c(3+w/2, 4+w/2, 5+w/2, 6+w/2) + w/3, 
                ymin = c(DA_mc["SSP5", "nz"], DA_mc["SSP5", "A"], DA_mc["SSP5", "B"], 0) / 1e3, 
                ymax = c( DA_mc["SSP5", "A"], DA_mc["SSP5", "B"], DA_mc["SSP5", "C"], DA_mc["SSP5", "C"])/1e3, 
                fill = rep("SSP5", 4)
  ),
  color = "black", size = c(0.5, 0.5, 0.5, 1)) + 
  labs(
    title = "<b>a ) National total health damages: PM<sub>2.5</sub>-attributable deaths</b>",
    x = "", y = "Thousand deaths", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(margin = margin(b = 36)), axis.text.x = element_markdown(size = 14),
        panel.background = element_blank(), legend.title = element_markdown(), legend.position = c(0.95, 0.89), 
        legend.box.background = element_blank(), legend.background = element_rect(fill = "transparent")) + 
  coord_cartesian(clip = "off") + 
  scale_x_continuous(breaks = c(0.2, 0.8, 2, 6), 
                     labels = c("2017", 
                                "2050<br>Reference<br>(SSP2)", 
                                "2050<br>Net-Zero<br>(SSP2)",
                                "2050<br>Net-Zero<br>(SSP3 or SSP5)")) +
  scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) + 
  scale_fill_manual(values = c("base" = "white", "SSP3" = "darkorange", "SSP5" = "dodgerblue"), 
                    breaks = c("SSP3", "SSP5")) +
  annotate(geom = "segment", x = 0.8+w/3, y = DA_mc["SSP3", "ref50"]/1e3, xend = 1.4-w/3, yend = DA_mc["SSP3", "ref50"]/1e3, linetype = "dashed", linewidth = .5, color = "black") +
  annotate(geom = "segment", x = 1.4+w/3, y = DA_mc["SSP3", "nz"]/1e3, xend = 2-w/3, yend = DA_mc["SSP3", "nz"]/1e3, linetype = "dashed", linewidth = .5, color = "black") +
  annotate(geom = "segment", x = 2+w/3, y = DA_mc["SSP3", "nz"]/1e3, xend = 3-w/3, yend = DA_mc["SSP3", "nz"]/1e3, linetype = "dashed", linewidth = .5, color = "black") +
  annotate(geom = "segment", x = 1.4-w/2, y = DA_mc["SSP3", "ref50"]/1e3, xend = 1.4-w/2, yend = DA_mc["SSP3", "nz"]/1e3, linewidth = .5, color = "black", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 3-w, y = DA_mc["SSP3", "nz"]/1e3, xend = 3-w, yend = DA_mc["SSP3", "A"]/1e3, linewidth = .5, color = "orange", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 4-w, y = DA_mc["SSP3", "A"]/1e3, xend = 4-w, yend = DA_mc["SSP3", "B"]/1e3, linewidth = .5, color = "orange", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 5-w, y = DA_mc["SSP3", "B"]/1e3, xend = 5-w, yend = DA_mc["SSP3", "C"]/1e3, linewidth = .5, color = "orange", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 3-w/6, y = DA_mc["SSP3", "A"]/1e3, xend = 4-w*5/6, yend = DA_mc["SSP3", "A"]/1e3, linetype = "dashed", linewidth = .5, color = "orange") +
  annotate(geom = "segment", x = 4-w/6, y = DA_mc["SSP3", "B"]/1e3, xend = 5-w*5/6, yend = DA_mc["SSP3", "B"]/1e3, linetype = "dashed", linewidth = .5, color = "orange") +
  annotate(geom = "segment", x = 5-w/6, y = DA_mc["SSP3", "C"]/1e3, xend = 6-w*5/6, yend = DA_mc["SSP3", "C"]/1e3, linetype = "dashed", linewidth = .5, color = "orange") +
  annotate(geom = "segment", x = 3+w, y = DA_mc["SSP5", "nz"]/1e3, xend = 3+w, yend = DA_mc["SSP5", "A"]/1e3, linewidth = .5, color = "dodgerblue", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 4+w, y = DA_mc["SSP5", "A"]/1e3, xend = 4+w, yend = DA_mc["SSP5", "B"]/1e3, linewidth = .5, color = "dodgerblue", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 5+w, y = DA_mc["SSP5", "B"]/1e3, xend = 5+w, yend = DA_mc["SSP5", "C"]/1e3, linewidth = .5, color = "dodgerblue", 
           lineend = "butt", linejoin = "mitre", 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
  annotate(geom = "segment", x = 3+w*5/6, y = DA_mc["SSP5", "A"]/1e3, xend = 4+w/6, yend = DA_mc["SSP5", "A"]/1e3, linetype = "dashed", linewidth = .5, color = "dodgerblue") +
  annotate(geom = "segment", x = 4+w*5/6, y = DA_mc["SSP5", "B"]/1e3, xend = 5+w/6, yend = DA_mc["SSP5", "B"]/1e3, linetype = "dashed", linewidth = .5, color = "dodgerblue") +
  annotate(geom = "segment", x = 5+w*5/6, y = DA_mc["SSP5", "C"]/1e3, xend = 6+w/6, yend = DA_mc["SSP5", "C"]/1e3, linetype = "dashed", linewidth = .5, color = "dodgerblue") +
  # geom_richtext(label = "<b>Socioeconomic<br>changes</b>", aes(x = 4, y = 175), 
  #               color = "black", fill = "transparent", label.color = NA, size = 4.5) +
  geom_richtext(label = "Population<br>growth", aes(x = 3, y = 50), 
                color = "black", fill = "transparent", label.color = NA, size = 4.5) +
  geom_richtext(label = "Aging", aes(x = 4, y = 45), 
                color = "black", fill = "transparent", label.color = NA, size = 4.5) +
  geom_richtext(label = "Baseline mortality <br>rate decline", aes(x = 5, y = 50), 
                color = "black", fill = "transparent", label.color = NA, size = 4.5) +
  # geom_richtext(label = "<b>Energy<br>decarbonization</b>", aes(x = 1.75, y = 175), 
  #               color = "black", fill = "transparent", label.color = NA, size = 4.5) +
  annotation_custom(
    grob = textGrob(
      x = unit(3.5/12, "npc"), y = unit(1.05, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 12), 
      label = "Energy decarbonization"
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(7.5/12, "npc"), y = unit(1.05, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 12), 
      label = "Socioeconomic changes"
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(3.5/12 + .01, "npc"), y0 = unit(1.08, "npc"),  
      x1 = unit(3.5/12 + .01, "npc"), y1 = unit(1.105, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(3.5/12 + .01, "npc"), y0 = unit(1.105, "npc"),  
      x1 = unit(7.5/12 - .01, "npc"), y1 = unit(1.105, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(7.5/12 - .01, "npc"), y0 = unit(1.08, "npc"),  
      x1 = unit(7.5/12 - .01, "npc"), y1 = unit(1.105, "npc"),  
      gp = gpar(col = "black", lwd = 2)
    )
  ) +
  annotation_custom(
    grob = textGrob(
      x = unit(5.5/12, "npc"), y = unit(1.15, "npc"),  
      gp = gpar(lineheight = 0.8, fontsize = 13), 
      label = "Macro-scale factors"
    )
  ) 

# ggsave("~/Documents/PSU/2023 Spring/ModelsComparison/Viz_v1/Fig2.png", p2, width = 9, height = 7)
ggsave("~/Documents/PSU/2023 Spring/ModelsComparison/Viz_Maintext_v1/Fig2.png", p2, width = 11, height = 4.5)


#### Some space for the other 2 panels of figure 2 #### 


#### 3. Figure 3 ####
#### first row

resultsInMAP_d <- resultsInMAP_nz_cty
resultsInMAP_d$TotalPM25 <- resultsInMAP_d$TotalPM25 - resultsInMAP_ref_cty$TotalPM25
p3a <- plot_usmap(data = resultsInMAP_d %>% select(fips, TotalPM25) %>% drop_na() %>% 
                    mutate(TotalPM25 = pmax(TotalPM25, -5)) %>% mutate(TotalPM25 = pmin(TotalPM25, 2.5)), 
                  regions = "counties", values = "TotalPM25", color = "grey30", linewidth = .1,
                  exclude = c(paste0("02", sprintf("%03d", 0:999)), paste0("15", sprintf("%03d", 0:999)))) + 
  scale_fill_gradient2(low = "green4", high = "orange2", limits = c(-5, 2.5),
                       breaks = c(-5, -2.5, 0, 2.5), labels = c("<-5", "2.5", "0", ">2.5")) +
  labs(x = "", y = "", 
       title = "<b>a) InMAP: Change in county-level <br>PM<sub>2.5</sub> concentration, 2050 Net-Zero <br>relative to 2017</b>",
       # title = "b) Proportional, InMAP",
       fill = "&mu;g/m<sup>3</sup>") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), 
        plot.title = element_markdown(),
        panel.background = element_blank(), 
        # legend.position = "none", 
        axis.text = element_blank(), axis.ticks =  element_blank(), axis.title = element_blank(), 
        legend.title = element_markdown(angle = 0, hjust = .5), 
        # legend.key.height = unit(1, "cm"), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = unit(10, "cm"))) +
  annotate("text", x = -1350000, y = -1600000, label = "plain('National average')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1335000, y = -1850000, label = "plain('in 2050 Net-Zero:')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1655000, y = -2100000, label = "4.2 * mu * g/m^3", parse = TRUE, size = 4.5)

jie_wrf_d <- jie_wrf_nz_cty
jie_wrf_d$TotalPM25 <- jie_wrf_d$TotalPM25 - jie_wrf_ref_cty$TotalPM25
p3b <- plot_usmap(data = jie_wrf_d %>% mutate(TotalPM25 = pmax(TotalPM25, -5)) %>% mutate(TotalPM25 = pmin(TotalPM25, 2.5)), 
                      regions = "counties", values = "TotalPM25", color = "grey30", linewidth = .1,
                      exclude = c(paste0("02", sprintf("%03d", 0:999)), paste0("15", sprintf("%03d", 0:999)))) + 
  scale_fill_gradient2(low = "green4", high = "orange2", limits = c(-5, 2.5),
                       breaks = c(-5, -2.5, 0, 2.5), labels = c("<-5", "2.5", "0", ">2.5")) +
  labs(x = "", y = "", 
       title = "<b>b) WRF-Chem: Change in county-level <br>PM<sub>2.5</sub> concentration, 2050 Net-Zero <br>relative to 2017</b>",
       # title = "a) Proportional, WRF-Chem",
       fill = "&mu;g/m<sup>3</sup>") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), 
        plot.title = element_markdown(),
        panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks =  element_blank(), axis.title = element_blank(), 
        legend.title = element_markdown(angle = 0, hjust = .5), 
        # legend.key.height = unit(1, "cm"), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  annotate("text", x = -1350000, y = -1600000, label = "plain('National average')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1335000, y = -1850000, label = "plain('in 2050 Net-Zero:')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1655000, y = -2100000, label = "6.3 * mu * g/m^3", parse = TRUE, size = 4.5)

resultsISRM_d <- resultsISRM_nz_cty
resultsISRM_d$TotalPM25 <- resultsISRM_d$TotalPM25 - resultsISRM_ref_cty$TotalPM25
p3c <- plot_usmap(data = resultsISRM_d %>% select(fips, TotalPM25) %>% drop_na() %>% 
                        mutate(TotalPM25 = pmax(TotalPM25, -5)) %>% mutate(TotalPM25 = pmin(TotalPM25, 2.5)), 
                      regions = "counties", values = "TotalPM25", color = "grey30", linewidth = .1,
                      exclude = c(paste0("02", sprintf("%03d", 0:999)), paste0("15", sprintf("%03d", 0:999)))) + 
  scale_fill_gradient2(low = "green4", high = "orange2", limits = c(-5, 2.5),
                       breaks = c(-5, -2.5, 0, 2.5), labels = c("<-5", "2.5", "0", ">2.5")) +
  labs(x = "", y = "", 
       title = "<b>c) ISRM: Change in county-level <br>PM<sub>2.5</sub> concentration, 2050 Net-Zero <br>relative to 2017</b>",
       # title = "d) Proportional, ISRM",
       fill = "&mu;g/m<sup>3</sup>") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), 
        plot.title = element_markdown(),
        panel.background = element_blank(), 
        # legend.position = "none", 
        axis.text = element_blank(), axis.ticks =  element_blank(), axis.title = element_blank(), 
        legend.title = element_markdown(angle = 90, hjust = .5), 
        # legend.key.height = unit(1, "cm"), 
        panel.border = element_blank(), legend.box.background = element_rect()) +
  guides(fill = guide_colorbar(title.position = "right")) +
  annotate("text", x = -1350000, y = -1600000, label = "plain('National average')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1335000, y = -1850000, label = "plain('in 2050 Net-Zero:')", parse = TRUE, size = 4.5) +
  annotate("text", x = -1655000, y = -2100000, label = "4.1 * mu * g/m^3", parse = TRUE, size = 4.5)

p3_row1 <- ggarrange(p3a, p3b, p3c, nrow = 1, common.legend = T, legend = "bottom") %>% 
  annotate_figure(
    top = text_grob(
      "", 
      size = 16, face = "bold", hjust = 0.8
    )
  )

#### second row
models_ref_cty <- bind_rows(jie_wrf_ref_cty %>% select(fips, TotalPM25), 
                            resultsInMAP_ref_cty %>% select(fips, TotalPM25),
                            resultsISRM_ref_cty %>% select(fips, TotalPM25))
models_ref_cty$model <- rep(c("wrf", "InMAP", "ISRM"), each = 3108)
models_ref_cty$model <- factor(models_ref_cty$model, levels = c("wrf", "InMAP", "ISRM"))

models_nz_cty <- bind_rows(jie_wrf_nz_cty %>% select(fips, TotalPM25), 
                           resultsInMAP_nz_cty %>% select(fips, TotalPM25), 
                           resultsISRM_nz_cty %>% select(fips, TotalPM25))
# models_nz_ref_cty$TotalPM25 <- models_nz_ref_cty$TotalPM25 - models_ref_cty$TotalPM25
models_nz_cty$model <- rep(c("wrf", "InMAP", "ISRM"), each = 3108)
models_nz_cty$model <- factor(models_ref_cty$model, levels = c("wrf", "InMAP", "ISRM"))

models_cty <- models_ref_cty %>% mutate(nz = models_nz_cty$TotalPM25)
models_cty <- models_cty %>% group_by(model) %>% arrange(TotalPM25)
models_cty <- models_cty %>% mutate(ref_pct = percent_rank(TotalPM25), nz_pct = percent_rank(nz))

models_rank <- models_cty %>% select(model, ref_pct, nz_pct) %>% 
  mutate(ref_pct = ceiling(ref_pct * 100)) %>% mutate(ref_pct = pmax(ref_pct, 1)) %>% 
  group_by(model, ref_pct) %>% summarise(nz_pct = mean(nz_pct) * 100) %>% ungroup()

p3d <- ggplot() + 
  annotate(geom = "segment", x = 0, xend = 100, y = 0, yend = 100, color = "red3", size = .7) +
  geom_point(data = models_rank %>% filter(model == "InMAP"), 
             mapping = aes(x = ref_pct, y = nz_pct), size = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 15), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(), axis.text.x = element_markdown(),
        axis.title.x = element_markdown(size = 19), axis.title.y = element_markdown(size = 18),
        panel.background = element_blank(), legend.title = element_markdown(), 
        legend.box.background = element_blank()) +
  labs(
    title = "<b>d) InMAP: Relationships between <br>county's PM<sub>2.5</sub> rank in 2017 and <br>2050 Net-Zero</b>", 
    x = "PM<sub>2.5</sub> percentile rank in 2017", 
    y = "Projected PM<sub>2.5</sub> percentile rank in NZ 2050") +
  scale_x_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) + 
  scale_y_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) 


p3e <- ggplot() + 
  annotate(geom = "segment", x = 0, xend = 100, y = 0, yend = 100, color = "red3", size = .7) +
  geom_point(data = models_rank %>% filter(model == "wrf"), 
             mapping = aes(x = ref_pct, y = nz_pct), size = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 15), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(), axis.text.x = element_markdown(),
        axis.title.x = element_markdown(size = 19), axis.title.y = element_markdown(size = 18),
        panel.background = element_blank(), legend.title = element_markdown(), 
        legend.box.background = element_blank()) +
  labs(
    title = "<b>e) WRF-Chem: Relationships <br>between county's PM<sub>2.5</sub> rank in <br>2017 and 2050 Net-Zero</b>", 
    x = "PM<sub>2.5</sub> percentile rank in 2017", 
    y = "Projected PM<sub>2.5</sub> percentile rank in NZ 2050") +
  scale_x_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) + 
  scale_y_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) 

p3f <- ggplot() + 
  annotate(geom = "segment", x = 0, xend = 100, y = 0, yend = 100, color = "red3", size = .7) +
  geom_point(data = models_rank %>% filter(model == "ISRM"), 
             mapping = aes(x = ref_pct, y = nz_pct), size = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 15), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(), axis.text.x = element_markdown(),
        axis.title.x = element_markdown(size = 19), axis.title.y = element_markdown(size = 18),
        panel.background = element_blank(), legend.title = element_markdown(), 
        legend.box.background = element_blank()) +
  labs(
    title = "<b>f) ISRM: Relationships between <br>county's PM<sub>2.5</sub> rank in 2017 and <br>2050 Net-Zero</b>", 
    x = "PM<sub>2.5</sub> percentile rank in 2017", 
    y = "Projected PM<sub>2.5</sub> percentile rank in NZ 2050") +
  scale_x_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) + 
  scale_y_continuous(expand = c(0, 0), labels = c("0th", "25th", "50th", "75th", "100th")) 

p3_row2 <- ggarrange(p3d, NULL, p3e, NULL, p3f, NULL, nrow = 1, widths = c(4, .1, 4, .1, 4, .1), heights = 4.5)

p3 <- ggarrange(p3_row1, NULL, p3_row2, nrow = 3, widths = 15, heights = c(5, 0.05, 5.5))
ggsave("~/Documents/PSU/2023 Spring/ModelsComparison/Viz_v1/Fig3.png", p3, width = 15, height = 10)

#### 4. Figure 4 national and NY distributions ####

box_ref <- InMAP_ref_HIA %>% 
  select(STATEFP, COUNTYFP, fips, TotalPM25, year, deaths) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(wa_2017 %>% select(STATE, COUNTY, wa, ba, his, other, all_other, wa_ratio),
            by = c("STATEFP" = "STATE", "COUNTYFP" = "COUNTY"))

box_ref50 <- InMAP_ref50_HIA %>% 
  select(STATEFP, COUNTYFP, fips, TotalPM25, year, deaths) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(wa_ssps %>% filter(ssp == "ssp2", year == 2050) %>% select(-GEOID10, -NAMELSAD10, -year), 
            by = c("STATEFP" = "STATEFP10", "COUNTYFP" = "COUNTYFP10")) %>% 
  drop_na()

box_nz <- InMAP_nz_HIA %>% 
  select(STATEFP, COUNTYFP, fips, TotalPM25, year, deaths) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(wa_ssps %>% filter(ssp == "ssp2", year == 2050) %>% select(-GEOID10, -NAMELSAD10, -year), 
            by = c("STATEFP" = "STATEFP10", "COUNTYFP" = "COUNTYFP10")) %>% 
  drop_na()

box_race2 <- InMAP_race2_HIA %>% 
  select(STATEFP, COUNTYFP, fips, TotalPM25, year, deaths) %>% 
  group_by(STATEFP, COUNTYFP, fips, TotalPM25, year) %>% 
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  left_join(wa_ssps %>% filter(ssp == "ssp2", year == 2050) %>% select(-GEOID10, -NAMELSAD10, -year), 
            by = c("STATEFP" = "STATEFP10", "COUNTYFP" = "COUNTYFP10")) %>% 
  drop_na()

fig_4a <- ggplot() + 
  geom_boxplot(data = box_ref, 
               mapping = aes(x = 1.2, y = TotalPM25, weight = wa, color = "wa"), width = 0.3, outlier.shape = NA) + 
  geom_boxplot(data = box_ref, 
               mapping = aes(x = 0.8, y = TotalPM25, weight = ba, color = "nw"), width = 0.3, outlier.shape = NA) +
  geom_boxplot(data = box_ref50, 
               mapping = aes(x = 2.2, y = TotalPM25, weight = wa, color = "wa"), width = 0.3, outlier.shape = NA) +
  geom_boxplot(data = box_ref50, 
               mapping = aes(x = 1.8, y = TotalPM25, weight = ba, color = "nw"), width = 0.3, outlier.shape = NA) +
  geom_boxplot(data = box_nz, 
               mapping = aes(x = 3.2, y = TotalPM25, weight = wa, color = "wa"), width = 0.3, outlier.shape = NA) + 
  geom_boxplot(data = box_nz, 
               mapping = aes(x = 2.8, y = TotalPM25, weight = ba, color = "nw"), width = 0.3, outlier.shape = NA) +
  geom_boxplot(data = box_race2, 
               mapping = aes(x = 4.2, y = TotalPM25, weight = wa, color = "wa"), width = 0.3, outlier.shape = NA) +
  geom_boxplot(data = box_race2, 
               mapping = aes(x = 3.8, y = TotalPM25, weight = ba, color = "nw"), width = 0.3, outlier.shape = NA) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(hjust = 0.5), axis.text.x = element_markdown(),
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), 
        legend.box.background = element_blank(), legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "&mu;g/m<sup>3</sup>", color = "Racial group",
       title = "<b>a) Nationwide distribution of PM<sub>2.5</sub> exposure for Black and White populations</b>") +
  scale_x_continuous(breaks = 1:4, labels = c("2017", "2050 Reference", "2050 Net-Zero", "2050 Net-Zero \n(Equity)")) +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  scale_color_manual(labels = c("Black", "White"), values = c("wa" = "brown4", "nw" = "grey30")) + 
  annotate("text", x = 1, y = 16, label = "N = 41 M", color = "grey20") + 
  annotate("text", x = 1.4, y = 13, label = "N = 197 M", color = "red3") +
  annotate("text", x = 2, y = 11, label = "N = 58 M", color = "grey20") + 
  annotate("text", x = 2.4, y = 9, label = "N = 195 M", color = "red3") +
  annotate("text", x = 3, y = 10, label = "N = 58 M", color = "grey20") + 
  annotate("text", x = 3.4, y = 8, label = "N = 195 M", color = "red3") +
  annotate("text", x = 3.9, y = 7, label = "N = 58 M", color = "grey20") + 
  annotate("text", x = 4.4, y = 7, label = "N = 195 M", color = "red3")


dt_fig_4b <- bind_rows(box_ref %>% filter(STATEFP == "36") %>% mutate(scenario = "ref17"), 
                       box_ref50 %>% filter(STATEFP == "36") %>% mutate(scenario = "ref50"), 
                       box_nz %>% filter(STATEFP == "36") %>% mutate(scenario = "nz"), 
                       box_race2 %>% filter(STATEFP == "36") %>% mutate(scenario = "equity"))
dt_fig_4b$scenario <- factor(dt_fig_3c$scenario, levels = c("ref17", "ref50", "nz", "equity"))

fig_4b <- ggplot() + 
  geom_density(data = dt_fig_4b,
               mapping = aes(x = TotalPM25, weight = ba, 
                             color = scenario, linetype = scenario, linewidth = scenario), 
               adjust = 5, key_glyph = "path") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), panel.border = element_blank(), axis.line.x.bottom = element_line(),
        # axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.title = element_markdown(), 
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), legend.position = c(.7, .8),
        legend.box.background = element_blank()) +
  labs(x = "PM<sub>2.5</sub> concentration (&mu;g/m<sup>3</sup>)", y = "", 
       title = "<b>b) Black population: State-wide<br>distributions of PM<sub>2.5</sub> exposure in NY</b>", 
       color = "", linetype = "", linewidth = "") +
  scale_x_continuous(limits = c(-1, 20), expand = c(0, 0), 
                     breaks = seq(0, 20, by = 5)) +
  scale_y_continuous(limits = c(0, .3), expand = c(0, 0)) +
  scale_color_manual(values = c("ref17" = "black", "ref50" = "black", "nz" = "dodgerblue3", "equity" = "green2"), 
                     labels = c("2017 (N = 2.8 M)", "2050 Reference (N = 3.1 M)", "2050 Net-Zero (N = 3.1 M)", "2050 Net-Zero (Equity) (N = 3.1 M)")) +
  scale_linetype_manual(values = c("ref17" = "dashed", "ref50" = "solid", "nz" = "solid", "equity" = "solid"), 
                        labels = c("2017 (N = 2.8 M)", "2050 Reference (N = 3.1 M)", "2050 Net-Zero (N = 3.1 M)", "2050 Net-Zero (Equity) (N = 3.1 M)")) +
  scale_linewidth_manual(values = c("ref17" = .5, "ref50" = .5, "nz" = 1.5, "equity" = 1.5), 
                         labels = c("2017 (N = 2.8 M)", "2050 Reference (N = 3.1 M)", "2050 Net-Zero (N = 3.1 M)", "2050 Net-Zero (Equity) (N = 3.1 M)")) +
  # guides(color = guide_legend(override.aes = list(linetype = ))) +
  annotate(geom = "segment", x = 0, y = 0, xend = 0, yend = .3, color = "grey80", size = .2)

fig_4c <- ggplot() + 
  geom_density(data = dt_fig_4b,
               mapping = aes(x = TotalPM25, weight = wa, 
                             color = scenario, linetype = scenario, linewidth = scenario), 
               adjust = 5, key_glyph = "path") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 15), panel.border = element_blank(), axis.line.x.bottom = element_line(),
        # axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.title = element_markdown(), 
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), legend.position = c(.7, .8),
        legend.box.background = element_blank()) +
  labs(x = "PM<sub>2.5</sub> concentration (&mu;g/m<sup>3</sup>)", y = "", 
       title = "<b>c) White population: State-wide<br>Distributions of PM<sub>2.5</sub> exposure in NY</b>", 
       color = "", linetype = "", linewidth = "") +
  scale_x_continuous(limits = c(-1, 20), expand = c(0, 0), 
                     breaks = seq(0, 20, by = 5)) +
  scale_y_continuous(limits = c(0, .3), expand = c(0, 0)) +
  scale_color_manual(values = c("ref17" = "black", "ref50" = "black", "nz" = "dodgerblue3", "equity" = "green2"), 
                     labels = c("2017 (N = 10.9 M)", "2050 Reference (N = 9.1 M)", "2050 Net-Zero (N = 9.1 M)", "2050 Net-Zero (Equity) (N = 9.1 M)")) +
  scale_linetype_manual(values = c("ref17" = "dashed", "ref50" = "solid", "nz" = "solid", "equity" = "solid"), 
                        labels = c("2017 (N = 10.9 M)", "2050 Reference (N = 9.1 M)", "2050 Net-Zero (N = 9.1 M)", "2050 Net-Zero (Equity) (N = 9.1 M)")) +
  scale_linewidth_manual(values = c("ref17" = .5, "ref50" = .5, "nz" = 1.5, "equity" = 1.5), 
                         labels = c("2017 (N = 10.9 M)", "2050 Reference (N = 9.1 M)", "2050 Net-Zero (N = 9.1 M)", "2050 Net-Zero (Equity) (N = 9.1 M)")) +
  # guides(color = guide_legend(override.aes = list(linetype = ))) +
  annotate(geom = "segment", x = 0, y = 0, xend = 0, yend = .3, color = "grey80", size = .2)


fig4 <- ggarrange(ggarrange(fig_4a, NULL, nrow = 1, widths = c(10, 0.5), heights = 4), 
                  ggarrange(fig_4b, NULL, fig_4c, NULL, nrow = 1, widths = c(5, .25, 5, .25), heights = c(4.5)), 
                  nrow = 2, heights = c(4, 4.5), widths = c(10.5))
ggsave(filename = "~/Documents/PSU/2023 Spring/ModelsComparison/Viz_v1/Fig4.png", plot = fig4, 
       width = 10, height = 8.5)

#### Figure 5. Impact of paras ####

dt_fig5a <- data.frame(c = seq(0, 10, by = 0.1) %>% rep(3), 
                       rr = c(exp(beta * seq(0, 10, by = 0.1)), 
                              exp(beta_di_black * seq(0, 10, by = 0.1)), 
                              exp(beta_di_nhwa * seq(0, 10, by = 0.1))), 
                       race = rep(c("All-race (Krewski et al. 2009)", 
                                    "Black (Di et al. 2017)", 
                                    "White (Di et al. 2017)"), each = 101)
)

fig_5a <-
  ggplot(data = dt_fig5a) + 
  geom_path(mapping = aes(x = c, y = rr, color = race), size = 1) + 
  # geom_ribbon(mapping = aes(x = c, ymin = rr_lower, ymax = rr_upper, fill = race), alpha = .8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(size = 18, halign = 0.15), axis.text.x = element_markdown(), axis.text.y = element_markdown(), 
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), 
        legend.text = element_markdown(), 
        legend.box.background = element_blank(), legend.position = c(0.35, 0.75)) +
  labs(y = "", x = "PM<sub>2.5</sub> concentration (&mu;g/m<sup>3</sup>)", color = "", fill = "",
       title = "<b>a) All-race and race-specific <br>relative risks</b>") +
  scale_x_continuous(limits = c(0, 11), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(1, 1.25), expand = c(0, 0)) +
  scale_color_manual(values = c("grey70", "grey20", "red3")) + 
  scale_fill_manual(values = c("grey70", "grey20", "red3")) +
  annotate("text", x = 9, y = 1.165, label = "Black", color = "grey20") +  
  annotate("text", x = 8, y = 1.06, label = "White", color = "red3") +
  annotate("text", x = 8.5, y = 1.04, label = "All-race", color = "grey60") 

dt_fig5b <- data.frame(
  scenario = c(rep(c("Uniform-Uniform NZ", "Different-Uniform NZ", "Different-Different NZ"), 2), 
               rep(c("Uniform-Uniform Equity", "Different-Uniform Equity", "Different-Different Equity"), 2)),
  ses = rep(c("White", "Non-White"), each = 3) %>% rep(2), 
  # dr = c(369, 369 * 1.02, 386 * 1.02, 359, 359 * 0.93, 777 * 0.93), # deaths per million
  dr = c(249, 261, 273, 267, 292, 917,  
         219, 230, 240, 216, 236, 747), # deaths per million
  dr_lower = c(168, 176, 260, 181, 197, 880,  
               148, 155, 229, 146, 160, 716), 
  dr_upper = c(327, 343, 282, 351, 384, 949,  
               288, 302, 248, 285, 311, 773)
)

fig_5b <-
  ggplot() + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(size = 18, halign = 0.3), axis.text.x = element_markdown(), axis.text.y = element_markdown(), 
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), legend.text = element_markdown(), 
        legend.box.background = element_blank(), legend.position = c(0.25, 0.65)) +
  labs(y = "", x = "Attributable fraction", color = "", fill = "",
       title = "<b>b) Attributable fractions among <br>the exposed population</b>") +
  scale_x_continuous(limits = c(0, 0.12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 4.1), expand = c(0, 0), 
                     breaks = rep(0:3, each = 2) + rep(c(0.4, 0.6), 4),
                     labels = c("Race-specific RR, Black", "Race-specific RR, White", 
                                "Uniform RR, Black", "Uniform RR, White") %>% rep(2)) + 
  geom_richtext(label = "<b>2050 Net-Zero</b>", aes(x = .023, y = 3.9), label.color = NA, size = 5) +
  geom_richtext(label = "<b>2050 Net-Zero (Equity)</b>", aes(x = .032, y = 1.9), label.color = NA, size = 5) +
  geom_richtext(label = "(3.56 &mu;g/m<sup>3</sup>)", aes(x = 0.050, y = 0.35), size = 4, label.color = NA) +
  geom_richtext(label = "(3.38 &mu;g/m<sup>3</sup>)", aes(x = 0.035, y = 0.65), size = 4, label.color = NA) +
  geom_richtext(label = "(3.56 &mu;g/m<sup>3</sup>)", aes(x = 0.045, y = 1.35), size = 4, label.color = NA) +
  geom_richtext(label = "(3.38 &mu;g/m<sup>3</sup>)", aes(x = 0.040, y = 1.65), size = 4, label.color = NA) +
  geom_richtext(label = "(4.52 &mu;g/m<sup>3</sup>)", aes(x = 0.067, y = 2.35), size = 4, label.color = NA) +
  geom_richtext(label = "(3.93 &mu;g/m<sup>3</sup>)", aes(x = 0.039, y = 2.65), size = 4, label.color = NA) +
  geom_richtext(label = "(4.52 &mu;g/m<sup>3</sup>)", aes(x = 0.050, y = 3.35), size = 4, label.color = NA) +
  geom_richtext(label = "(3.93 &mu;g/m<sup>3</sup>)", aes(x = 0.045, y = 3.65), size = 4, label.color = NA) +
  annotate("segment", x = 0, xend = 0.12, y = 2.1, yend = 2.1, color = "grey70") +
  annotate("point", x = 1 - exp(0.00583 * 4.52)^-1, y = 3.4, color = "grey70", size = 2) +
  annotate("segment", x = 1 - exp(0.00392 * 4.52)^-1, xend = 1 - exp(0.00770 * 4.52)^-1, y = 3.4, yend = 3.4, color = "grey70", size = 1) +
  annotate("point", x = 1 - exp(0.00583 * 3.93)^-1, y = 3.6, color = "grey70", size = 2) +
  annotate("segment", x = 1 - exp(0.00392 * 3.93)^-1, xend = 1 - exp(0.00770 * 3.93)^-1, y = 3.6, yend = 3.6, color = "grey70", size = 1) +
  annotate("point", x = 1 - exp(0.0189 * 4.52)^-1, y = 2.4, color = "grey20", size = 2) +
  annotate("segment", x = 1 - exp(0.0181 * 4.52)^-1, xend = 1 - exp(0.0196 * 4.52)^-1, y = 2.4, yend = 2.4, color = "grey20", size = 1) +
  annotate("point", x = 1 - exp(0.0061 * 3.93)^-1, y = 2.6, color = "red3", size = 2) +
  annotate("segment", x = 1 - exp(0.0058 * 3.93)^-1, xend = 1 - exp(0.0063 * 3.93)^-1, y = 2.6, yend = 2.6, color = "red3", size = 1) +
  annotate("point", x = 1 - exp(0.00583 * 3.56)^-1, y = 1.4, color = "grey70", size = 2) +
  annotate("segment", x = 1 - exp(0.00392 * 3.56)^-1, xend = 1 - exp(0.00770 * 3.56)^-1, y = 1.4, yend = 1.4, color = "grey70", size = 1) +
  annotate("point", x = 1 - exp(0.00583 * 3.38)^-1, y = 1.6, color = "grey70", size = 2) +
  annotate("segment", x = 1 - exp(0.00392 * 3.38)^-1, xend = 1 - exp(0.00770 * 3.38)^-1, y = 1.6, yend = 1.6, color = "grey70", size = 1) +
  annotate("point", x = 1 - exp(0.0189 * 3.56)^-1, y = 0.4, color = "grey20", size = 2) +
  annotate("segment", x = 1 - exp(0.0181 * 3.56)^-1, xend = 1 - exp(0.0196 * 3.56)^-1, y = 0.4, yend = 0.4, color = "grey20", size = 1) +
  annotate("point", x = 1 - exp(0.0061 * 3.38)^-1, y = 0.6, color = "red3", size = 2) +
  annotate("segment", x = 1 - exp(0.0058 * 3.38)^-1, xend = 1 - exp(0.0063 * 3.38)^-1, y = 0.6, yend = 0.6, color = "red3", size = 1) 


dt_fig5 <- data.frame(
  scenario = c(rep(c("Uniform-Uniform NZ", "Different-Uniform NZ", "Different-Different NZ"), 2), 
               rep(c("Uniform-Uniform Equity", "Different-Uniform Equity", "Different-Different Equity"), 2)),
  ses = rep(c("White", "Non-White"), each = 3) %>% rep(2), 
  # dr = c(369, 369 * 1.02, 386 * 1.02, 359, 359 * 0.93, 777 * 0.93), # deaths per million
  dr = c(249, 261, 273, 267, 292, 917,  
         219, 230, 240, 216, 236, 747), # deaths per million
  dr_lower = c(168, 176, 260, 181, 197, 880,  
               148, 155, 229, 146, 160, 716), 
  dr_upper = c(327, 343, 282, 351, 384, 949,  
               288, 302, 248, 285, 311, 773)
)
# dt_fig5$scenario <- factor(dt_fig5$scenario, 
#                            levels = rev(c("Uniform-Uniform NZ", "Different-Uniform NZ", "Different-Different NZ", 
#                                           "Uniform-Uniform Equity", "Different-Uniform Equity", "Different-Different Equity")))
dt_fig5$scenario <- c(rep(7:5, 2), rep(3:1, 2))
dt_fig5$ses <- factor(dt_fig5$ses, levels = c("Non-White", "White"))

fig_5c <- ggplot(data = dt_fig5) + 
  geom_pointrange(mapping = aes(y = scenario, x = dr, xmin = dr_lower, xmax = dr_upper, color = ses), 
                  position = position_dodge(width = .5), linewidth = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 16), axis.ticks.x = element_blank(), 
        plot.title = element_markdown(size = 18), axis.text.x = element_markdown(), axis.text.y = element_markdown(), 
        axis.title.x = element_markdown(), axis.title.y = element_markdown(),
        panel.background = element_blank(), legend.title = element_markdown(), legend.text = element_markdown(), 
        legend.box.background = element_blank(), legend.position = c(0.75, 0.85)) +
  labs(y = "", x = "Annual PM<sub>2.5</sub>-attributable death rate (deaths per million)", color = "Racial group", 
       title = "<b>c) Annual PM<sub>2.5</sub>-attributable death rate</b>") +
  scale_y_continuous(limits = c(0, 8.5), expand = c(0, 0),
                     breaks = c(1:3, 5:7),
                     labels = c("Race-specific y<sub>0</sub><br>Race-specific RR",
                                "Race-specific y<sub>0</sub><br>Uniform RR",
                                "Race-uniform y<sub>0</sub><br>Uniform RR") %>% rep(2)) +
  scale_x_continuous(limits = c(0, 1000), expand = c(0, 0)) +
  scale_color_manual(values = c("black", "red3"), labels = c("Black", "White")) + 
  annotate(geom = "segment", x = 0, xend = 1000, y = 4.5, yend = 4.5, color = "grey70") + 
  annotate(geom = "text", x = 150, y = 7.78, label = "2050 Net-Zero", size = 5.5, fontface = "bold") +
  annotate(geom = "text", x = 190, y = 3.85, label = "2050 Net-Zero (Equity)", size = 5.5, fontface = "bold") 

fig5 <- gridExtra::grid.arrange(
  gridExtra::grid.arrange(fig_5a, fig_5b, NULL, ncol = 3, widths = c(5, 7, .15), heights = 5),
  gridExtra::grid.arrange(fig_5c, NULL, ncol = 2, widths = c(12, .15), heights = 5),
  nrow = 2, widths = 12.15, heights = c(5, 5)
)
ggsave(filename = "~/Documents/PSU/2023 Spring/ModelsComparison/Viz_v1/Fig5.png", plot = fig5, 
       width = 12.15, height = 10)




