# Code Purpose: Pre-Processing Work for the Water Consumption ABM in Las Vegas
# Code By: Renee Obringer
# Code Run: 25 October 2023
 
# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load the hydroclimatic input data
# DATA PRE-PROCESSING: spatiotemporal aggregation, unit conversion, etc.
# WATER CONSUMPTION CALCULATION: using Wang et al. 2021, calculate water consumption for all survey respondents
# FIT DISTRIBUTIONS: fit distributions for all hydroclimatic variables and water consumption
# FIGURES AND TABLES: code for plotting figures and creating tables 

rm(list=ls())
options(scipen = 999)

# libraries
library(tidyverse)
library(readxl)        # for reading in xlsx files
library(hydroGOF)      # for NRMSE calculations
library(lubridate)     # for working with dates
library(reshape2)      # for data pre-processing
library(fitdistrplus)  # for fitting distributions
library(markovchain)   # for fitting distributions
library(dplyr)         # for data processing
library(ggplot2)       # for plotting
library(readr)         # for writing csv files

# set file path
# NOTE: set this path to the folder on your personal machine which contains the cloned repository
# for example: path <- '/Users/Obringer/Downloads/WaterConservation_ClimateChange_ABM'

path <- '   '

# set directories
maindir <- path                                                             # main directory
datadir <- paste(path, '/observedData/lasvegashydrodata', sep = '')         # directory for the input data
rdatadir <- paste(path, '/supplementalcalculations/rdatafiles/', sep = '')  # directory for storing rdata files

# OPTIONAL: create an output directory
outputdir <- paste(path, '/outputdir/', sep = '')                           # directory for any non-rdata output files (e.g., csv, pdf, etc.)
dir.create(outputdir)

########## LOAD DATA ################

setwd(datadir)

# storage (acre-feet)
storage <- read.csv('dailystorage_1937-2023.csv')

# inflow (acre-feet)
inflow <- read.csv('lakepowell_dailyreleasevolume_1963-2023.csv')

# outflow (acre-feet)
outflow <- read.csv('dailyreleasevolume_1934-2023.csv')

# evaporation (kg/m^2)
setwd(paste(datadir, '/evaporation', sep = ''))
evap1 <- read.csv('evap2007.csv'); evap2 <- read.csv('evap2008.csv') 
evap3 <- read.csv('evap2009.csv'); evap4 <- read.csv('evap2010.csv') 
evap5 <- read.csv('evap2011.csv'); evap6 <- read.csv('evap2012.csv') 
evap7 <- read.csv('evap2013.csv'); evap8 <- read.csv('evap2014.csv') 
evap9 <- read.csv('evap2015.csv'); evap10 <- read.csv('evap2016.csv') 
evap11 <- read.csv('evap2017.csv'); evap12 <- read.csv('evap2018.csv') 

evaporation <- rbind(evap1, evap2, evap3, evap4, evap5, evap6, evap7, evap8,
                     evap9, evap10, evap11, evap12)

# precipitation (in)
setwd(paste(datadir, '/weatherstations', sep = ''))
precip1 <- read.csv('Las Vegas_Air Terminal 2007-2016.csv')
precip2 <- read.csv('Las Vegas_Henderson Airport 2007-2016.csv')
precip3 <- read.csv('Las Vegas_Indian Springs 2007-2016.csv')
precip4 <- read.csv('Las Vegas_Indian Springs+Air Terminal+Henderson Airport 2017-2019.csv')
precip5 <- read.csv('Las Vegas_McCarran International Airport 2007-2016.csv')
precip6 <- read.csv('Las Vegas_Nellis AFB 2007-2016.csv')
precip7 <- read.csv('Las Vegas_Nellis AFB+McCarran International Airport 2017-2019.csv')

precipitation <- bind_rows(precip1 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall) %>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric), 
                           precip2 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric),
                           precip3 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric),
                           precip4 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric),
                           precip5 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric),
                           precip6 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric),
                           precip7 %>% dplyr::select(DATE, HOURLYPrecip, DAILYPrecip, DAILYSnowfall)%>% 
                             mutate_at(c('HOURLYPrecip', 'DAILYPrecip', 'DAILYSnowfall'), as.numeric))

# water use (mgal)
wateruse <- read.csv(paste(waterdir, 'wateruse.csv', sep = ''))

# save rdata
setwd(rdatadir)
save.image('lasvegas_rawdata.rdata')

########## DATA PRE-PROCESSING ###############

# OPTIONAL: load rdata with raw data files
setwd(rdatadir)
load('lasvegas_rawdata.rdata')

# DESCRIPTION:
# This section of code is for pre-processing the hydroclimatic data. Each variable 
# has different treatments. All  variables are spatially aggregated, if necessary, 
# and the units are converted from US customary to metric. The inflow and outflow 
# are calculated for the system. Finally, all variables were temporally aggregated 
# to monthly values to match the water use data.


# STORAGE PRE-PROCESSING

# reformat date
storage$datetime <- as.Date(storage$datetime, '%m/%d/%y')

# fix year problem post-hoc
year(storage$datetime[storage$datetime >= '2024-01-01']) <- year(storage$datetime[storage$datetime >= '2024-01-01']) - 100

# extract dates of interest
totalstorage <- storage[between(storage$datetime, ymd('2007-01-01'), ymd('2018-12-31')),]
  
# convert units
totalstorage$storage_m3 <- totalstorage$storage_acft*1233.48

# temporal aggregation
totalstorage <- totalstorage %>% group_by(month = floor_date(datetime, "month")) %>%
  summarize(storage_acft = last(storage_acft), storage_m3 = last(storage_m3))

# PRECIPITATION PRE-PROCESSING

# calculate daily values
totalprecip <- precipitation[,c(1,3,4)]
totalprecip <- totalprecip %>% mutate_all(na_if,"")
totalprecip$DAILYPrecip <- as.numeric(totalprecip$DAILYPrecip)
totalprecip$DAILYSnowfall <- as.numeric(totalprecip$DAILYSnowfall)
totalprecip[is.na(totalprecip)] <- 0

# combine precipitation and snowfall
totalprecip$total_in <- totalprecip$DAILYPrecip + totalprecip$DAILYSnowfall

# convert units 
totalprecip$total_m <- totalprecip$total_in*0.0254

# calculate total over total surface area
surfacearea <- 247.1*2.59e+6 # mi^2 --> m^2

totalprecip$total_m3 <- totalprecip$total_m*surfacearea

# convert date column to date type
totalprecip$DATE <- mdy_hm(totalprecip$DATE)

# spatial aggregation
totalprecip <- totalprecip %>% group_by(month = floor_date(DATE, "day")) %>%
  summarize(total_m3 = mean(total_m3, na.rm = T))

# temporal aggregation 
totalprecip <- totalprecip %>% group_by(month = floor_date(month, "month")) %>%
  summarize(total_m3 = sum(total_m3, na.rm = T))
totalprecip <- totalprecip[1:144,]

# WATER USE PRE-PROCESSING

# combine with date column from another variable
totalwateruse <- data.frame(date = totalprecip$month, wateruse_mg = wateruse$MG)

# convert units
totalwateruse$wateruse_m3 <- totalwateruse$wateruse_mg*1000000*0.00378541

# consider only 90% of the water used (this is the amount drawn from Lake Mead)
totalwateruse$lakemeadwateruse_m3 <- 0.9*totalwateruse$wateruse_m3

# STREAMFLOW PRE-PROCESSING

# reformat date
inflow$datetime <- as.Date(inflow$datetime, '%m/%d/%y')
outflow$datetime <- as.Date(outflow$datetime, '%m/%d/%y')

# fix year problem post-hoc
year(inflow$datetime[inflow$datetime >= '2024-01-01']) <- year(inflow$datetime[inflow$datetime >= '2024-01-01']) - 100
year(outflow$datetime[outflow$datetime >= '2024-01-01']) <- year(outflow$datetime[outflow$datetime >= '2024-01-01']) - 100

# extract dates of interest
totalinflow <- inflow[between(inflow$datetime, ymd('2007-01-01'), ymd('2018-12-31')),]
totaloutflow <- outflow[between(outflow$datetime, ymd('2007-01-01'), ymd('2018-12-31')),]

# convert units
totalinflow$inflow_m3 <- totalinflow$release.volume_acft*1233.48
totaloutflow$outflow_m3 <- totaloutflow$release.volume_acft*1233.48

# temporal aggregation
totalinflow <- totalinflow %>% group_by(month = floor_date(datetime, "month")) %>%
  summarize(release.volume_acft = sum(release.volume_acft, na.rm = T), 
            inflow_m3 = sum(inflow_m3, na.rm = T))

totaloutflow <- totaloutflow %>% group_by(month = floor_date(datetime, "month")) %>%
  summarize(release.volume_acft = sum(release.volume_acft, na.rm = T), 
            outflow_m3 = sum(outflow_m3, na.rm = T))

# add the water returned from Las Vegas as inflow (40% of water used goes back to Lake Mead)
totalinflow$totalinflow_m3 <- totalinflow$inflow_m3 + (0.4*totalwateruse$wateruse_m3)

# EVAPORATION PRE-PROCESSING

totalevap <- evaporation[,c(3,2)]

# calculate total over total surface area
totalevap$evap_m3 <- totalevap$evaporation_m*surfacearea

# convert date column to date type
totalevap$date <- ymd(totalevap$date)

# temporal aggregation
totalevap <- totalevap %>% group_by(month = floor_date(date, "month")) %>%
  summarize(evap_m3 = sum(evap_m3, na.rm = T))

# COMBINE ALL DATA
lasvegasmonthlydata <- data.frame(date = totalstorage$month, storage = totalstorage$storage_m3, 
                                  inflow = totalinflow$totalinflow_m3, outflow = totaloutflow$outflow_m3, 
                                  precip = totalprecip$total_m3, evap = totalevap$evap_m3)

# save rdata file
setwd(rdatadir)
save.image('lasvegas_processedhydrodata.RDATA')

########## WATER CONSUMPTION CALCULATION ####################

# OPTIONAL: load rdata file
setwd(rdatadir)
load('lasvegas_processedhydrodata.rdata')

# DESCRIPTION:
# Calculating the water consumption for all the survey respondents using the 
# equation developed by Wang et al. 2021 (DOI: 10.1016/j.resconrec.2021.105520)

# find maximum dry bulb temperature (degC)
drybulbtemp <- bind_rows(precip1 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC), 
                         precip2 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC),
                         precip3 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC),
                         precip4 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC),
                         precip5 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC),
                         precip6 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC),
                         precip7 %>% dplyr::select(DATE, HOURLYDRYBULBTEMPC))
drybulbtemp$HOURLYDRYBULBTEMPC <- as.numeric(drybulbtemp$HOURLYDRYBULBTEMPC)
drybulbtemp <- na.omit(drybulbtemp)
drybulbtemp$DATE <- mdy_hm(drybulbtemp$DATE)
drybulbtemp <- drybulbtemp %>% group_by(month = floor_date(DATE, "day")) %>%
  summarize(maxdbt = max(HOURLYDRYBULBTEMPC, na.rm = T))
drybulbtemp <- drybulbtemp[1:4383,]

# find daily precipitation (mm)
dailyprecip <- precipitation[,c(1,3,4)]
dailyprecip <- dailyprecip %>% mutate_all(na_if,"")
dailyprecip$DAILYPrecip <- as.numeric(dailyprecip$DAILYPrecip)
dailyprecip$DAILYSnowfall <- as.numeric(dailyprecip$DAILYSnowfall)
dailyprecip$DAILYPrecip[which(is.na(dailyprecip$DAILYPrecip) == TRUE)] <- 0
dailyprecip$DAILYSnowfall[which(is.na(dailyprecip$DAILYSnowfall) == TRUE)] <- 0
dailyprecip$total_in <- dailyprecip$DAILYPrecip + dailyprecip$DAILYSnowfall
dailyprecip$total_mm <- dailyprecip$total_in*25.4
dailyprecip$DATE <- mdy_hm(dailyprecip$DATE)
dailyprecip <- dailyprecip %>% group_by(month = floor_date(DATE, "day")) %>%
  summarize(total_mm = mean(total_mm, na.rm = T))
dailyprecip <- dailyprecip[1:4383,]

# find daily evaporation (mm)
dailyevap <- data.frame(date = evaporation$date, evap_mm = evaporation$evaporation_m*1000)

# calculate antecedent precipitation index
api <- c()
api[1] <- 0.95*dailyprecip$total_mm[1]
for (i in 2:length(dailyprecip$total_mm)) {
  # decay value = 0.95, as recommended by Hill et al. 2014
  api[i] <- 0.95*api[i-1] + dailyprecip$total_mm[i]
}

# calculate the number of continuous days without rain
nd <- c()
nd[1] <- 0
for (i in 2:length(dailyprecip$total_mm)) {
  if (dailyprecip$total_mm[i] == 0) {
    nd[i] <- nd[i-1] + 1
  } else {
    nd[i] <- 0
  }
}

# find income from https://fred.stlouisfed.org/series/MHINV32003A052NCEN
annualincome <- data.frame(date = seq(as.Date("2007-01-01"), as.Date("2018-01-01"), by = "years"), 
                           income = c(55960, 56691, 53512, 51427, 48343, 49583, 51071, 51241, 
                                      51624, 54455, 57217, 57155))

dailydates <- data.frame(date = seq(as.Date("2007-01-01"), as.Date("2018-12-31"), by = "days"))

income_int <- merge(annualincome, dailydates, by = 'date',by.x='date',by.y='date',all.x=T,all.y=T)
income_int <- na.locf(income_int)
income_int[,3] <- income_int[,2]/12; names(income_int)[c(2,3)] <- c('annualIncome','monthlyIncome')

# find water price; 2010-2018 data from: https://www.circleofblue.org/waterpricing/; assumed 5% increase from 2007-2010
waterprice <- data.frame(date = seq(as.Date("2007-01-01"), as.Date("2018-01-01"), by = "years"),
                         price = c(0.2845, 0.2987, 0.3136, 0.3293, 0.3613, 0.4113, 0.4113, 0.4227, 
                                   0.4347, 0.4743, 0.5099, 0.5320)/0.00378541) # $/gallon to $/m3

price_int <- merge(waterprice, dailydates, by = 'date',by.x='date',by.y='date',all.x=T,all.y=T)
price_int <- na.locf(price_int); names(price_int)[2] <- 'price'

# expand water use to daily
totalwateruse$m3percap <- totalwateruse$lakemeadwateruse_m3/wateruse$population
use_int <- merge(totalwateruse[,c(1,5)], dailydates, by = 'date',by.x='date',by.y='date',all.x=T,all.y=T)
use_int <- na.locf(use_int); names(use_int)[2] <- 'wateruse'
use_int$wateruse <- use_int$wateruse/days_in_month(use_int$date)
use_int <- use_int[!duplicated(use_int), ]

# set up multiple linear regression (following Wang et al. 2021 methods)
mlrdata <- data.frame(date = drybulbtemp$month, wateruse = use_int$wateruse, maxtemp = drybulbtemp$maxdbt, precipitation = dailyprecip$total_mm,
                     evaporation = dailyevap$evap_mm, api = api, drydays = nd, income = income_int$monthlyIncome,
                     price = price_int$price)

mlr_fit <- lm(wateruse ~ maxtemp + precipitation + evaporation + api + drydays + income + price, data = mlrdata)
summary(mlr_fit)
plot(mlr_fit)

# OPTIONAL: save data for input to ABM
clim_se_data <- mlrdata[,-2]
setwd(outputdir)
write.csv(clim_se_data,'LasVegasClimateSocioEconData.csv')

# use Wang et al. 2021 to get water consumption estimates for each respondent

# load data from Obringer and White (2023) in Water Resources Management
setwd(rdatadir)
load('clusteringanalysis.RData')

# separate income data
surveyincome <- as.numeric(surveydata$q18[which(surveydata$sample_group == 3)])

# get specific income from categories (assuming uniform distribution)
for (i in 1:length(surveyincome)) {
  if (is.na(surveyincome[i])) {
    next
  } else if (surveyincome[i] == 1) {
    surveyincome[i] <- runif(1,1000,20000)
  } else if (surveyincome[i] == 2) {
    surveyincome[i] <- runif(1,20001,40000)
  } else if (surveyincome[i] == 3) {
    surveyincome[i] <- runif(1,40001,60000)
  } else if (surveyincome[i] == 4) {
    surveyincome[i] <- runif(1,60001,80000)
  } else if (surveyincome[i] == 5) {
    surveyincome[i] <- runif(1,80001,100000)
  } else if (surveyincome[i] == 6) {
    surveyincome[i] <- runif(1,100001,300000)
  }
}

# extract monthly data in 2017
data2017 <- mlrdata[which(mlrdata$date >= '2017-01-01' & mlrdata$date <= '2017-12-31'),]
data2017[,10] <- month(data2017$date)
mondata2017 <- aggregate(data2017, by = list(data2017$V10), FUN = mean)
mondata2017 <- mondata2017[,-c(1:2)]; names(mondata2017)[9] <- 'month'

# merge survey income data with weather data for each month in 2017
sdata <- list()
for (i in 1:12) {
  sdata[[i]] <- data.frame(mondata2017 %>% slice(i, each = length(surveyincome)),surveyincome/12)
  sdata[[i]] <- sdata[[i]][,-c(1,7,9)]
  names(sdata[[i]])[c(7)] <- c('income')
}

# predict water use for each respondent in each month in 2017
waterusePred <- list()
for (i in 1:12) {
  waterusePred[[i]] <- predict(mlr_fit, newdata = sdata[[i]])
  waterusePred[[i]] <- waterusePred[[i]]*30
}

# replace negatives with zeroes
waterusePred <- lapply(waterusePred, function(x) {x[x < 0] <- 0; x})

# get means by archetype by month
avgwaterusePC <- list()
for (i in 1:12) {
  cdata <- data.frame(wateruse = waterusePred[[i]], node = model[[5]]$unit.classif[which(surveydata$sample_group == 3)])
  avgwateruse <- aggregate(cdata$wateruse, by = list(cdata$node), FUN = mean, na.rm=TRUE, na.action=NULL)
  avgwateruse <- data.frame(avgwateruse, cluster = clusters[[5]])
  avgwaterusePC[[i]] <- aggregate(avgwateruse$x, by = list(avgwateruse$cluster), FUN = mean)
}

setwd(rdatadir)
save.image('lasvegas_waterconsumptioncalc.rdata')

########## FIT DISTRIBUTIONS ####################

# OPTIONAL: load rdata file
setwd(rdatadir)
load('lasvegas_processedhydrodata.rdata')
load('lasvegas_waterconsumptioncalc.rdata')

# rename variables and remove/replace NA values
S <- totalstorage$storage_m3; P <- totalprecip$total_m3; Qin <- totalinflow$totalinflow_m3 
W <- totalwateruse$lakemeadwateruse_m3; E <- totalevap$evap_m3; Qout <- totaloutflow$outflow_m3

# unaccounted for losses 
L <- c()
for (i in 2:144) {
  L[i] <- S[i-1] - S[i] + P[i] + Qin[i] - W[i] - E[i] - Qout[i]
}

# check water balance
Smod<- c(); Smod[1] <- S[1]
for (i in 2:144) {
  Smod[i] <- Smod[i-1] + P[i] + Qin[i] - W[i] - E[i] - L[i] - Qout[i]
}

nrmse(Smod, S[1:144]) # check accuracy of water balance

alldata <- data.frame(S, P, Qin, W, E, Qout, L)

# save data for input to ABM
setwd(outputdir)
write.csv(alldata,'LasVegasWaterBalData_original.csv')

# Get distributions of data

# WATER USE

# get distributions of total water use by archetype by month
distwaterusePC <- list()
for (i in 1:12) {
  cdata <- data.frame(wateruse = waterusePred[[i]], node = model[[5]]$unit.classif[which(surveydata$sample_group == 3)], cluster = NA)
  for (j in 1:30) {
    cdata[which(cdata$node == j), 3] <- clusters[[5]][j]
  }
  waterusedist <- list()
  for (k in 1:7) {
    waterusedist[[k]] <- cdata[which(cdata$cluster == k),]
    waterusedist[[k]]$wateruse[which(waterusedist[[k]]$wateruse < 0)] <- 0
  }
  distwaterusePC[[i]] <- waterusedist
}

# fit gamma distributions
gammafit <- list()
seasons <- list(c(12, 1, 2, 3), c(4, 5, 10, 11), c(6, 7, 8, 9))
for (i in 1:3) {
  gammafit1 <- list()
  for (j in 1:7) {
    wdata <- c(na.omit(distwaterusePC[[seasons[[i]][1]]][[j]]$wateruse), 
               na.omit(distwaterusePC[[seasons[[i]][2]]][[j]]$wateruse), 
               na.omit(distwaterusePC[[seasons[[i]][3]]][[j]]$wateruse),
               na.omit(distwaterusePC[[seasons[[i]][4]]][[j]]$wateruse))
    gammafit1[[j]] <- fitdist(as.numeric(wdata), "gamma", method = 'mme')
  }
  gammafit[[i]] <- gammafit1
}

# save rdata file
setwd(rdatadir)
save.image('lasvegas_distributions.rdata')

########## FIGURES AND TABLES ##################

# read in results from ABM model
setwd(maindir)
abmresults <- read.csv('experimentresults.csv', skip = 6, stringsAsFactors=FALSE)

# read in actual data
actualdata <- read.csv(paste(maindir, '/inputData/PhoenixWaterBalData.csv', sep = ''))

# data pre-processing

baseline <- abmresults[-c(1:9),which(abmresults[1,] == 'base')]              # baseline scenario: observed proportion of archetypes
partialpart <- abmresults[-c(1:9),which(abmresults[1,] == 'partial-part')]   # partially participatory scenario: 50% of archetype 3 --> 5
particpatory <- abmresults[-c(1:9),which(abmresults[1,] == 'part')]          # participatory scenario: 100% of archetype 3 --> 5
individualistic <- abmresults[-c(1:9),which(abmresults[1,] == 'indiv')]      # individualistic scenario: 100% of archetype 6 --> 2
concerned <- abmresults[-c(1:9),which(abmresults[1,] == 'concern')]          # concerned scenario: 100% of archetype 7 --> 4
combined <- abmresults[-c(1:9),which(abmresults[1,] == 'all-changes')]       # combined scenario: all of the above changes (100% of 3 --> 5, 6 --> 2, and 7 --> 4)

# FIGURE 1

# calculate average
baseline_matrix <- matrix(as.numeric(unlist(baseline)), nrow = 3500, byrow = F)
average <- rowMeans(baseline_matrix)

# convert matrix to data frame
baseline_df <- as.data.frame(baseline_matrix)

# rename columns in baseline_df
names(baseline_df) = gsub(pattern = "V*", replacement = "", x = names(baseline_df))

# prep plotting data
timestep <- c(1:3500)
alldata_baseline <- cbind(baseline_df, average, timestep, actualdata[1:3500,1])

# rearrange data to long form for plotting
plotdata <- data.frame(melt(alldata_baseline, id.vars = 'timestep', variable.name = 'ModelRun', value.name = 'Storage'))

# add in variable for distinguishing average from the rest of the runs
typerun <- c(rep('model', 350000), rep('Average', 3500), rep('Actual', 3500))
plotdata <- cbind(plotdata, typerun)

# plot data + save as a pdf
setwd(outputdir)
pdf('figure1.pdf', height = 5, width = 12.5)
ggplot(plotdata) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = typerun, size = typerun)) +
  scale_color_manual(values = c('blue','black','#D3D3D3')) + theme_light(base_size = 16) + guides(color = 'none', size = 'none') +
  xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_size_manual(values = c(1,1,3))
dev.off()

# FIGURE 2

# convert to matrix
partialpart_matrix <- matrix(as.numeric(unlist(partialpart)), nrow = 3500, byrow = F)
particpatory_matrix <- matrix(as.numeric(unlist(particpatory)), nrow = 3500, byrow = F)
individualistic_matrix <- matrix(as.numeric(unlist(individualistic)), nrow = 3500, byrow = F)
concerned_matrix <- matrix(as.numeric(unlist(concerned)), nrow = 3500, byrow = F)
combined_matrix <- matrix(as.numeric(unlist(combined)), nrow = 3500, byrow = F)

# calculate average
partialpart_average <- rowMeans(partialpart_matrix)
particpatory_average <- rowMeans(particpatory_matrix)
individualistic_average <- rowMeans(individualistic_matrix)
concerned_average <- rowMeans(concerned_matrix)
combined_average <- rowMeans(combined_matrix)

# convert matrix to data frame
partialpart_df <- as.data.frame(partialpart_matrix)
particpatory_df <- as.data.frame(particpatory_matrix)
individualistic_df <- as.data.frame(individualistic_matrix)
concerned_df <- as.data.frame(concerned_matrix)
combined_df <- as.data.frame(combined_matrix)

# rename columns
names(partialpart_df) = gsub(pattern = "V*", replacement = "", x = names(partialpart_df))
names(particpatory_df) = gsub(pattern = "V*", replacement = "", x = names(particpatory_df))
names(individualistic_df) = gsub(pattern = "V*", replacement = "", x = names(individualistic_df))
names(concerned_df) = gsub(pattern = "V*", replacement = "", x = names(concerned_df))
names(combined_df) = gsub(pattern = "V*", replacement = "", x = names(combined_df))

# prep plotting data
timestep <- c(1:3500)
alldata_modelruns <- rbind(cbind(baseline_df, timestep),
                           cbind(partialpart_df, timestep),
                           cbind(particpatory_df, timestep),
                           cbind(individualistic_df, timestep),
                           cbind(concerned_df, timestep),
                           cbind(combined_df, timestep))
scenarios <- c(rep('1baseline', 3500), rep('2partialpart', 3500), rep('3part', 3500), rep('4indiv', 3500), rep('5concerned', 3500), rep('6allchanges', 3500))
#scenarios <- c(rep('Baseline', 3500), rep('Partial Participation', 3500), rep('Full Participation', 3500), rep('Individualistic', 3500), rep('Concerned', 3500), rep('All Changes', 3500))
alldata_modelruns <- cbind(alldata_modelruns, scenarios)

alldata_averages <- data.frame(average, partialpart_average, particpatory_average, individualistic_average, concerned_average, combined_average, timestep)

# rearrange data to long form for plotting
plotdata_modelruns <- data.frame(melt(alldata_modelruns, id.vars = c('timestep','scenarios'), variable.name = 'ModelRun', value.name = 'Storage'))
plotdata_averages <- data.frame(melt(alldata_averages, id.vars = c('timestep'), variable.name = 'ModelRun', value.name = 'Storage'))

plotdata_averages <- cbind(plotdata_averages, scenarios)

# other data for labeling plots
scenario_names <- c('1baseline'="Baseline", '2partialpart'="Partial Participation",
  '3part'="Full Participation", '4indiv'="Individualistic", '5concerned' = "Concerned", '6allchanges'="All Changes")

# plot model runs data + save as a pdf
setwd(outputdir)
pdf('figure2a.pdf', height = 6, width = 12.5)
ggplot(plotdata_modelruns) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = scenarios)) +
  scale_color_manual(values = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f')) + theme_light(base_size = 16) + 
  xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~ scenarios, labeller=as_labeller(scenario_names)) + guides(color = 'none')
dev.off()

# plot average data + save as a pdf
setwd(outputdir)
pdf('figure2b.pdf', height = 4.5, width = 12.5)
ggplot(plotdata_averages) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = scenarios), size = 1) +
  scale_color_manual('Scenario', values = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f'), 
                     labels = c('Baseline','Partial Participation', 'Full Participation', 'Individualistic','Concerned','All Changes')) + 
  theme_light(base_size = 16) + xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  theme(legend.position = "bottom")
dev.off()

setwd(outputdir)
pdf('figure2b_box.pdf', height = 4.5, width = 12.5)
ggplot(plotdata_averages) + geom_boxplot(aes(x = scenarios, y = Storage/1000000000, group = ModelRun), fill = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f')) +
  theme_light(base_size = 16) + xlab('Scenario') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_x_discrete(labels = c('Baseline','Partial Participation', 'Full Participation', 'Individualistic','Concerned','All Changes'))
dev.off()


# TABLE 2

# calculate measures of model performance 

nrmse_calc <- c()
r2_calc <- c()

for (i in 1:100) {
  # NRSME
  nrmse_calc[i] <- nrmse(as.numeric(baseline[,i]), actualdata[1:3500,1], norm = 'maxmin') 
  
  # R^2
  r2_calc[i] <-  gof(as.numeric(baseline[,i]), actualdata[1:3500,1])[17]
}

# store values
modperf <- data.frame(measure = c('nrmse','r-squared'), minimum = c(min(nrmse_calc), min(r2_calc)), average = c(mean(nrmse_calc), mean(r2_calc)), maximum = c(max(nrmse_calc), max(r2_calc)))

# save csv
setwd(outputdir)
write_csv(modperf, 'modelperformance.csv')

# TABLE 3

# store measures of interest (min/mean/max)
scenario_results <- data.frame(scenario = c('baseline', 'partial_part', 'part', 'indiv', 'concerned', 'combined'), 
                               minimum = c(min(baseline_df), min(partialpart_df), min(particpatory_df), min(individualistic_df), min(concerned_df), min(combined_df)),
                               average = c(mean(as.matrix(baseline_df)), mean(as.matrix(partialpart_df)), mean(as.matrix(particpatory_df)), mean(as.matrix(individualistic_df)),
                                           mean(as.matrix(concerned_df)), mean(as.matrix(combined_df))),
                               maximum = c(max(baseline_df), max(partialpart_df), max(particpatory_df), max(individualistic_df), max(concerned_df), max(combined_df)))

# save csv
setwd(outputdir)
write_csv(scenario_results, 'scenarioresults.csv')

# statistical tests

# get average for each scenario
all_averages <- data.frame(cbind(average, rowMeans(partialpart_df), rowMeans(particpatory_df), rowMeans(individualistic_df), rowMeans(concerned_df), rowMeans(combined_df)))
names(all_averages) <- c('baseline','partialpart','part','indiv','concerned','allchanges')

# t test: Are the scenarios statistically significantly different from the baseline
for (i in 2:6) {
  print(names(all_averages)[i])
  print(t.test(all_averages[,1],all_averages[,i]))
}

#RESULTS: 
# partialpart --> p-value = 2.889e-05 < 0.05 therefore reject the null, difference in means is NOT 0
# part --> p-value < 2.2e-16 < 0.05 therefore reject the null, difference in means is NOT 0
# indiv --> p-value = 0.04275 < 0.05 therefore reject the null, difference in means is NOT 0
# concerned --> p-value = 0.6549 therefore fail to reject the null, difference in means is 0
# allchanges --> p-value < 2.2e-16 < 0.05 therefore reject the null, difference in means is NOT 0








