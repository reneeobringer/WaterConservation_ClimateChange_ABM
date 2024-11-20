# Figures for Water Conservatioon + Climate Change ABM Paper
# Renee Obringer
# 24 September 2024

rm(list=ls())
options(scipen = 999)

# libraries
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(scales)
library(hydroGOF) 
library(lubridate)

# set file path
# NOTE: set this path to the folder on your personal machine which contains the cloned repository
# for example: path <- '/Users/Obringer/Downloads/WaterConservation_ClimateChange_ABM'

path <- '   '

# set directories
maindir <- path                                                             # main directory
datadir <- paste(path, '/observedData', sep = '')                           # directory for the input data
abmresultsdir <- paste(path, '/abmresults', sep = '')                       # directory for ABM results
rdatadir <- paste(path, '/supplementalcalculations/rdatafiles/', sep = '')  # directory for storing rdata files

# OPTIONAL: create an output directory
outputdir <- paste(path, '/outputdir/', sep = '')                           # directory for any non-rdata output files (e.g., csv, pdf, etc.)
dir.create(outputdir)

########## LOAD DATA ############


# read in scenario results csv files
archetypescenarios <- read.csv(paste(abmresultsdir, '/scenarioresults_observations.csv', sep =''), header = F, na.string = "")
climatechangescenarios <- read.csv(paste(abmresultsdir, '/scenarioresults_climatechange.csv', sep =''), header = F, na.string = "")

# read in original data
denverobs <- read.csv(paste(datadir, '/DenverWaterBalData_Original.csv', sep =''), header = T, na.string = "")
lasvegasobs <- read.csv(paste(datadir, '/LasVegasWaterBalData_Original.csv', sep =''), header = T, na.string = "")
phoenixobs <- read.csv(paste(datadir, '/PhoenixWaterBalData_Original.csv', sep =''), header = T, na.string = "")

########## ORGANIZE DATA ############

# get dates
obsdates <- seq.Date(as.Date('2007-01-01'), as.Date('2018-12-31'), by = 'month')
histdates <- seq.Date(as.Date('1976-01-01'), as.Date('2005-12-31'), by = 'month')
futuredates <- seq.Date(as.Date('2006-01-01'), as.Date('2099-12-31'), by = 'month')

# OBSERVATION-BASED DATASETS

# add dates to dataframes
denverobs$X <- obsdates; lasvegasobs$X <- obsdates; phoenixobs$X <- obsdates[1:132]
archetypescenarios$Date <- c.Date(NA, NA, obsdates[1:143]) 

# prepping datasets for melting
newcolumnnames <- paste(archetypescenarios[1,], archetypescenarios[2,], names(archetypescenarios), sep = "_") 
names(archetypescenarios)[1:1800] <- newcolumnnames[1:1800] 
archetypescenarios <- archetypescenarios[-c(1:2),]

# melting
scenarios1 <- melt(archetypescenarios, id = 'Date')

# splitting the "variable" column into multiple descriptors
scenarios1 <- separate(scenarios1, 'variable', into = c('scenario', 'city', 'model run'), sep = '_')

# convert to numeric
scenarios1$value <- as.numeric(scenarios1$value)

# FUTURE CLIMATE-BASED DATASETS

# split historical data from RCP data
hist_ccscenarios <- climatechangescenarios[, which(climatechangescenarios[4,] == 'hist' & climatechangescenarios[6,] == F)]; hist_ccscenarios <- na.omit(hist_ccscenarios)
rcp_ccscenarios <- climatechangescenarios[, which(climatechangescenarios[4,] != 'hist' & climatechangescenarios[6,] == F)]

hist_acscenarios <- climatechangescenarios[, which(climatechangescenarios[4,] == 'hist' & climatechangescenarios[6,] == T)]; hist_acscenarios <- na.omit(hist_acscenarios)
rcp_acscenarios <- climatechangescenarios[, which(climatechangescenarios[4,] != 'hist' & climatechangescenarios[6,] == T)]

# add dates
hist_ccscenarios$Date <- c.Date(NA, NA, NA, NA, NA, NA, histdates[2:360]) 
rcp_ccscenarios$Date <- c.Date(NA, NA, NA, NA, NA, NA, futuredates[2:1123]) 

hist_acscenarios$Date <- c.Date(NA, NA, NA, NA, NA, NA, histdates[2:360]) 
rcp_acscenarios$Date <- c.Date(NA, NA, NA, NA, NA, NA, futuredates[2:1123]) 

# prepping datasets for melting
newcolumnnames <- paste(hist_ccscenarios[1,], hist_ccscenarios[2,], hist_ccscenarios[3,], hist_ccscenarios[4,], hist_ccscenarios[5,], sep = "_") 
names(hist_ccscenarios)[1:120] <- newcolumnnames[1:120] 
hist_ccscenarios <- hist_ccscenarios[-c(1:6),]

newcolumnnames <- paste(rcp_ccscenarios[1,], rcp_ccscenarios[2,], rcp_ccscenarios[3,], rcp_ccscenarios[4,], rcp_ccscenarios[5,], sep = "_") 
names(rcp_ccscenarios)[1:480] <- newcolumnnames[1:480] 
rcp_ccscenarios <- rcp_ccscenarios[-c(1:6),]

newcolumnnames <- paste(hist_acscenarios[1,], hist_acscenarios[2,], hist_acscenarios[3,], hist_acscenarios[4,], hist_acscenarios[5,], sep = "_") 
names(hist_acscenarios)[1:120] <- newcolumnnames[1:120] 
hist_acscenarios <- hist_acscenarios[-c(1:6),]

newcolumnnames <- paste(rcp_acscenarios[1,], rcp_acscenarios[2,], rcp_acscenarios[3,], rcp_acscenarios[4,], rcp_acscenarios[5,], sep = "_") 
names(rcp_acscenarios)[1:480] <- newcolumnnames[1:480] 
rcp_acscenarios <- rcp_acscenarios[-c(1:6),]

# melting
scenarios2_hist <- melt(hist_ccscenarios, id = 'Date')
scenarios2_rcp <- melt(rcp_ccscenarios, id = 'Date')

scenarios3_hist <- melt(hist_acscenarios, id = 'Date')
scenarios3_rcp <- melt(rcp_acscenarios, id = 'Date')

# splitting the "variable" column into multiple descriptors
scenarios2_hist <- separate(scenarios2_hist, 'variable', into = c('scenario', 'city', 'model', 'model run', 'repetition'), sep = '_')
scenarios2_rcp <- separate(scenarios2_rcp, 'variable', into = c('scenario', 'city', 'model', 'model run', 'repetition'), sep = '_')

scenarios3_hist <- separate(scenarios3_hist, 'variable', into = c('scenario', 'city', 'model', 'model run', 'repetition'), sep = '_')
scenarios3_rcp <- separate(scenarios3_rcp, 'variable', into = c('scenario', 'city', 'model', 'model run', 'repetition'), sep = '_')

# merge data
scenarios2 <- rbind(scenarios2_hist, scenarios2_rcp)
scenarios3 <- rbind(scenarios3_hist, scenarios3_rcp)

# convert to numeric
scenarios2$value <- as.numeric(scenarios2$value)
scenarios3$value <- as.numeric(scenarios3$value)

# SAVE DATA
setwd(rdatadir)
save(denverobs, lasvegasobs, phoenixobs, scenarios1, scenarios2, scenarios3, file = 'scenariodata.rdata')

########## FIGURES & TABLES ############

setwd(rdatadir)
load('scenariodata.rdata')

# TABLE 1: NRMSE calculations

# organize data
phoenixbase <- scenarios1[which(scenarios1$city == 'Phoenix' & scenarios1$scenario == 'base'),]
phoenixbase <- dcast(phoenixbase, Date + scenario + city ~ `model run`)

denverbase <- scenarios1[which(scenarios1$city == 'Denver' & scenarios1$scenario == 'base'),]
denverbase <- dcast(denverbase, Date + scenario + city ~ `model run`)

lasvegasbase <- scenarios1[which(scenarios1$city == 'Las Vegas' & scenarios1$scenario == 'base'),]
lasvegasbase <- dcast(lasvegasbase, Date + scenario + city ~ `model run`)

# initialize
phoenixnrmse <- c(); denvernrmse <- c(); lasvegasnrmse <- c()

# loop through each model run
for (i in 1:100) {
  phoenixnrmse[i] <- nrmse(phoenixbase[1:132,i+3], phoenixobs$S, norm = 'maxmin')
  denvernrmse[i] <- nrmse(denverbase[,i+3], denverobs$S[1:143], norm = 'maxmin')
  lasvegasnrmse[i] <- nrmse(lasvegasbase[,i+3], lasvegasobs$S[1:143], norm = 'maxmin')
}

# print values for table
mean(phoenixnrmse); range(phoenixnrmse); mean(denvernrmse); range(denvernrmse); 
mean(lasvegasnrmse); range(lasvegasnrmse)

# FIGURE 1: Observed data for Baseline scenario across three cities (100 run experiment)

plotdata <- scenarios1[which(scenarios1$scenario == 'base'),-2] # extract baseline scenario data

# organize city observations:
pltdata_d <- data.frame('Date' = denverobs$X, 'city' = rep('Denver', 144), 'model run' = rep('observations', 144), 'value' = denverobs$S, check.names = F)
pltdata_lv <- data.frame('Date' = lasvegasobs$X, 'city' = rep('Las Vegas', 144), 'model run' = rep('observations', 144), 'value' = lasvegasobs$S, check.names = F)
pltdata_p <- data.frame('Date' = phoenixobs$X, 'city' = rep('Phoenix', 132), 'model run' = rep('observations', 132), 'value' = phoenixobs$S, check.names = F)

# get averages over 100 model runs
averages <- plotdata %>% group_by(Date, city) %>% summarize(mean(value))
names(averages)[3] <- 'value'
averages$`model run` <- rep('average', 429)
averages <- averages[,c(1,2,4,3)]

# merge everything
plotdata <- rbind(averages, pltdata_d, pltdata_lv, pltdata_p, plotdata)

# create model run type column
plotdata$runtype <- case_when(str_detect(plotdata$`model run`, 'aver') ~ 'average', 
                              str_detect(plotdata$`model run`, 'obs') ~ 'observation', 
                              str_detect(plotdata$`model run`, 'V') ~ 'model run')

setwd(outputdir)
pdf('observed_baseline_allcities.pdf', width = 10.5, height = 10)
ggplot() + geom_line(data = plotdata[which(plotdata$runtype == 'model run'),], aes(x = Date, y = value/1000000000, group = `model run`), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$runtype == 'observation'),], aes(x = Date, y = value/1000000000, group = `model run`), color = 'red', linetype = 'dashed') +
  geom_line(data = plotdata[which(plotdata$runtype == 'average'),], aes(x = Date, y = value/1000000000, group = `model run`), color = 'black') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 16) 
dev.off()

# FIGURE 2: Baseline + climate change scenarios across all three cities (All Scenarios - Ensemble of GCMs)

plotdata <- scenarios2[which(scenarios2$scenario == 'base' & scenarios2$`model run` != 'hist'),-2] # extract baseline scenario data

# get averages over 5 model runs for each GCM
averages <- plotdata %>% group_by(Date, city, `model run`, model) %>% summarize(mean(value))
names(averages)[5] <- 'value'

# average over 5 GCMs
plotdata <- averages %>% group_by(Date, city, `model run`) %>% summarize(mean(value))
names(plotdata)[4] <- 'value'

setwd(outputdir)
pdf('future_baseline_cconly_allcities.pdf', width = 10.5, height = 10)
ggplot(plotdata) + geom_line(aes(x = Date, y = value/1000000000, color = `model run`), size = 1) +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light() + theme(legend.position="bottom", text = element_text(size = 16)) +
  scale_color_manual(name = 'Scenario', labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'), values = c('#fec44f', '#fe9929','#d95f0e','#993404'))
dev.off()

# FIGURE 3: Archetypes + climate change scenarios across all three cities (Box Plots)

# get averages over 5 model runs
averages <- scenarios2 %>% group_by(Date, city, `model run`, model, scenario) %>% summarize(mean(value))
names(averages)[6] <- 'value'

# average over 5 GCMs
plotdata <- averages %>% group_by(Date, city, `model run`, scenario) %>% summarize(mean(value))
names(plotdata)[5] <- 'value'

setwd(outputdir)
pdf('archetypes_cconly_allcities.pdf', width = 10.5, height = 7)
ggplot(plotdata) + geom_boxplot(aes(x = `model run`, y = value/1000000000,  fill = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('Climate Change Scenario') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_x_discrete(labels = c('Historical','RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')) +
  scale_fill_manual(values = c('black','#8da0cb'), name = 'Policy Scenario', labels = c('Baseline', 'Full Participation')) +
  facet_wrap(~city, nrow = 3, scales = 'free')
dev.off()

# FIGURE 4: Baseline + adaptive consumption + climate change scenarios across all three cities (All Scenarios - Ensemble of GCMs)

plotdata <- scenarios3[which(scenarios3$scenario == 'base' & scenarios3$`model run` != 'hist'),-2] # extract baseline scenario data

# get averages over 5 model runs for each GCM
averages <- plotdata %>% group_by(Date, city, `model run`, model) %>% summarize(mean(value))
names(averages)[5] <- 'value'

# average over 5 GCMs
plotdata <- averages %>% group_by(Date, city, `model run`) %>% summarize(mean(value))
names(plotdata)[4] <- 'value'

setwd(outputdir)
pdf('future_baseline_adacons_allcities.pdf', width = 10.5, height = 10)
ggplot(plotdata) + geom_line(aes(x = Date, y = value/1000000000, color = `model run`), size = 1) +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light() + theme(legend.position="bottom", text = element_text(size = 16)) +
  scale_color_manual(name = 'Scenario', labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'), values = c('#fec44f', '#fe9929','#d95f0e','#993404'))
dev.off()

# FIGURE 5: Archetypes + adaptive water consumption + climate change scenarios across all three cities (Box Plots)

# get averages over 5 model runs
averages <- scenarios3 %>% group_by(Date, city, `model run`, model, scenario) %>% summarize(mean(value))
names(averages)[6] <- 'value'

# average over 5 GCMs
plotdata <- averages %>% group_by(Date, city, `model run`, scenario) %>% summarize(mean(value))
names(plotdata)[5] <- 'value'

setwd(outputdir)
pdf('archetypes_adacons_allcities.pdf', width = 10.5, height = 7)
ggplot(plotdata) + geom_boxplot(aes(x = `model run`, y = value/1000000000,  fill = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('Climate Change Scenario') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_x_discrete(labels = c('Historical','RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')) +
  scale_fill_manual(values = c('black','#8da0cb'), name = 'Policy Scenario', labels = c('Baseline', 'Full Participation')) +
  facet_wrap(~city, nrow = 3, scales = 'free')
dev.off()

# FIGURE S1: Archetype scenarios across all three cities 

# get averages over 100 model runs
averages <- scenarios1 %>% group_by(Date, city, scenario) %>% summarize(mean(value))
names(averages)[4] <- 'value'
averages$`model run` <- rep('average', 2574)
averages <- averages[,c(1,3,2,5,4)]

# merge averages w/ actual model runs
plotdata <- rbind(averages, scenarios1)

# update scenario names
plotdata$scenario <- case_when(plotdata$scenario == 'base' ~ '1base', plotdata$scenario == 'partial-part' ~ '2partial-part', 
                               plotdata$scenario == 'part' ~ '3part', plotdata$scenario == 'indiv' ~ '4indiv', 
                               plotdata$scenario == 'concern' ~ '5concern', plotdata$scenario == 'all-changes' ~ '6all-changes')

setwd(outputdir)
pdf('archetypescenarios_allcities.pdf', width = 10.5, height = 7)
ggplot(plotdata[which(plotdata$`model run` == 'average'),]) + geom_boxplot(aes(x = scenario, y = value/1000000000,  fill = scenario)) +
  theme_light(base_size = 16) + xlab('Scenario') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_x_discrete(labels = c('Baseline','Partial Participation', 'Full Participation', 'Individualistic','Concerned','All Changes')) +
  scale_fill_manual(values = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f'), guide = 'none') +
  facet_wrap(~city, nrow = 3, scales = 'free')
dev.off()

# FIGURE S2: Baseline + climate change scenarios across all three cities (Historical - ALL GCMS)

plotdata <- scenarios2[which(scenarios2$scenario == 'base'),-2] # extract baseline scenario data

# get averages over 5 model runs for each GCM
averages <- plotdata %>% group_by(Date, city, `model run`, model) %>% summarize(mean(value))
names(averages)[5] <- 'value'
averages$repetition <- rep('average', nrow(averages))
averages <- averages[,c(1,2,4,3,6,5)]

# merge averages w/ actual model runs
plotdata <- rbind(averages, plotdata)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_cconly_historical_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S3: Baseline + climate change scenarios across all three cities (RCP2.6 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp2p6' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_cconly_rcp26_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S4: Baseline + climate change scenarios across all three cities (RCP4.5 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp4p5' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_cconly_rcp45_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S5: Baseline + climate change scenarios across all three cities (RCP6.0 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp6p0' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_cconly_rcp60_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S6: Baseline + climate change scenarios across all three cities (RCP8.5 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp8p5' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_cconly_rcp85_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S7: Archetypes + climate change scenarios across all cities (Line Plots)

p1 <- ggplot(plotdata[which(plotdata$city == 'Denver'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_color_manual(values = c('black','#8da0cb'), name = '', labels = c('', '')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') +ggtitle('Denver')

p2 <- ggplot(plotdata[which(plotdata$city == 'LasVegas'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('Date') + ylab('') +
  scale_color_manual(values = c('black','#8da0cb'), name = 'Scenario', labels = c('Baseline', 'Participation')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') + ggtitle('Las Vegas')

p3 <- ggplot(plotdata[which(plotdata$city == 'Phoenix'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('') + ylab('') +
  scale_color_manual(values = c('black','#8da0cb'), name = '', labels = c('', '')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') + ggtitle('Phoenix')

setwd(outputdir)
pdf('archetypes_cconly_lineplots.pdf', width = 10, height = 15)
plot_grid(p1, p2, p3, nrow = 1)
dev.off()

# FIGURE S8: Baseline + adaptive water consumption + climate change scenarios across all three cities (Historical - ALL GCMS)

plotdata <- scenarios3[which(scenarios3$scenario == 'base'),-2] # extract baseline scenario data

# get averages over 5 model runs for each GCM
averages <- plotdata %>% group_by(Date, city, `model run`, model) %>% summarize(mean(value))
names(averages)[5] <- 'value'
averages$repetition <- rep('average', nrow(averages))
averages <- averages[,c(1,2,4,3,6,5)]

# merge averages w/ actual model runs
plotdata <- rbind(averages, plotdata)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#D3D3D3') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run` == 'hist' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = 'black') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_adacons_historical_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S9: Baseline + adaptive consumption + climate change scenarios across all three cities (RCP2.6 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp2p6' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#ffffe5') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp2p6' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#fe9929') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_adacons_rcp26_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S10: Baseline + adaptive consumption + climate change scenarios across all three cities (RCP4.5 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp4p5' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fff7bc') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp4p5' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#ec7014') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_adacons_rcp45_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S11: Baseline + adaptive consumption + climate change scenarios across all three cities (RCP6.0 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp6p0' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fee391') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp6p0' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#cc4c02') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_adacons_rcp60_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S12: Baseline + adaptive consutmpion + climate change scenarios across all three cities (RCP8.5 - ALL GCMS)

p1 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run` == 'rcp8p5' & plotdata$model == 'gfdl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'gfdl'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('GFDL')

p2 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'ipsl'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'ipsl'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('IPSL')

p3 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'hadgem'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'hadgem'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('HadGEM')

p4 <- ggplot(plotdata[which(plotdata$repetition != 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'miroc'),]) + geom_line(aes(x = Date, y = value/1000000000, group = model), color = '#fec44f') +
  geom_line(data = plotdata[which(plotdata$repetition == 'average' & plotdata$`model run`  == 'rcp8p5' & plotdata$model == 'miroc'),], aes(x = Date, y = value/1000000000, group = model), color = '#8c2d04') +
  xlab('Date') + ylab('') +
  facet_wrap(~city, nrow = 3, scales = 'free') + theme_light(base_size = 12) + ggtitle('MIROC')

setwd(outputdir)
pdf('baselines_adacons_rcp85_allgcms.pdf', width = 11, height = 5)
plot_grid(p1, p2, p3, p4, nrow = 1)
dev.off()

# FIGURE S13: Archetypes + adaptive water consumption + climate change scenarios across all cities (Line Plots)

p1 <- ggplot(plotdata[which(plotdata$city == 'Denver'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_color_manual(values = c('black','#8da0cb'), name = '', labels = c('', '')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') +ggtitle('Denver')

p2 <- ggplot(plotdata[which(plotdata$city == 'LasVegas'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('Date') + ylab('') +
  scale_color_manual(values = c('black','#8da0cb'), name = 'Scenario', labels = c('Baseline', 'Participation')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') + ggtitle('Las Vegas')

p3 <- ggplot(plotdata[which(plotdata$city == 'Phoenix'),]) + geom_line(aes(x = Date, y = value/1000000000, color = scenario)) +
  theme_light() + theme(legend.position="bottom", text = element_text(size = 16))+ xlab('') + ylab('') +
  scale_color_manual(values = c('black','#8da0cb'), name = '', labels = c('', '')) +
  facet_wrap(~`model run`, nrow = 5, scales = 'free') + ggtitle('Phoenix')

setwd(outputdir)
pdf('archetypes_adacons_lineplots.pdf', width = 10, height = 15)
plot_grid(p1, p2, p3, nrow = 1)
dev.off()

########## EXTRA CALCULATIONS ############

setwd(rdatadir)
load('scenariodata.rdata')

# getting ensemble data (cconly)
averages <- scenarios2 %>% group_by(Date, city, `model run`, model, scenario) %>% summarize(mean(value)); names(averages)[6] <- 'value'
ensemble <- averages %>% group_by(Date, city, `model run`, scenario) %>% summarize(mean(value)); names(ensemble)[5] <- 'value'

# comparing median (Denver RCP4.5; climate change only scenarios)
mediandiff <- median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')]) - 
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')])

relativechange <- mediandiff/median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')])

# t-tests for changes under climate change w/ archetypes

# RCP2.6 - Denver
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Denver')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Denver')])

# RCP4.5 - Denver
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')])

# RCP6.0 - Denver
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')])

# RCP8.5 - Denver
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver')])

# RCP2.6 - Las Vegas
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas')])

# RCP4.5 - Las Vegas
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas')])

# RCP6.0 - Las Vegas
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'LasVegas')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'LasVegas')])

# RCP8.5 - Las Vegas
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas')])

# RCP2.6 - Phoenix
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Phoenix')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Phoenix')])

# RCP4.5 - Phoenix
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix')])

# RCP6.0 - Phoenix
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Phoenix')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Phoenix')])

# RCP8.5 - Phoenix
t.test(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix')], 
       ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix')])

# getting ensemble data (adacons)
averages <- scenarios3 %>% group_by(Date, city, `model run`, model, scenario) %>% summarize(mean(value)); names(averages)[6] <- 'value'
ensemble <- averages %>% group_by(Date, city, `model run`, scenario) %>% summarize(mean(value)); names(ensemble)[5] <- 'value'

# relative change using adaptive water consumption scenarios

# RCP2.6 - Denver
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Denver')]) - 
                     median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Denver')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Denver')])

# RCP4.5 - Denver
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver')])

# RCP6.0 - Denver
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')])

# RCP8.5 - Denver
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver')])

# RCP2.6 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas')])

# RCP4.5 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas')])

# RCP6.0 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'LasVegas')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'LasVegas')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'LasVegas')])

# RCP8.5 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas')])

# RCP2.6 - Phoenix
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Phoenix')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Phoenix')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'Phoenix')])

# RCP4.5 - Phoenix
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix')])

# RCP6.0 - Phoenix
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Phoenix')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Phoenix')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Phoenix')])

# RCP8.5 - Phoenix
(median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix')]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix')])

# other differences

# Denver RCP6.0 participation - baseline
median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')])

# Denver RCP6.0 participation - historical baseline
median(ensemble$value[which(ensemble$scenario == 'part' & ensemble$`model run` == 'rcp6p0' & ensemble$city == 'Denver')]) - 
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'hist' & ensemble$city == 'Denver')])

# time-based relative change (baseline w/ adaptive water consumption)

# RCP4.5 & RCP8.5 - Denver
(median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Denver' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Denver' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)])

# RCP4.5 & RCP8.5 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)])

# RCP2.6 & RCP8.5 - Las Vegas
(median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp2p6' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'LasVegas' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)])

# RCP4.5 & RCP8.5 - Phoenix
(median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp4p5' & ensemble$city == 'Phoenix' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]) - 
    median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)]))/
  median(ensemble$value[which(ensemble$scenario == 'base' & ensemble$`model run` == 'rcp8p5' & ensemble$city == 'Phoenix' & year(ensemble$Date) >= 2050 & year(ensemble$Date) < 2060)])




