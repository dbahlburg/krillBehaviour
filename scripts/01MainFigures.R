# This script contains all code necessary to reproduce the different Figures contained in the main manuscript of Bahlburg et al. (2023):
# Dominik Bahlburg, Lukas Hüppe,Thomas Böhrer, Sally E. Thorpe, Eugene J. Murphy, Uta Berger, Bettina Meyer:
# Plasticity and seasonality of the vertical migratory behaviour of Antarctic krill using acoustic data from fishing vessels
# Royal Society Open Science
# Example code as well as a tutorial for the screenshot processing to create the backscattering intensities used in this analysis 
# can be found under https://dbahlburg.github.io/isolateBiomassSignal/ and https://www.biorxiv.org/content/10.1101/2023.04.16.537064v1.full.pdf
# Currently, the Figures showing raw data (Figure 2, behavioural examples and Figure 6) are still missing in this script as the raw acoustic data are property of 
# Aker BioMarine. However, these data are supposed to be released soon under https://www.hubocean.earth/
# The code used for creating these Figures, however, is contained in 02BehaviouralExamples.R
# Figure 7 was entirely drawn in Affinity Design.
# ------------------------------------------------------------------------------------------------------------------------------ #
# load libraries
library(tidyverse)
library(lubridate)
library(terra)
library(scico)
library(ggnewscale)
library(tidyterra)
library(cowplot)
# ------------------------------------------------------------------------------------------------------------------------------ #
# Figure 1 - Cruise track. The arrangement of the two Figures as well as labels for the different geographical regions were later done
# in Affinity Design
# import krill behaviour data - this is a time series where the behaviour of krill swarms was categorized for each day
krillBehaviour <- read.csv('data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behavior = factor(behavior, 
                           levels = c('constantSurface','reverseDVM','shallowDVM','DVM12h','DVM',
                                      'diffuseDVM','deepDVM','NA'))) %>% #'deepDiffuseDVM',
  dplyr::select(date, behavior, behavBlock)

# helper tibble for nicer behaviour labels
behaviourTitles <- tibble(behavior = c('constantSurface','shallowDVM','reverseDVM','DVM','DVM12h',
                                       'diffuseDVM','deepDVM','NA'),
                          title = c('constant surface', 'DVM-100m', 'reverse DVM', 'DVM-150m', 'DVM 12h', 
                                    'diffuse DVM', 'deep DVM', 'no data')) 
# add labels to behaviour data
krillBehaviour <- krillBehaviour %>% 
  left_join(.,behaviourTitles)

# import cruise data from timeSeriesMetrics.RDS - this is a slimmed down dataset that excludes any depth-specific information (backscattering values etc.)
cruiseData <- readRDS('data/krillAcousticsProcessed/timeSeriesMetrics.RDS') %>%
  filter(lat < -57.5) %>% 
  distinct(localTime, lat, long) %>% 
  mutate(date = as.Date(localTime, tz = 'Etc/GMT+3')) %>% 
  left_join(.,krillBehaviour) %>% 
  mutate(id = paste(date,behavBlock, sep = '_')) %>% 
  filter(!is.na(behavior))

# add geographical data for map information (coastline, bathymetry), crop subareas for faster plotting
coastline <- vect('data/coastline/add_coastline_medium_res_polygon_v7_4/add_coastline_medium_res_polygon_v7_4.shp')
coastline <- terra::project(coastline, 'epsg:4326')
wholeArea <- crop(coastline, ext(-62, -43,-65, -59))
peninsula <- crop(coastline, ext(-62, -55,-65, -62))
southOrkneys <- crop(coastline, ext(-47.5, -43, -60.75, -59))

# import IBCSO bathymetry - dataset not included in the github repo due to its large size but but accessible under
# https://doi.pangaea.de/10.1594/PANGAEA.937574?format=html#download
bathy <- rast('~/github/SOdata/IBCSO_v2_ice-surface_WGS84.nc')
wholeAreaBathy <- crop(bathy, ext(-62, -43,-65, -59))
wholeAreaBathy <- as.data.frame(wholeAreaBathy, xy=T) %>% 
  mutate(elevation = ifelse(x>0,0,z))
SOBathy <- crop(bathy, ext(-47.1, -43.3, -60.75, -59.1))
SOBathy <- as.data.frame(SOBathy,xy=T)%>% 
  mutate(elevation = ifelse(z>0,0,z))
SOPeninsula <- crop(bathy, ext(-62, -55.9, -64.4, -62.4))
SOPeninsula <- as.data.frame(SOPeninsula,xy=T)%>% 
  mutate(elevation = ifelse(z>0,0,z))

# pre-define theme settings for plots to save space
lightMode <- theme(panel.background = element_rect(fill = '#ffffff', colour = '#212121'),
                   plot.background = element_rect(fill = '#ffffff', colour = NA),
                   legend.position = 'top',
                   legend.key.width = unit(2, units = 'cm'),
                   legend.key.height = unit(1, units = 'cm'),
                   legend.background = element_rect(fill = '#ffffff', colour = NA),
                   legend.key = element_rect(fill = '#ffffff', colour = NA),
                   legend.text = element_text(size = 18, colour = '#303030'),
                   legend.title = element_blank(),
                   axis.text = element_text(size = 36, colour = '#454545'),
                   axis.title = element_text(size = 36, colour = '#454545'))

# plot cruise track for entire dataset, later subset South Orkney Islands and Antarctic Peninsula for clearer maps
monthLabel <- tibble(month = 0:7,
                     monthLabel = factor(c('Dec','Jan','Feb','Mar','Apr','May','Jun','Jul'),
                                         levels = c('Dec','Jan','Feb','Mar','Apr','May','Jun','Jul')))

cruiseTrackPlot <- cruiseData %>% 
  arrange(localTime) %>% 
  mutate(month = month(localTime),
         month = ifelse(month == 12, 0, month)) %>% 
  left_join(., monthLabel) %>% 
  ggplot(.) +
  geom_raster(data = wholeAreaBathy, aes(x = x, y = y, fill = elevation)) +
  scale_fill_steps(low = '#616161', high = '#fafafa',
                   limits = c(-6000, 0),
                   breaks = seq(-6000, 0, by = 500),
                   guide = 'none') +
  geom_spatvector(data = wholeArea, linewidth = 0.1, 
                  colour = NA, fill = '#6e6e6e') +
  geom_path(aes(x = long, y = lat, colour = monthLabel, group = behavBlock),
            linewidth = 1.5, lineend = 'round')  +
  scale_colour_manual(values = scico(n = 9, palette = 'batlow')) +
  labs(x = '', y = '') +
  lightMode +
  theme(legend.position = 'bottom')

# subset areas
cruiseTrackPeninsula <- cruiseTrackPlot +
  scale_x_continuous(limits = c(-62, -55.9), breaks = seq(-62,-56,by = 2)) +
  scale_y_continuous(limits = c(-64.4, -62.4), breaks = seq(-65,-52,by = 1)) 

cruiseTrackSouthOrkneys <- cruiseTrackPlot +
  scale_x_continuous(limits = c(-47.1, -43.3), breaks = c(-47:-44)) +
  scale_y_continuous(limits = c(-60.75, -59.1))

# save plots
ggsave('plots/cruiseTrackPeninsula.pdf', plot = cruiseTrackPeninsula, width = 14, height = 9)
ggsave('plots/cruiseTrackSouthOrkneys.pdf', plot = cruiseTrackSouthOrkneys, width = 14, height = 9)
# ------------------------------------------------------------------------------------------------------------------------------ #
# Figure 3 behaviour maps and timelines
# Similar to Figure 1, the final arrangement of the subplots was done in Affinity Design and the following code returns the 
# individual sub-plots

# colour code for different behavioural classes
coloursBehav <- scico(7, palette = 'roma', alpha = 0.75)

# create map for Antarctic Peninsula
peninsulaBehavPlot <- cruiseData %>% 
  arrange(localTime) %>% 
  filter(lat < -62) %>% 
  mutate(title = factor(title,
                        levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h', 
                                   'diffuse DVM', 'deep DVM', 'no data'))) %>% 
  ggplot(.) +
  geom_raster(data = SOPeninsula, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = '#616161', high = '#fafafa',
                      limits = c(-6000, 0),
                      breaks = seq(-6000, 0, by = 2000), guide = 'none') +
  geom_spatvector(data = peninsula, size = 0.1, 
                  colour = '#4a4a4a50', fill = '#4a4a4a') +
  scale_x_continuous(limits = c(-62, -55.9), breaks = seq(-62,-56,by = 2)) +
  scale_y_continuous(limits = c(-64.4, -62.4), breaks = seq(-65,-52,by = 1)) +
  scale_colour_manual(values = c(coloursBehav,'#bababa'),drop = FALSE) +
  geom_path(aes(x = long, y = lat, colour = title, group = behavBlock),
            size = 1.5)  +
  labs(x = '', y = '') +
  lightMode +
  theme(legend.position = 'bottom')

# save plot
ggsave('plots/behavPeninsula.pdf', plot = peninsulaBehavPlot, width = 14, height = 9)

# create map for South Orkney Islands
SObehavPlot <- cruiseData %>% 
  filter(lat > -62) %>% 
  arrange(localTime) %>% 
  mutate(title = factor(title,
                        levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h', 
                                   'diffuse DVM', 'deep DVM', 'no data'))) %>% 
  ggplot(.) +
  geom_raster(data = SOBathy, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = '#616161', high = '#fafafa',
                      limits = c(-6000, 0),
                      breaks = seq(-6000, 0, by = 2000), guide = 'none') +
  geom_spatvector(data = southOrkneys, size = 0.1, 
                  colour = '#ebebeb', fill = '#808080') +
  scale_x_continuous(limits = c(-47.1, -43.3), breaks = c(-47:-44)) +
  scale_y_continuous(limits = c(-60.75, -59.1)) +
  scale_colour_manual(values = c(scico(7, palette = 'roma', alpha = 0.75),'#f2f2f2'), drop=F) +
  geom_path(aes(x = long, y = lat, colour = title, group = behavBlock),
            size = 1.5, alpha = 0.7)  +
  labs(x = '', y = '') +
  lightMode +
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(legend.position = 'bottom')

# save plot
ggsave('plots/behavSO.pdf', plot = SObehavPlot, width = 14, height = 9)

# ---------------- #
# Timeline plots
# load processed behaviour data, filter "good quality" days
behaviourData <- read.csv('data/behaviourData.csv') %>% 
  mutate(localTime = as.POSIXct(localTime, format = "%Y-%m-%d %H:%M:%S"),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  arrange(localTime)

# import data for region timeline
# Cruise log 
cruiseLog <- tibble(startDate = c('2020-12-04','2020-12-28','2021-01-11', '2021-03-20', '2021-03-28', '2021-05-05', '2021-06-05', '2021-06-12', '2021-06-25'),
                    endDate = c('2020-12-28','2021-01-11', '2021-03-20', '2021-03-28', '2021-05-05', '2021-06-05', '2021-06-12', '2021-06-25', '2021-07-28'),
                    region = c('South Orkney Islands','Bransfield Strait','South Orkney Islands', 'Bransfield Strait', 
                               'Gerlache Strait', 'Bransfield Strait', 'South Orkney Islands', 'South Georgia', 
                               'South Orkney Islands')) %>% 
  mutate(startDate = as.Date(startDate),
         endDate = as.Date(endDate))

labelTib <- cruiseLog %>% 
  distinct(region) %>% 
  mutate(label = c('SOI','BS','SAP','SG'))

# tibble for correct behaviour labels
behavLabel <- tibble(behavior = c('constantSurface', 'reverseDVM','shallowDVM', 'DVM12h','DVM',
                                  'diffuseDVM','deepDVM','noData'),
                     label = factor(c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM 12h', 'DVM-150m', 
                                      'diffuse DVM', 'deep DVM', 'no data'),
                                    levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h',  
                                               'diffuse DVM', 'deep DVM', 'no data')))

# tibble that contains start and end days of behaviours
krillBehaviourStartStop <- read.csv('data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behavior = ifelse(is.na(behavior), 'noData',behavior)) %>% 
  group_by(behavior, behavBlock) %>% 
  summarize(start = first(date),
            end = last(date)) %>% 
  mutate(startDT = as.POSIXct(paste(start, '00:00:00', sep = ' ')),
         endDT = as.POSIXct(paste(end, '23:59:59', sep = ' ')),
         behavior = factor(behavior, 
                           levels = c('constantSurface','reverseDVM','shallowDVM','DVM','DVM12h',
                                      'diffuseDVM','deepDVM','noData')))
# plot theme for timelines
timelineTheme <- theme_bw() +
  theme(axis.title = element_text(size = 18),
        panel.grid = element_blank(),
        axis.text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, hjust = 0.5),
        legend.position = 'none')

# behaviour timeline
behaviourTimeline <- krillBehaviourStartStop %>% 
  left_join(., behavLabel) %>% 
  ggplot(.) +
  geom_rect(aes(xmin = startDT, xmax = endDT, ymin = 0, ymax = 1, fill = label)) +
  scale_fill_manual(values = c(scico(7, palette = 'roma', alpha = 0.75),'#f2f2f2')) +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  labs(title = '') +
  theme_bw() +
  timelineTheme
ggsave('plots/behaviourTimeline.pdf', plot = behaviourTimeline, width = 28, height = 2)


# temperature timeline
temperatureTimeline <- behaviourData %>% 
  filter(ibcsoDepth < 0) %>% 
  mutate(dateBin = cut(localTime, '8 hours')) %>% 
  group_by(dateBin) %>% 
  summarize(temp = mean(surfaceTemp,na.rm=T),
            localTime = mean(localTime,na.rm=T)) %>% 
  ggplot(.) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  geom_path(aes(x = localTime, y = temp), size = 0.4) +
  scale_fill_manual(values = c(scico(8, palette = 'roma', alpha = 0.45),'#bababa')) +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  labs(title = '',
       y = '°C') +
  timelineTheme
ggsave('plots/tempTimeline.pdf', plot = temperatureTimeline, width = 28, height = 2)

photoperiodTimeline <- behaviourData %>% 
  mutate(dateBin = cut(localTime, '8 hours')) %>% 
  group_by(dateBin) %>% 
  summarize(photoperiod = mean(difftime(sunset, sunrise, units = 'hours')),
            localTime = mean(localTime,na.rm=T)) %>% 
  ggplot(.) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  geom_path(aes(x = localTime, y = photoperiod), size = 0.4) +
  scale_fill_manual(values = c(scico(8, palette = 'roma', alpha = 0.45),'#bababa')) +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  scale_y_continuous(limits = c(2,22), breaks = c(6,12,18)) +
  labs(title = '',
       y = 'hours') +
  timelineTheme
ggsave('plots/photoperiodTimeline.pdf', plot = photoperiodTimeline, width = 28, height = 2)

depthTimeline <- behaviourData %>% 
  filter(ibcsoDepth < 0) %>% 
  mutate(dateBin = cut(localTime, '6 hours')) %>% 
  group_by(dateBin) %>% 
  summarize(depth = mean(ibcsoDepth,na.rm=T),
            localTime = mean(localTime,na.rm=T)) %>% 
  ggplot(.) +
  geom_path(aes(x = localTime, y = depth), size = 0.4) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  scale_y_continuous(limits = c(-5000,0), breaks = c(-5000,-2500,0)) +
  labs(title = '',
       y = '') +
  timelineTheme
ggsave('plots/depthTimeline.pdf', plot = depthTimeline, width = 28, height = 2)

regionTimeline <- cruiseLog %>%
  mutate(startDate = as.POSIXct(paste(startDate, '00:00:00')),
         endDate = as.POSIXct(paste(endDate, '23:59:59'))) %>% 
  left_join(.,labelTib) %>% 
  group_by(startDate, label) %>% 
  mutate(meanPos = startDate + floor((endDate-startDate)/2)) %>% 
  ggplot(.) +
  geom_rect(aes(xmin = startDate, xmax = endDate, ymin = 0, ymax = 1, fill = label)) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  geom_text( aes(x = meanPos, y = 0.5, label = label), size = 6) + 
  scale_fill_manual(values = c('#f0f0f0','#d6d6d6','#f0f0f0','#d6d6d6')) +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  labs(title = '',
       y = '',
       x = '') +
  timelineTheme
ggsave('plots/regionPlot.pdf', plot = regionTimeline, width = 28, height = 2)

# ------------------------------------------------------------------------------------------------------------------------------ #
# Figure 4 - Center of Mass seasonal comparison
# Visualize the seasonal dynamics of the Center of Mass (Figure 4)
# modify behaviourData -> bin observations into seasons
seasDiffData <-  behaviourData %>% 
  mutate(dateMonth = month(date),
         season = ifelse(dateMonth %in% c(12,1,2), 'summer',
                         ifelse(dateMonth %in% c(3,4,5), 'autumn',
                                ifelse(dateMonth %in% c(6,7), 'winter', NA)
                         )
         )
  )
seasonsVec <- c('summer','autumn','winter')

# create plots
timePlotList <- list()
for (i in 1:length(seasonsVec)){
  sunriseSunset <- seasDiffData %>% 
    filter(season == seasonsVec[i]) %>% 
    mutate(sunrise = as.POSIXct(paste('2020-01-01', strftime(sunrise, format="%H:%M:%S", tz = 'Etc/GMT+3'), sep = ' ')),
           sunset = as.POSIXct(paste('2020-01-01', strftime(sunset, format="%H:%M:%S", tz = 'Etc/GMT+3'), sep = ' '))) %>% 
    summarize(sunriseMin = min(sunrise),
              sunriseMax = max(sunrise),
              sunsetMin = min(sunset),
              sunsetMax = max(sunset))
  
  seasDiffPlotTime <-  seasDiffData %>% 
    filter(season == seasonsVec[i]) %>% 
    mutate(time = as.POSIXct(paste('2020-01-01', strftime(localTime, format="%H:%M:%S", tz = 'Etc/GMT+3'), sep = ' '))) %>% 
    ggplot(., aes(x=time, y=COM)) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, adjust = 0.2) +
    scale_fill_scico(palette = 'davos') +
    scale_x_continuous(expand = c(0, 0), breaks = c(1577837100, 1577858400,
                                                    1577880000, 1577901600,
                                                    1577922900),
                       labels = c('00:00','06:00','12:00','18:00','24:00')) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(-260,5),
                       breaks = c(-200, -100, 0)) +
    annotate(geom = "rect", xmin = sunriseSunset$sunriseMin, xmax = sunriseSunset$sunriseMax, ymin = -260, ymax = 5,
             fill = "#f2f2f2", alpha = 0.2) +
    annotate(geom = "rect", xmin = sunriseSunset$sunsetMin, xmax = sunriseSunset$sunsetMax, ymin = -260, ymax = 5,
             fill = "#f2f2f2", alpha = 0.2) +
    labs(y = 'Depth in m',
         x = 'time',
         title = c('Summer','Autumn','Winter')[i]) +
    theme(panel.background = element_rect(fill = NA, colour = '#3d3d3d'),
          strip.background = element_rect(fill = NA),
          strip.text = element_text(size = 28),
          axis.text = element_text(size = 28),
          plot.title = element_text(size = 42, hjust = 0.5),
          axis.title = element_text(size = 28),
          legend.key.width=unit(3,"cm"),
          legend.key.height=unit(0.2,"cm"),
          legend.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.position = 'bottom') +
    guides(fill = guide_colourbar(title.position = "top"))
  
  timePlotList[[i]] <- seasDiffPlotTime
}

# arrange plots in panel and save
timePlot <- plot_grid(plotlist=timePlotList, nrow = 1)
ggsave('plots/Figure4.pdf', plot = timePlot, width = 22, height = 6)
# ------------------------------------------------------------------------------------------------------------------------------ #
# Figure 5 - environmental properties for behaviour classes
# calculate daily means of environmental variables to match temporal resolution of behavioural classes
environmentBehaviour <- behaviourData %>% 
  group_by(date) %>% 
  summarize(`distance to coast [km]` = mean(distanceToCoast_km),
            `surface chlorophyll a [mg m-3]` = mean(surfaceChla),
            `depth [m]` = mean(ibcsoDepth),
            `sea surface temperature [°C]` = mean(surfaceTemp, na.rm = T),
            `photoperiod [h]` = as.numeric(mean(difftime(sunset, sunrise, units = 'hours')))) %>% 
  left_join(.,krillBehaviour) %>% 
  filter(!is.na(behavior)) %>% 
  dplyr::select(-date, -behavBlock, -behavior) %>% 
  pivot_longer(-c(title), names_to = 'variable', values_to = 'value')

# plot the data
environmentBehaviourPlot <- environmentBehaviour %>% 
  mutate(title = factor(title, 
                        levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h', 
                                   'diffuse DVM', 'deep DVM', 'no data'))) %>% 
  ggplot(.,aes(x = title, y = value, fill = title)) +
  geom_boxplot() +
  scale_fill_manual(values = c(coloursBehav,'#bababa'),drop = FALSE) +
  facet_wrap(~variable, scales = 'free_y') +
  labs(x = '', y = '') +
  theme(panel.background = element_rect(fill = NA, colour = '#3d3d3d'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 19),
        axis.text.x = element_text(size = 16,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 16),
        plot.margin = margin(75, 25, 5, 10),
        legend.position = 'none')
ggsave('plots/Figure5.pdf', width = 14, height = 9)



















