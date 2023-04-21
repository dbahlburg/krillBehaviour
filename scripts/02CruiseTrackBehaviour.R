library(tidyverse)
library(lubridate)
library(terra)
library(scico)
library(metR)
library(ggnewscale)
library(tidyterra)

krillBehaviour <- read.csv('~/github/krillBehaviour/data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behavior = factor(behavior, 
                            levels = c('constantSurface','reverseDVM','shallowDVM','DVM12h','DVM',
                                       'diffuseDVM','deepDVM','NA'))) %>% #'deepDiffuseDVM',
  dplyr::select(date, behavior, behavBlock)


# load processed behaviour data
behaviourData <- read.csv('data/behaviourData.csv') %>% 
  mutate(localTime = as.POSIXct(localTime),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  arrange(localTime)

# helper tibble for nicer behavioural labels
behaviourTitles <- tibble(behavior = c('constantSurface','shallowDVM','reverseDVM','DVM','DVM12h',
                                       'diffuseDVM','deepDVM','NA'),
                          title = c('constant surface', 'DVM-100m', 'reverse DVM', 'DVM-150m', 'DVM 12h', 
                                    'diffuse DVM', 'deep DVM', 'no data')) 

krillBehaviour <- krillBehaviour %>% 
  left_join(.,behaviourTitles)

cruiseData <- readRDS('~/github/krillBehaviour/data/krillAcousticsProcessed/timeSeriesMetrics.RDS') %>%
  filter(lat < -57.5) %>% 
  distinct(localTime, lat, long) %>% 
  mutate(date = as.Date(localTime, tz = 'Etc/GMT+3')) %>% 
  left_join(.,krillBehaviour) %>% 
  mutate(id = paste(date,behavBlock, sep = '_')) %>% 
  filter(!is.na(behavior))

# Cruise log 
cruiseLog <- tibble(startDate = c('2020-12-04','2020-12-28','2021-01-11', '2021-03-20', '2021-03-28', '2021-04-08', '2021-05-05', '2021-06-05', '2021-06-12', '2021-07-14'),
                    endDate = c('2020-12-28','2021-01-11', '2021-03-20', '2021-03-28', '2021-04-08', '2021-05-05', '2021-06-05', '2021-06-12', '2021-07-14', '2021-07-28'),
                    region = c('South Orkney Islands','Bransfield Strait','South Orkney Islands', 'Bransfield Strait', 
                               'Tower Island','Gerlache Strait', 'Bransfield Strait', 'South Orkney Islands', 'South Georgia', 
                               'South Orkney Islands')) %>% 
  mutate(startDate = as.Date(startDate),
         endDate = as.Date(endDate))

coastline <- vect('/Users/dominik/github/krillBehaviour/data/coastline/add_coastline_high_res_line_v7_6.shp/add_coastline_high_res_line_v7_6.shp')
coastline <- vect('/Users/dominik/github/krillBehaviour/data/coastline/add_coastline_high_res_polygon_v7_6.shp/add_coastline_high_res_polygon_v7_6.shp')
coastline <- terra::project(coastline, 'epsg:4326')
wholeArea <- crop(coastline, ext(-62, -43,-65, -59))
peninsula <- crop(coastline, ext(-62, -55,-65, -62))
southOrkneys <- crop(coastline, ext(-47.5, -43, -60.75, -59))

coloursBehav <- scico(7, palette = 'roma', alpha = 0.75)

bathy <- rast('/Users/dominik/github/krillBehaviour/data/bathymetry/IBCSO_v2_bed_WGS84.tif')
wholeAreaBathy <- crop(bathy, ext(-62, -43,-65, -59))
wholeAreaBathy <- as.data.frame(wholeAreaBathy, xy=T) %>% 
  mutate(elevation = ifelse(elevation>0,0,elevation))
SOBathy <- crop(bathy, ext(-47.1, -43.3, -60.75, -59.1))
SOBathy <- as.data.frame(SOBathy,xy=T)%>% 
  mutate(elevation = ifelse(elevation>0,0,elevation))
SOPeninsula <- crop(bathy, ext(-62, -55.9, -64.4, -62.4))
SOPeninsula <- as.data.frame(SOPeninsula,xy=T)%>% 
  mutate(elevation = ifelse(elevation>0,0,elevation))

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

# cruise track by month
cruiseTrackPlot <- cruiseData %>% 
  arrange(localTime) %>% 
  mutate(month = month(localTime),
         month = ifelse(month == 12, 0, month)) %>% 
  ggplot(.) +
  geom_raster(data = wholeAreaBathy, aes(x = x, y = y, fill = elevation)) +
  # scale_fill_gradient(low = '#616161', high = '#fafafa',
  #                     limits = c(-6000, 0),
  #                     breaks = seq(-6000, 0, by = 2000)) +
  scale_fill_steps(low = '#616161', high = '#fafafa',
                   limits = c(-6000, 0),
                   #breaks = seq(-6000, 0, by = 2000)
                   breaks = seq(-6000, 0, by = 500)
                   ) +
  geom_spatvector(data = wholeArea, size = 0.1, 
                  colour = NA, fill = '#6e6e6e') +
  scale_colour_stepsn(colours = scico(n = 9, palette = 'batlow'),
                      limits = c(-1,7),
                      breaks = -1:7,
                      guide = 'none'
  ) +
  geom_path(aes(x = long, y = lat, colour = month, group = behavBlock),
            size = 3.5)  +
  labs(x = '', y = '') +
  lightMode +
  theme(legend.position = 'none')

cruiseTrackPeninsula <- cruiseTrackPlot +
  scale_x_continuous(limits = c(-62, -55.9), breaks = seq(-62,-56,by = 2)) +
  scale_y_continuous(limits = c(-64.4, -62.4), breaks = seq(-65,-52,by = 1)) 

cruiseTrackSouthOrkneys <- cruiseTrackPlot +
  scale_x_continuous(limits = c(-47.1, -43.3), breaks = c(-47:-44)) +
  scale_y_continuous(limits = c(-60.75, -59.1))

legendPlot <- cruiseTrackSouthOrkneys +
  theme(legend.position = 'bottom',
        legend.key.width = unit(4.5,'cm'))

ggsave('~/github/krillBehaviour/plots/cruiseTrackPeninsula.pdf', plot = cruiseTrackPeninsula, width = 14, height = 9)
ggsave('~/github/krillBehaviour/plots/cruiseTrackLegend.pdf', plot = legendPlot, width = 14, height = 9)
ggsave('~/github/krillBehaviour/plots/cruiseTrackSOI.pdf', plot = cruiseTrackSouthOrkneys, width = 14, height = 9)


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
                      breaks = seq(-6000, 0, by = 2000)) +
  geom_spatvector(data = peninsula, size = 0.1, 
                  colour = '#4a4a4a50', fill = '#4a4a4a') +
  scale_x_continuous(limits = c(-62, -55.9), breaks = seq(-62,-56,by = 2)) +
  scale_y_continuous(limits = c(-64.4, -62.4), breaks = seq(-65,-52,by = 1)) +
  scale_colour_manual(values = c(coloursBehav,'#bababa'),drop = FALSE) +
  geom_path(aes(x = long, y = lat, colour = title, group = behavBlock),
            size = 2.5)  +
  labs(x = '', y = '') +
  lightMode +
  theme(legend.position = 'none')
ggsave('~/github/krillBehaviour/plots/behavPeninsulaNew2.pdf', plot = peninsulaBehavPlot, width = 14, height = 9)

SObehavPlot <- cruiseData %>% 
  filter(lat > -62) %>% 
  arrange(localTime) %>% 
  mutate(title = factor(title,
                        levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h', 
                                   'diffuse DVM', 'deep DVM', 'no data'))) %>% 
  #slice(which(row_number() %% 9 == 1)) %>% 
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
            size = 3, alpha = 0.7)  +
  labs(x = '', y = '') +
  lightMode +
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(legend.position = 'none')
ggsave('~/github/krillBehaviour/plots/behavSONew2.pdf', plot = SObehavPlot, width = 14, height = 9)

behaviourLegend <- cruiseData %>% 
  mutate(title = factor(title,
                        levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h', 
                                   'diffuse DVM', 'deep DVM', 'no data'))) %>% 
  ggplot(.) +
  geom_path(aes(x = long, y = lat, colour = title, group = behavBlock),
            size = 3, alpha = 0.7)  +
  scale_colour_manual(values = c(scico(7, palette = 'roma', alpha = 0.75),'#f2f2f2'), drop=F) +
  labs(x = '', y = '') +
  lightMode +
  theme(legend.position = 'bottom',
        legend.key.width = unit(3.5,'cm')
        ) +
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('~/github/krillBehaviour/plots/behaviourLegend.pdf', plot = behaviourLegend, width = 14, height = 9)
# ------------------------------------------------------------------------------------------------------------- #


# =========================================================================================== #
# Timeline plots Figure 1
# =========================================================================================== #
# -------------------------------------------------------------------------------------------------- #
# load processed behaviour data, filter "good quality" days
behaviourData <- read.csv('data/behaviourData.csv') %>% 
  mutate(localTime = as.POSIXct(localTime),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  arrange(localTime)

# Extract chlorophyll a data to match acoustics
# Data product from Copernicus Marine Server: OCEANCOLOUR_GLO_BGC_L4_MY_009_104
cruiseData <- readRDS('~/github/krillBehaviour/data/krillAcousticsProcessed/timeSeriesMetrics.RDS')
chlaData <- rast('~/github/krillBehaviour/data/cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D_1666536408783.nc')
depth <- rast('~/github/krillBehaviour/data/bathymetry/IBCSO_v2_bed_WGS84.tif')
depth[depth>0] <- 0

chlaTimes <- terra::time(chlaData)
cruiseData <- cruiseData %>% 
  filter(localTime >= as.Date('2020-12-01') & localTime <= as.Date('2021-07-31')) %>% 
  mutate(date = as.Date(localTime))

cruiseCoords <- cruiseData %>% 
  dplyr::select(date, long, lat) %>% 
  rowwise() %>% 
  mutate(layerIndex = which(chlaTimes == date)[1]) %>% 
  ungroup() 

chlaValues <- terra::extract(chlaData, as.matrix(cruiseCoords[,2:3]), method = 'bilinear')
chlaValuesExtracted <- unlist(mapply(function(i, j) chlaValues[i, j], 1:nrow(chlaValues), cruiseCoords$layerIndex))
depthValues <- unlist(terra::extract(depth, as.matrix(cruiseCoords[,2:3]), method = 'bilinear'))

cruiseData <- cruiseData %>% 
  mutate(chla = chlaValuesExtracted,
         depth = depthValues,
         month = month(date))


# behavioural plot
krillBehaviour <- read.csv('~/github/krillBehaviour/data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
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

behavLabel <- tibble(behavior = c('constantSurface', 'reverseDVM','shallowDVM', 'DVM12h','DVM',
                                  'diffuseDVM','deepDVM','noData'),
                     label = factor(c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM 12h', 'DVM-150m', 
                                      'diffuse DVM', 'deep DVM', 'no data'),
                                    levels = c('constant surface', 'reverse DVM', 'DVM-100m', 'DVM-150m', 'DVM 12h',  
                                               'diffuse DVM', 'deep DVM', 'no data'),))

krillBehavPlot <- krillBehaviour %>% 
  left_join(.,behavLabel) %>% 
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
  theme(axis.title = element_text(size = 18),
        panel.grid = element_blank(),
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.key.width = unit(2, units = 'cm'),
        legend.key.height = unit(0.5, units = 'cm'),
        legend.background = element_rect(fill = '#ffffff', colour = NA),
        legend.key = element_rect(fill = '#ffffff', colour = NA),
        legend.text = element_text(size = 12, colour = '#303030'),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.position = 'bottom')  +
  guides(color = guide_legend(override.aes = list(size = 2)))
ggsave('~/github/krillBehaviour/plots/krillBehavNew2.pdf', plot = krillBehavPlot, width = 14, height = 1.5)



krillBehavPlot2 <- behaviourData %>% 
  filter(ibcsoDepth < 0) %>% 
  mutate(dateBin = cut(localTime, '24 hours')) %>% 
  group_by(dateBin) %>% 
  summarize(chla = mean(surfaceChla,na.rm=T),
            localTime = mean(localTime,na.rm=T)) %>% 
  ggplot(.) +
  geom_vline(xintercept = seq(as.POSIXct('2020-12-01'), as.POSIXct('2021-08-01'), by = '30 days'), size = 0.25, colour= '#b8b8b8') +
  geom_col(aes(x = localTime, y = chla)) +
  scale_fill_manual(values = c(scico(8, palette = 'roma', alpha = 0.45),'#bababa')) +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b',
                   limits = c(as.POSIXct('2020-12-01 00:00:00'),
                              as.POSIXct('2021-07-31 00:00:00'))) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,1,2)) +
  labs(title = 'surface chlorophyll a concentration',
       y = expression(mg~m^{-3})) +
  theme_bw() +
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
ggsave('~/github/krillBehaviour/plots/chlaTimeline.pdf', plot = krillBehavPlot2, width = 14, height = 2)



krillBehavPlot2 <- behaviourData %>% 
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
  #scale_y_continuous(limits = c(0,2), breaks = c(0,1,2)) +
  labs(title = '',
       y = '°C') +
  theme_bw() +
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
ggsave('~/github/krillBehaviour/plots/tempTimeline.pdf', plot = krillBehavPlot2, width = 14, height = 2)


krillBehavPlot2 <- behaviourData %>% 
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
  theme_bw() +
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
ggsave('~/github/krillBehaviour/plots/photoperiodTimeline.pdf', plot = krillBehavPlot2, width = 14, height = 2)

krillBehavPlot3 <- behaviourData %>% 
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
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        panel.grid = element_blank(),
        axis.text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, hjust = 0.5),
        legend.position = 'none')
ggsave('~/github/krillBehaviour/plots/depth.pdf', plot = krillBehavPlot3, width = 14, height = 2)


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

regionPlot <- cruiseLog %>%
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
       x = '') +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        panel.grid = element_blank(),
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.key.width = unit(2, units = 'cm'),
        legend.key.height = unit(0.5, units = 'cm'),
        legend.background = element_rect(fill = '#ffffff', colour = NA),
        legend.key = element_rect(fill = '#ffffff', colour = NA),
        legend.text = element_text(size = 12, colour = '#303030'),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.position = 'none')
ggsave('~/github/krillBehaviour/plots/regionPlot.pdf', plot = regionPlot, width = 14, height = 2)



