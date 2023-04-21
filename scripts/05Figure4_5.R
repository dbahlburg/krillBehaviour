# -------------------------------------------------------------------------------------------------- #
# Dominik Bahlburg
# 12.01.2023
# In this script, Figure 4 and Figure 5 of the manuscript are produced
# -------------------------------------------------------------------------------------------------- #
# Load dependencies and libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(cowplot)
library(scico)
library(fuzzyjoin)
# -------------------------------------------------------------------------------------------------- #
# load processed behaviour data, filter "good quality" days
behaviourData <- read.csv('data/behaviourData.csv') %>% 
  filter(qualityGood == T) %>% 
  mutate(localTime = as.POSIXct(localTime),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  arrange(localTime)
# -------------------------------------------------------------------------------------------------- #
# Visualize the seasonal dynamics of the Center of Mass (Figure 4)
seasDiffData <-  behaviourData %>% 
  mutate(dateMonth = month(date),
         season = ifelse(dateMonth %in% c(12,1,2), 'summer',
                         ifelse(dateMonth %in% c(3,4,5), 'autumn',
                                ifelse(dateMonth %in% c(6,7), 'winter', NA)
                                )
                         )
         )
seasonsVec <- c('summer','autumn','winter')

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

timePlot <- plot_grid(plotlist=timePlotList, nrow = 1)
ggsave('plots/Figure3.pdf', plot = timePlot, width = 22, height = 6)
# -------------------------------------------------------------------------------------------------- #
# Environmental properties for each observed behavioural class
krillBehaviour <- read.csv('~/github/krillBehaviour/data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behavior = factor(behavior, 
                           levels = c('constantSurface','shallowDVM','reverseDVM','DVM','DVM12h',
                                      'diffuseDVM','deepDVM','NA'))) %>% 
  dplyr::select(date, behavior, behavBlock)

# helper tibble for nicer behavioural labels
behaviourTitles <- tibble(behavior = c('constantSurface','shallowDVM','reverseDVM','DVM','DVM12h',
                                       'diffuseDVM','deepDVM','NA'),
                          title = c('constant surface', 'DVM-100m', 'reverse DVM', 'DVM-150m', 'DVM 12h', 
                                    'diffuse DVM', 'deep DVM', 'no data')) 

# join titles to krillBehaviour
krillBehaviour <- krillBehaviour %>% 
  left_join(., behaviourTitles)

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

# create colour palette
coloursBehav <- scico(7, palette = 'roma', alpha = 0.75)

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

