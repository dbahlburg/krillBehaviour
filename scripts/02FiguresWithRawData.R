# This script contains the code to produce Figure 2, 6 and Appendix Figures 3 and 5-13 contained in the manuscript of Bahlburg et al. (2023):
# Dominik Bahlburg, Lukas Hüppe,Thomas Böhrer, Sally E. Thorpe, Eugene J. Murphy, Uta Berger, Bettina Meyer:
# Plasticity and seasonality of the vertical migratory behaviour of Antarctic krill using acoustic data from fishing vessels
# Royal Society Open Science
# Example code as well as a tutorial for the screenshot processing to create the backscattering intensities used in this analysis 
# can be found under https://dbahlburg.github.io/isolateBiomassSignal/ and https://www.biorxiv.org/content/10.1101/2023.04.16.537064v1.full.pdf
# These Figures require raw data, which are property of Aker BioMarine and therefore, not included in the data provided.
# The code below is, therefore, not executable but added for completeness.
# These raw data are supposed to be released soon under https://www.hubocean.earth/
# ------------------------------------------------------------------------------------------------------------------------------ #
# load libraries
library(tidyverse)
library(lubridate)
library(scico)
library(terra)
library(tidyterra)

# list files from screenshot processing
acousticsPaths <- list.files('~/github/krillBehaviour/data/krillAcousticsProcessed/processedFiles', pattern = 'RDS', full.names = T)

krillBehaviour <- read.csv('~/github/krillBehaviour/data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behaviour = factor(behavior, 
                            levels = c('constantSurface','shallowDVM','reverseDVM','DVM12h','DVM',
                                       'diffuseDVM','deepDiffuseDVM','deepDVM'))) %>% 
  dplyr::select(date, behavior, behavBlock) %>% 
  mutate(behavior = ifelse(date == as.Date('2021-02-08'), 'shallowDVM', behavior))

COMData <- read.csv('~/github/krillBehaviour/data/behaviourData.csv') %>% 
  mutate(localTime = as.POSIXct(localTime, format = "%Y-%m-%d %H:%M:%S"),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  filter(qualityGood == T)

# Visualize different behavioural classes:
krillBehaviourSelected <- krillBehaviour %>% 
  distinct(behavior) %>% 
  filter(!is.na(behavior)) %>% 
  mutate(startDate = as.POSIXct(paste(c('2021-02-06','2021-04-29', '2021-02-22',
                                        '2021-01-16','2021-04-13', '2021-05-15',
                                        '2021-07-11'), '00:00:01', sep = ' '), tz = 'Etc/GMT+3'),
         endDate = startDate + days(2) - seconds(2),
         behaviourTitle = c('DVM-100m (06-07.02.2021)', 'diffuse surface (29-30.04.2021)',
                            'DVM-150m (22-23.02.2021)','DVM 12h (16-17.01.2021)',
                            'small scale reverse DVM (13-14.04.2021)','diffuse DVM (15-16.05.2021)','deep DVM (11-12.07.2021)')) %>% 
  gather(dateType, dateTime, -behavior, -behaviourTitle) %>% 
  mutate(date = as.Date(dateTime, tz = 'Etc/GMT+3'))


# generate file that contains information which date range is contained in each raw file
fileDateInfos <- NULL
for(i in 1:length(acousticsPaths)){
  
  acousticFile <- readRDS(acousticsPaths[i])
  acousticFileDates <- acousticFile %>% 
    mutate(date = as.Date(localTime)) %>% 
    distinct(date) %>% 
    mutate(fileIndex = i)
  
  fileDateInfos <- fileDateInfos %>% 
    bind_rows(.,acousticFileDates)
}

# create tibble that contains the required date ranges for the behaviour examples as well as the file indexes of the acoustic
# data that need to be imported
timeline <- tibble(date = seq(as.Date('2020-12-01'),as.Date('2021-07-31'), by = '1 day')) %>% 
  left_join(.,fileDateInfos) %>% 
  left_join(.,krillBehaviourSelected) %>% 
  filter(!is.na(behavior)) 

# tibble for behaviour labels and subsetting
behaviours <- distinct(timeline, behavior, behaviourTitle)

# object in which acoustic data will be stored
allAcoustics <- NULL

# extract data for behaviours
for(i in 1:nrow(behaviours)){
  
  requiredFiles <- timeline %>% 
    filter(behavior == behaviours$behavior[i])
  
  requiredFilesCleaned <- requiredFiles %>% 
    filter(!is.na(fileIndex))
  
  acousticData <- NULL
  if(nrow(requiredFilesCleaned) > 0){
    for(k in 1:nrow(requiredFilesCleaned)){
      acousticFile <- readRDS(acousticsPaths[requiredFilesCleaned$fileIndex[k]]) %>% 
        mutate(behavior = behaviours$behaviourTitle[i]) %>% 
        dplyr::select(behavior, localTime, depth, CM, biomassScore, sunrise, sunset) %>% 
        filter(between(localTime, min(requiredFilesCleaned$dateTime,na.rm = T), max(requiredFilesCleaned$dateTime, na.rm = T))) #%>% 
        
        acousticData <- acousticData %>% 
        bind_rows(.,acousticFile)
    }
    allAcoustics <- allAcoustics %>% 
      bind_rows(., mutate(acousticData,
                          yday = yday(localTime),
                          nDay = yday - min(yday),
                          fakeDate = as.POSIXct(paste('2020-01-0', 1 + nDay,' ', format(localTime, format = "%H:%M:%S"), sep = '')),
                          fakeSunrise = as.POSIXct(paste('2020-01-0', 1 + nDay,' ', format(sunrise, format = "%H:%M:%S"), sep = '')),
                          fakeSunset = as.POSIXct(paste('2020-01-0', 1 + nDay,' ', format(sunset, format = "%H:%M:%S"), sep = ''))))
  }
  print(i)
}

# extract center of mass data
comData <- allAcoustics %>% 
  group_by(localTime) %>% 
  summarize(fakeDate = mean(fakeDate),
            CM = mean(CM),
            sunrise = mean(fakeSunrise),
            sunset = mean(fakeSunset)) %>% 
  mutate(date = as.Date(localTime)) %>% 
  left_join(.,krillBehaviour) %>% 
  left_join(., behaviours) %>% 
  mutate(behavior = behaviourTitle,
         behavior = factor(behavior, 
                            levels = c('diffuse surface (29-30.04.2021)', 'small scale reverse DVM (13-14.04.2021)',
                                       'DVM-100m (06-07.02.2021)', 'DVM-150m (22-23.02.2021)',
                                       'DVM 12h (16-17.01.2021)','diffuse DVM (15-16.05.2021)','deep DVM (11-12.07.2021)'))) 

# create plot for behavioural examples
sunriseSunset <- comData %>% 
  mutate(day = day(fakeDate)) %>% 
  group_by(behavior, behaviourTitle, day) %>% 
  summarize(sunrise = mean(sunrise),
            sunset = mean(sunset))
  
behavPlot <- allAcoustics %>% 
  mutate(behavior = factor(behavior, 
                           levels = c('diffuse surface (29-30.04.2021)', 'small scale reverse DVM (13-14.04.2021)',
                                      'DVM-100m (06-07.02.2021)', 'DVM-150m (22-23.02.2021)',
                                      'DVM 12h (16-17.01.2021)','diffuse DVM (15-16.05.2021)','deep DVM (11-12.07.2021)'))) %>% 
  ggplot(.) +
  geom_raster(aes(x = fakeDate, y = depth, fill = biomassScore)) +
  geom_point(data = comData, aes(x = fakeDate, y = CM),
             colour = '#1859b540',
             size = 1.2) +
  scale_fill_scico(palette = 'bilbao',
                   direction = 1,
                   limits = c(0,1), 
                   na.value = 'white',
                   alpha = 0.9,
                   breaks = c(0,1),
                   labels = c('low','high')) +
  labs(x = 'time',
       y = 'depth [m]',
       fill = 'relative backscattering strength') +
  scale_x_datetime(date_labels = '%H:%M', expand = c(0.01,0.01), 
                   breaks = c(as.POSIXct('2020-01-01 00:00:00'),
                              as.POSIXct('2020-01-01 12:00:00'),
                              as.POSIXct('2020-01-02 00:00:00'),
                              as.POSIXct('2020-01-02 12:00:00')),
                   limits = c(as.POSIXct('2020-01-01 00:00:00'),
                              as.POSIXct('2020-01-03 00:00:01'))) +
  scale_y_continuous(limits = c(-250,0), expand = c(0.025,0.025)) +
  geom_vline(data = sunriseSunset,
             aes(xintercept = sunrise),
             colour = '#616161',
             size = 1.2,
             linetype = '22') +
  geom_vline(data = sunriseSunset,
             aes(xintercept = sunset),
             colour = '#6dc7b2',
             size = 1.2,
             linetype = '22') +
  facet_wrap(~behavior, ncol = 2) +
  # geom_vline(xintercept = as.POSIXct(c('2020-01-01 12:00:00','2020-01-02 12:00:00')),
  #            colour = '#616161',
  #            size = 1.2,
  #            linetype = '22') +
  theme_bw() +
  theme(axis.title = element_text(size = 28),
        axis.text = element_text(size = 28),
        panel.background = element_rect(fill = 'white', colour = '#303030'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, 'lines'),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 30),
        legend.position = 'bottom',
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(0.3,"cm"),
        legend.title = element_text(size = 28, hjust = 0.5),
        legend.text = element_text(size = 28)) +
  guides(fill = guide_colourbar(title.position = "top"))

ggsave('plots/Figure2.pdf', dpi=70, plot = behavPlot, width = 19, height = 20)
# ----------------------------------------------------------------------------------------------------- #
# Figure 6
# Extract two behavioral examples where it is visible that krill individuals can behave differently under the same
# environmental conditions

# set start and end times of data to be extracted
krillBehavExamples <-  tibble(startDate = as.POSIXct(paste(c('2021-01-13', '2021-01-16', '2021-04-20'), '00:00:01', sep = ' '), tz = 'Etc/GMT+3'),
         endDate = startDate + days(3) - seconds(2)) %>% 
  mutate(date = as.Date(startDate, tz = 'Etc/GMT+3'))

krillBehavExamples <- krillBehavExamples %>% 
  left_join(.,fileDateInfos)

fileIndexes <- distinct(krillBehavExamples, fileIndex)

allAcoustics <- NULL
# extract data for behaviours
for(i in 1:nrow(fileIndexes)){

      acousticFile <- readRDS(acousticsPaths[fileIndexes$fileIndex[i]]) 

      allAcoustics <- allAcoustics %>% 
        bind_rows(., mutate(acousticFile,
                            yday = yday(localTime),
                            nDay = yday - min(yday),
                            fakeDate = as.POSIXct(paste('2020-01-0', 1 + nDay,' ', format(localTime, format = "%H:%M:%S"), sep = ''))))
  
  print(i)
}

# create plots
multipleAscents <- allAcoustics %>% 
  filter(localTime > as.POSIXct('2021-01-14 03:50:00') & localTime < as.POSIXct('2021-01-15 05:00:01')) %>% 
  ggplot(.,aes(x = localTime, y = depth, fill = biomassScore)) +
  geom_raster() +
  scale_fill_scico(#palette = 'vikO', 
    palette = 'bilbao',
    direction = 1,
    limits = c(0,1), 
    na.value = 'white',
    alpha = 0.9,
    breaks = c(0,1),
    labels = c('low','high')) +
  scale_y_continuous(breaks = c(0,-100,-200)) +
  labs(x = 'time',
       y = 'depth [m]',
       fill = 'relative backscattering strength',
       title = 'parts of the school make multiple ascents (14.01.2021)') +
  scale_x_datetime(date_labels = '%H:%M', expand = c(0.01,0.01)) +
  theme_bw() +
  theme(plot.title = element_text(size = 21, hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = 'white', colour = '#303030'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, 'lines'),
        strip.background = element_rect(fill = NA, colour = NA),
        legend.position = 'none',
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(0.2,"cm"),
        legend.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 18)) +
  guides(fill = guide_colourbar(title.position = "top"))

differingBehaviours <- allAcoustics %>% 
  filter(localTime > as.POSIXct('2021-04-21 09:00:00') & localTime < as.POSIXct('2021-04-22 18:00:01')) %>% 
  filter(depth < -10) %>% 
  ggplot(.,aes(x = localTime, y = depth, fill = biomassScore)) +
  geom_raster() +
  scale_fill_scico(#palette = 'vikO', 
    palette = 'bilbao',
    direction = 1,
    #limits = c(0,1), 
    na.value = 'white',
    alpha = 0.9,
    breaks = c(0,1),
    labels = c('low','high')) +
  scale_y_continuous(breaks = c(0,-100,-200),
                     limits = c(-210, 0)) +
  labs(x = 'time',
       y = 'depth [m]',
       fill = 'relative backscattering strength',
       title = 'two differently behaving schools merge (21.04.2021)') +
  scale_x_datetime(date_labels = '%H:%M', expand = c(0.01,0.01)) +
  theme_bw() +
  theme(plot.title = element_text(size = 21, hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = 'white', colour = '#303030'),
        panel.grid = element_blank(),
        panel.spacing = unit(2, 'lines'),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 26),
        legend.position = 'none',
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(0.2,"cm"),
        legend.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 18)) +
  guides(fill = guide_colourbar(title.position = "top"))

# arrange plots in grid
pplot <- plot_grid(multipleAscents, differingBehaviours, ncol = 1)
ggsave('plots/Figure6.pdf', pplot, width = 10, height = 6)

# ----------------------------------------------------------------------------------------------------- #
# Appendix Figure 3
winterWhales <- readRDS('data/winterWhales.RDS')
winterWhalesPlot <- winterWhales %>% 
  filter(dateTime > as.POSIXct('2021-07-04 04:00:00')) %>% 
  filter(dateTime < as.POSIXct('2021-07-05 00:00:00')) %>% 
  ggplot(.,aes(x = dateTime, y = depth, fill = relativeIntensity, colour = relativeIntensity)) +
  geom_raster() +
  scale_x_datetime(date_breaks = '4 hours', date_labels = '%H:%M') +
  scale_fill_scico(palette = 'vikO', limits = c(0,1)) +
  scale_colour_scico(palette = 'vikO', limits = c(0,1)) +
  scale_y_reverse() +
  labs(x = '', y = 'depth [m]', title = 'predators feeding on a krill school (04.07.2021)') +
  theme(panel.background = element_rect(fill = '#ffffff', colour = '#212121'),
        plot.background = element_rect(fill = '#ffffff', colour = NA),
        plot.title = element_text(size = 24, hjust = 0.5),
        legend.position = 'none',
        legend.key.width = unit(2, units = 'cm'),
        legend.key.height = unit(1, units = 'cm'),
        legend.background = element_rect(fill = '#ffffff', colour = NA),
        legend.key = element_rect(fill = '#ffffff', colour = NA),
        legend.text = element_text(size = 12, colour = '#303030'),
        legend.title = element_blank(),
        axis.text = element_text(size = 24, colour = '#454545'),
        axis.title = element_text(size = 24, colour = '#454545'))
ggsave('plots/winterWhales.png', plot = winterWhalesPlot, width = 17, height = 7)
# ----------------------------------------------------------------------------------------------------- #
# Appendix Figure 5-13, month by month
acousticBlocks <- fileDateInfos %>% 
  right_join(., tibble(date = seq(as.Date('2020-12-01'), as.Date('2021-07-31'), by = '1 day'))) %>% 
  arrange(date) %>% 
  mutate(chunkBlock = month(date)) %>% 
  group_by(chunkBlock) %>% 
  mutate(dateBlock = cut(date, '6 days')) %>% 
  ungroup() 

dateBlocks <- acousticBlocks %>% 
  distinct(chunkBlock)

allComData <- NULL
for(i in 1:nrow(dateBlocks)){
  
  # determine current dateBlock
  currentBlock <- as.character(dateBlocks$chunkBlock[i])
  
  # subset index data to determine datasets that need to be imported
  chunkIndexes <- acousticBlocks %>% 
    filter(chunkBlock == currentBlock) %>%
    distinct(dateBlock)
  
  plotlist <- NULL
  
  for(k in 1:nrow(chunkIndexes)){
    
    # create container to store acoustic data
    acousticContainer <- NULL
    
    fileIndexes <- acousticBlocks %>% 
      filter(dateBlock == chunkIndexes$dateBlock[k]) %>% 
      distinct(dateBlock, fileIndex) %>% 
      filter(!is.na(fileIndex))
    
    if(nrow(fileIndexes) > 0){
      for(j in 1:nrow(fileIndexes)){
        
        acousticFile <- readRDS(acousticsPaths[fileIndexes$fileIndex[j]]) 
        
        acousticContainer <- acousticContainer %>% 
          bind_rows(., acousticFile)
        
      }
    }
    
    minDate <- as.POSIXct(paste(min(filter(acousticBlocks, dateBlock == chunkIndexes$dateBlock[k])$date), '00:00:01'), 
                          tz = tz(acousticContainer$localTime),
                          format = '%Y-%m-%d %H:%M:%S')
    maxDate <- as.POSIXct(paste(max(filter(acousticBlocks, dateBlock == chunkIndexes$dateBlock[k])$date), '23:59:59'), 
                          tz = tz(acousticContainer$localTime), 
                          format = '%Y-%m-%d %H:%M:%S')
    
    dateBreaks <- seq(minDate, minDate + days(6), by = '12 hours')
    
    
    if(nrow(fileIndexes) > 0){
      
      acousticContainer <- acousticContainer %>% 
        filter(between(localTime, minDate, maxDate))
      
      comSubset <- acousticContainer %>%
        filter(depth < -15) %>% 
        group_by(localTime) %>%
        summarize(COM = sum(depth * correctSv, na.rm = T) / sum(correctSv, na.rm = T),
                  sunrise = mean(sunrise, na.rm = T),
                  sunset = mean(sunset, na.rm = T))
      
      allComData <- allComData %>% 
        bind_rows(., comSubset)
      
      acousticsPlot <- acousticContainer %>% 
        ggplot(.) +
        geom_raster(aes(x = localTime, y = depth, fill = biomassScore)) +
        scale_fill_scico(palette = 'bilbao', limits = c(0,1)) +
        geom_point(data = comSubset, aes(x = localTime, y = COM),
                   colour = '#1859b5',
                   size = 1.2, alpha = 0.15) +
        scale_x_datetime(limits = c(minDate, minDate + days(6)),
                         breaks = dateBreaks,
                         date_labels = '%H:%M') +
        scale_y_continuous(limits = c(-250, 0)) +
        # scale_x_datetime(date_breaks = '1 day',
        #                  date_labels = '%H:%M \n %d-%b') +
        labs(x = 'time', y = 'depth in m', fill = 'biomassScore',
             title = paste('acoustics from', as.Date(minDate), 'to', as.Date(minDate + days(5)))) +
        # geom_vline(xintercept = seq(minDate + hours(12), minDate + days(6) - hours(12), by = '24 hours')) +
        theme(panel.background = element_rect(fill = NA, colour = '#292929'),
              legend.position = 'bottom',
              legend.key.width = unit(1.7, 'cm'),
              legend.key.height = unit(0.2, 'cm'),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(hjust = 0.5, size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12)) +
        guides(fill = guide_colourbar(title.position = "top",
                                      title.hjust = 0.5))
    } else {
      acousticsPlot <- ggplot() +
        geom_raster(data = tibble(x = minDate, y = 0, fill = c(0)), 
                    aes(x = x, y = y, fill = fill), alpha = 0) +
        scale_fill_scico(palette = 'bilbao', limits = c(0,1)) +
        scale_x_datetime(limits = c(minDate, minDate + days(6)),
                         breaks = dateBreaks,
                         date_labels = '%H:%M') +
        scale_y_continuous(limits = c(-250, 0)) +
        labs(x = 'time', y = 'depth in m', fill = 'biomassScore',
             title = paste('acoustics from', as.Date(minDate), 'to', as.Date(minDate + days(5)))) +
        theme(panel.background = element_rect(fill = NA, colour = '#292929'),
              legend.position = 'bottom',
              legend.key.width = unit(1.7, 'cm'),
              legend.key.height = unit(0.2, 'cm'),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(hjust = 0.5, size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12)) +
        guides(fill = guide_colourbar(title.position = "top",
                                      title.hjust = 0.5))
    }
    plotlist[[k]] <- acousticsPlot
  }

  # save plots
  savePlot <- plot_grid(plotlist = plotlist, ncol = 1)
  ggsave(paste('plots/rawAcoustics_', i, '.pdf', sep = ''), savePlot, width = 12, height = 3.5 * length(plotlist))
  
}
 







