library(tidyverse)
library(lubridate)
library(scico)
library(terra)
library(tidyterra)

acousticsPaths <- list.files('~/github/krillBehaviour/data/krillAcousticsProcessed/Data/processedFiles', pattern = 'RDS', full.names = T)

krillBehaviour <- read.csv('~/github/krillBehaviour/data/qualitativeBehaviourConsensus.csv', sep = ';') %>% 
  mutate(date = as.Date(date, format = '%d.%m.%y'),
         behaviour = factor(behavior, 
                            levels = c('constantSurface','shallowDVM','reverseDVM','DVM12h','DVM',
                                       'diffuseDVM','deepDiffuseDVM','deepDVM'))) %>% 
  dplyr::select(date, behavior, behavBlock) %>% 
  mutate(behavior = ifelse(date == as.Date('2021-02-08'), 'shallowDVM', behavior))

COMData <- readRDS('~/github/krillBehaviour/data/krillAcousticsProcessed/timeSeriesMetrics.RDS') %>% 
  mutate(date = as.Date(localTime))

# Visualize different behavioural classes:
krillBehaviourSelected <- krillBehaviour %>% 
  distinct(behavior) %>% 
  filter(!is.na(behavior)) %>% 
  mutate(startDate = as.POSIXct(paste(c('2021-02-06','2021-04-29', '2021-02-22',
                                        '2021-01-16','2021-04-13', '2021-05-15',
                                        '2021-07-11'), '00:00:01', sep = ' '), tz = 'Etc/GMT+3'),
         endDate = startDate + days(2) - seconds(2),
         behaviourTitle = c('DVM-100m (06-07.02.2021)', 'constant surface (29-30.04.2021)',
                            'DVM-150m (22-23.02.2021)','DVM 12h (16-17.01.2021)',
                            'reverse DVM (13-14.04.2021)','diffuse DVM (15-16.05.2021)','deep DVM (11-12.07.2021)')) %>% 
  gather(dateType, dateTime, -behavior, -behaviourTitle) %>% 
  mutate(date = as.Date(dateTime, tz = 'Etc/GMT+3'))


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

timeline <- tibble(date = seq(as.Date('2020-12-01'),as.Date('2021-07-31'), by = '1 day')) %>% 
  left_join(.,fileDateInfos) %>% 
  left_join(.,krillBehaviourSelected) %>% 
  filter(!is.na(behavior)) 

behaviours <- distinct(timeline, behavior, behaviourTitle)

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
                            levels = c('constant surface (29-30.04.2021)', 'reverse DVM (13-14.04.2021)',
                                       'DVM-100m (06-07.02.2021)', 'DVM-150m (22-23.02.2021)',
                                       'DVM 12h (16-17.01.2021)','diffuse DVM (15-16.05.2021)','deep DVM (11-12.07.2021)'))) 
behavPlot <- allAcoustics %>% 
  mutate(behavior = factor(behavior, 
                           levels = c('constant surface (29-30.04.2021)', 'reverse DVM (13-14.04.2021)',
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
  facet_wrap(~behavior, ncol = 2) +
  geom_vline(xintercept = as.POSIXct(c('2020-01-01 12:00:00','2020-01-02 12:00:00')),
             colour = '#616161',
             size = 1.2,
             linetype = '22') +
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
ggsave('~/github/krillBehaviour/plots/Figure1.pdf', dpi=70, plot = behavPlot, width = 19, height = 20)

# ----------------------------------------------------------------------------------------------------- #
# Extract two behavioral examples where it is visible that krill individuals can behave differently under the same
# environmental conditions

# Visualize different behavioural classes:
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


p1 <- allAcoustics %>% 
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



p3 <- allAcoustics %>% 
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

pplot <- plot_grid(p1,p3, ncol = 1)
ggsave('plots/schoolDivergence.pdf', pplot, width = 10, height = 6)






