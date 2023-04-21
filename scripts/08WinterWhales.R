# 08.02.2022
# Dominik Bahlburg
# show predator presence in mid July
#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Load packages used for the image- and data-processing
library(tidyverse)
library(lubridate)
library(scico)
library(foreach)
library(parallel)

# reconstruct the data from screenshots in higher resolution than default:
#list files of acoustic data, attach depth info and extract date and time stamp
# acousticDataList <- tibble(filePath = list.files('data/acoustics/AE_2021-07',pattern = '.jpg', full.names = T),
#                            fileName = list.files('data/acoustics/AE_2021-07',pattern = '.jpg', full.names = F)) #%>%
# 
# acousticDataList <- acousticDataList %>%
#   left_join(.,read_csv('data/acousticsDepthInfo.csv')) %>%
#   mutate(timeInfo = regmatches(fileName, gregexpr("[[:digit:]]+", fileName))) %>%
#   filter(!is.na(maxDepth)) %>%
#   rowwise() %>%
#   mutate(
#     DateTimeStart = as.POSIXct(paste(paste(unlist(timeInfo)[1:3], collapse = '-'),
#                                      paste(unlist(timeInfo)[4:6], collapse = ':'), sep = ' ')),
#     DateTimeStop = as.POSIXct(paste(paste(unlist(timeInfo)[7:9], collapse = '-'),
#                                     paste(unlist(timeInfo)[10:12], collapse = ':'), sep = ' '))) %>%
#   dplyr::select(filePath, maxDepth, DateTimeStart, DateTimeStop)
# 
# acousticDataList <- acousticDataList[148:207,]
# #----------------------------------------------------------------------------------#
# #create the colour palette used for data visualization by Echoview.
# #The colours were extracted from the Echoview-Software-Manual.
# colourPalette <- c('#9C8AA8', '#4E4848','#006CFF','#240CAE',
#                    '#2AD27E','#078460','#FFFF2A','#FC7831',
#                    '#FC5AA8','#FF1836','#B43C30','#962B3C')
# 
# #create palette generator which interpolates between the specified colours above
# colourPaletteGenerator <- colorRampPalette(colourPalette)
# 
# #colour tibble containing the colourscale and dummy-values (x and y) to
# #plot colourscale. It also contains the rgb-components of each colour
# colourTib <- tibble(x = seq(0,1,length.out = 600),
#                     y = 1,
#                     colour = colourPaletteGenerator(600))  %>%
#   rowwise() %>%
#   mutate(colRGB = list(col2rgb(colour)),
#          red = colRGB[[1]],
#          green = colRGB[[2]],
#          blue = colRGB[[3]])
# 
# # temporal resolution
# tempResolution <- '2 min'
# 
# # vertical resolution in meters
# depthResolution <- 2/3
# 
# # detect number of cores to paralellize processing (subtract 1 for safety)
# nCores <- parallel::detectCores() - 1
# 
# #create the cluster
# myCluster <- parallel::makeCluster(
#   nCores,
#   type = "PSOCK"
# )
# 
# #register cluster to be used by %dopar%
# doParallel::registerDoParallel(cl = myCluster)
# 
# # start the parallelized loop (it takes 12 minutes on my machine to process the 70 files)
# processedAcoustics <- foreach(i = 1:nrow(acousticDataList), .combine='rbind') %dopar% {
# 
#   acoustics <- jpeg::readJPEG(acousticDataList$filePath[i])
# 
#   #convert image into long data format, extract rgb-values for each pixel
#   acousticsColours <- tidyr::expand_grid(col = 1:ncol(acoustics),
#                                          row = 1:nrow(acoustics)) |>
#     dplyr::mutate(colour = rgb(acoustics[,,1],acoustics[,,2],acoustics[,,3],maxColorValue = 1),
#                   red = c(acoustics[,,1]) * 255,
#                   green = c(acoustics[,,2] * 255),
#                   blue = c(acoustics[,,3]) * 255)
# 
#   #----------------------------------------------------------------------------------#
#   #In this section the colours of the image are matched with the colours from
#   #the colourscale. In the following lines, each pixel is matched with all colours
#   #from the colourscale. The colours are then matched based on the smallest
#   #Euklidean distance in the rgb-colour-space.
#   acousticsColoursMatched <- acousticsColours |>
#     dplyr::rowwise() |>
#     dplyr::mutate(closestMatch = which.min(((red - colourTib$red)^2 +
#                                               (green - colourTib$green)^2 +
#                                               (blue - colourTib$blue)^2)^(1/2)),
#                   relativeIntensity = closestMatch/nrow(colourTib))
# 
#   #Add the exact colour from the legend which has been matched in the previous lines
#   #Add red, green and blue components of the colourscale-colours.
#   acousticsColoursMatched$legendColour <- colourTib$colour[acousticsColoursMatched$closestMatch]
#   acousticsColoursMatched$legendColourRed <- colourTib$red[acousticsColoursMatched$closestMatch]
#   acousticsColoursMatched$legendColourGreen <- colourTib$green[acousticsColoursMatched$closestMatch]
#   acousticsColoursMatched$legendColourBlue <- colourTib$blue[acousticsColoursMatched$closestMatch]
# 
#   #----------------------------------------------------------------------------------#
#   #add the info about the known sea floor depth at this point (extracted from image info)
#   imageDepth <- acousticDataList$maxDepth[i]
# 
#   #add depth info
#   acousticsColoursMatched <- acousticsColoursMatched |>
#     dplyr::ungroup() |>
#     dplyr::mutate(depth = row/max(row) * imageDepth) |>
#     dplyr::mutate(dateTime = acousticDataList$DateTimeStart[i] + col/max(col) *
#                     as.numeric(difftime(acousticDataList$DateTimeStop[i],acousticDataList$DateTimeStart[i],
#                                         units = 'secs')))
# 
#   #reduce temporal resolution of the data and scale pixels along y-scale accordingly
#   acousticsColoursBinned <- acousticsColoursMatched |>
#     dplyr::ungroup() |>
#     dplyr::filter(col < max(col)) |>
#     dplyr::mutate(dateTime = as.POSIXct(cut(dateTime, breaks = tempResolution)),
#                   depthBin = cut(depth, breaks = seq(0, 380, by = depthResolution), labels = F),
#                   depth = depthBin/max(depthBin) * imageDepth) |>
#     dplyr::group_by(depth, dateTime) |>
#     dplyr::summarise(relativeIntensity = mean(relativeIntensity, na.rm = T))
# 
#   return(acousticsColoursBinned)
# }
# 
# # close the cluster
# parallel::stopCluster(cl = myCluster)
# 
# write_rds(processedAcoustics, 'data/winterWhales.RDS')
#----------------------------------------------------------------------------------#
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
