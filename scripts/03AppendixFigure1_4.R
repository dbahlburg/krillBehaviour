# This script contains all code necessary to reproduce the Appendix Figures 1 and 4 of Bahlburg et al. (2023):
# Dominik Bahlburg, Lukas Hüppe,Thomas Böhrer, Sally E. Thorpe, Eugene J. Murphy, Uta Berger, Bettina Meyer:
# Plasticity and seasonality of the vertical migratory behaviour of Antarctic krill using acoustic data from fishing vessels
# Royal Society Open Science
# ------------------------------------------------------------------------------------------------------------------------------ #
# Load libraries
library(terra)
library(scico)
library(tidyverse)
library(lubridate)
library(tidyterra)
library(metR)
library(cowplot)
# ------------------------------------------------------------------------------------------------- #
# the following code shows how the raw netcdf-data were processed and summarized
# # import data (split into two parts)
# adv1 <- rast('data/advection/cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m_1674553149737.nc')
# adv2 <- rast('data/advection/cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m_1674553636659.nc')
# adv <- c(adv1,adv2)
# 
# timeStamp <- time(adv)
# depthStamp1 <- names(adv1)
# depthStamp2 <- paste(names(adv2), 2, sep = '_')
# depthStamp <- c(depthStamp1, depthStamp2)
# 
# # extract information about velocity type and depth
# depthStampSep <- str_split(depthStamp, pattern = '_')
# velocity <- unlist(lapply(depthStampSep, `[[`, 1))
# depths <- lapply(depthStampSep, `[[`, 2)
# depths <- str_split(depths, pattern = '=')
# depths <- as.numeric(unlist(lapply(depths, `[[`, 2)))
# 
# Define polygons for extracting regions from our analyses
# southOrkneysShelf <- ext(-47.460514, -44.179801, -60.919087, -60.293618)
# southOrkneysOpenOcean <- ext(-45.522265,-43.167934,-59.937816,-58.696922)
# peninsulaSouth <- ext(-63.261465,-61.271641,-64.829086,-64.142963)
# peninsulaNorth <- ext(-60.667207, -56.797494, -63.886644, -62.380916)
# 
# advSouthOrkneysShelf <- crop(adv, southOrkneysShelf)
# advSouthOrkneysOpenOcean <- crop(adv, southOrkneysOpenOcean)
# advGerlache <- crop(adv, peninsulaSouth)
# advBransfield <- crop(adv, peninsulaNorth)
# 
# # transform advection data for each region into dataframe
# # filter the time periods during which data were actually collected in each region
# advSouthOrkneysOpenOceanDf <- as.data.frame(advSouthOrkneysOpenOcean, na.rm = F, xy=T) %>%
#   pivot_longer(cols = -c(x,y)) %>%
#   mutate(date = rep(timeStamp, times = dim(advSouthOrkneysOpenOcean)[1] * dim(advSouthOrkneysOpenOcean)[2]),
#          velocityType = rep(velocity, times = dim(advSouthOrkneysOpenOcean)[1] * dim(advSouthOrkneysOpenOcean)[2]),
#          depth = rep(depths, times = dim(advSouthOrkneysOpenOcean)[1] * dim(advSouthOrkneysOpenOcean)[2])) %>%
#   filter(!is.na(value)) %>%
#   dplyr::select(x,y,date,velocityType,depth,value)  %>%
#   distinct(x,y,date,velocityType,depth, value) %>%
#   pivot_wider(id_cols = c(x,y,date,depth), names_from = velocityType, values_from = value) %>%
#   mutate(totalVelo = sqrt(uo^2 + vo^2),
#          region = 'South Orkney Islands \n Open Ocean') %>%
#   filter(between(date, as.POSIXct('2020-12-01 00:00:00'), as.POSIXct('2020-12-11 23:59:59')))
# 
# advSouthOrkneysShelfDfI <- as.data.frame(advSouthOrkneysShelf, na.rm = F, xy=T) %>%
#   pivot_longer(cols = -c(x,y)) %>%
#   mutate(date = rep(timeStamp, times = dim(advSouthOrkneysShelf)[1] * dim(advSouthOrkneysShelf)[2]),
#          velocityType = rep(velocity, times = dim(advSouthOrkneysShelf)[1] * dim(advSouthOrkneysShelf)[2]),
#          depth = rep(depths, times = dim(advSouthOrkneysShelf)[1] * dim(advSouthOrkneysShelf)[2])) %>%
#   filter(!is.na(value)) %>%
#   dplyr::select(x,y,date,velocityType,depth,value) %>%
#   distinct(x,y,date,velocityType,depth, value) %>%
#   pivot_wider(id_cols = c(x,y,date,depth), names_from = velocityType, values_from = value) %>%
#   mutate(totalVelo = sqrt(uo^2 + vo^2),
#          region = 'South Orkney Islands \n Shelf') %>%
#   filter(between(date, as.POSIXct('2020-12-12 00:00:00'), as.POSIXct('2020-12-26 23:59:59')) |
#          between(date, as.POSIXct('2021-01-12 00:00:00'), as.POSIXct('2021-03-18 23:59:59')) |
#          between(date, as.POSIXct('2021-06-25 00:00:00'), as.POSIXct('2021-07-31 23:59:59')))
# 
# advGerlacheDf <- as.data.frame(advGerlache, na.rm = F, xy=T) %>%
#   pivot_longer(cols = -c(x,y)) %>%
#   mutate(date = rep(timeStamp, times = dim(advGerlache)[1] * dim(advGerlache)[2]),
#          velocityType = rep(velocity, times = dim(advGerlache)[1] * dim(advGerlache)[2]),
#          depth = rep(depths, times = dim(advGerlache)[1] * dim(advGerlache)[2])) %>%
#   filter(!is.na(value)) %>%
#   dplyr::select(x,y,date,velocityType,depth,value) %>%
#   distinct(x,y,date,velocityType,depth, value) %>%
#   pivot_wider(id_cols = c(x,y,date,depth), names_from = velocityType, values_from = value) %>%
#   mutate(totalVelo = sqrt(uo^2 + vo^2),
#          region = 'Gerlache Strait') %>%
#   filter(between(date, as.POSIXct('2021-04-05 00:00:00'), as.POSIXct('2021-05-07 23:59:59')))
# 
# advBransfieldDf <- as.data.frame(advBransfield, na.rm = F, xy=T) %>%
#   pivot_longer(cols = -c(x,y)) %>%
#   mutate(date = rep(timeStamp, times = dim(advBransfield)[1] * dim(advBransfield)[2]),
#          velocityType = rep(velocity, times = dim(advBransfield)[1] * dim(advBransfield)[2]),
#          depth = rep(depths, times = dim(advBransfield)[1] * dim(advBransfield)[2])) %>%
#   filter(!is.na(value)) %>%
#   dplyr::select(x,y,date,velocityType,depth,value) %>%
#   distinct(x,y,date,velocityType,depth, value) %>%
#   pivot_wider(id_cols = c(x,y,date,depth), names_from = velocityType, values_from = value) %>%
#   mutate(totalVelo = sqrt(uo^2 + vo^2),
#          region = 'Bransfield Strait') %>%
#   filter(between(date, as.POSIXct('2020-12-30 00:00:00'), as.POSIXct('2021-01-10 23:59:59')) |
#            between(date, as.POSIXct('2021-03-23 00:00:00'), as.POSIXct('2021-03-26 23:59:59')) |
#            between(date, as.POSIXct('2021-05-07 00:00:00'), as.POSIXct('2021-06-03 23:59:59')))
# 
# # merge all data
# advectionAllRegions <- advSouthOrkneysOpenOceanDf %>%
#   bind_rows(., advSouthOrkneysShelfDfI) %>%
#   bind_rows(., advGerlacheDf) %>%
#   bind_rows(., advBransfieldDf)
# 
# # calculate season- and region-specific velocity gradients
# velocityGradient <- advectionAllRegions %>% 
#   mutate(season = ifelse(between(month(date), 3, 5), 'autumn', 
#                          ifelse(between(month(date), 6,8), 'winter', 'summer'))) %>% 
#   group_by(region, season, depth) %>% 
#   summarise(meanVelo = mean(totalVelo, na.rm = T),
#             sdVelo = sd(totalVelo, na.rm = T))
# 
# # save file
# saveRDS(velocityGradient, 'data/velocityGradient.RDS')
# ------------------------------------------------------------------------------------------------- #
# import coastline and change projection
velocityGradient <- readRDS('data/velocityGradient.RDS')
coastline <- vect('data/coastline/add_coastline_medium_res_polygon_v7_4/add_coastline_medium_res_polygon_v7_4.shp')
coastline <- terra::project(coastline, 'epsg:4326')
wholeArea <- crop(coastline, ext(-69, -40,-66, -56))

# show vertical velocity gradients
velocityGradientPlot <- velocityGradient %>% 
  ggplot(.) +
  geom_line(aes(y = meanVelo, x = depth, colour = factor(season, levels = c('summer','autumn','winter'))),
            linewidth = 2) +
  facet_grid(~region, scales = 'free_x') +
  scale_colour_manual(values = c('#f54242','#eba61c','#3a75cf')) +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'depth [m]', y = expression(current~velocity~m~s^{-1}),
       colour = '') +
  theme(panel.background = element_rect(fill = '#ffffff', colour = '#212121'),
        plot.background = element_rect(fill = '#ffffff', colour = NA),
        legend.position = 'bottom',
        legend.key.width = unit(0.5, units = 'cm'),
        legend.key.height = unit(1, units = 'cm'),
        legend.background = element_rect(fill = '#ffffff', colour = NA),
        legend.key = element_rect(fill = '#ffffff', colour = NA),
        legend.text = element_text(size = 24, colour = '#303030'),
        legend.title = element_text(size = 24, colour = '#454545'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 24, colour = '#454545'),
        axis.text = element_text(size = 24, colour = '#454545'),
        axis.title = element_text(size = 24, colour = '#454545'))
ggsave('plots/velocityGradient.pdf', velocityGradientPlot, width = 16, height = 7)

# create overview map showing the polygons that were used to extract the advection data
southOrkneysShelf <- ext(-47.460514, -44.179801, -60.919087, -60.293618)
southOrkneysOpenOcean <- ext(-45.522265,-43.167934,-59.937816,-58.696922)
peninsulaSouth <- ext(-63.261465,-61.271641,-64.829086,-64.142963)
peninsulaNorth <- ext(-60.667207, -56.797494, -63.886644, -62.380916)

overviewMap <- ggplot() +
  geom_spatvector(data = wholeArea, size = 0.1, 
                  colour = NA, fill = '#6e6e6e') +
  geom_spatvector(data = as.polygons(peninsulaNorth, crs = 'epsg:4326'), fill = NA, colour = '#fc4e1e', size = 1) +
  geom_spatvector(data = as.polygons(peninsulaSouth, crs = 'epsg:4326'), fill = NA, colour = '#fc4e1e', size = 1) +
  geom_spatvector(data = as.polygons(southOrkneysOpenOcean, crs = 'epsg:4326'), fill = NA, colour = '#fc4e1e', size = 1) +
  geom_spatvector(data = as.polygons(southOrkneysShelf, crs = 'epsg:4326'), fill = NA, colour = '#fc4e1e', size = 1) +
  scale_y_continuous(limits = c(-65, -58), breaks = c(-65,-62,-59)) +
  scale_x_continuous(limits = c(-65, -40), breaks = c(-65,-55,-45)) +
  theme(panel.background = element_rect(fill = '#ffffff', colour = '#212121'),
        plot.background = element_rect(fill = '#ffffff', colour = NA),
        legend.position = 'bottom',
        legend.key.width = unit(0.5, units = 'cm'),
        legend.key.height = unit(1, units = 'cm'),
        legend.background = element_rect(fill = '#ffffff', colour = NA),
        legend.key = element_rect(fill = '#ffffff', colour = NA),
        legend.text = element_text(size = 24, colour = '#303030'),
        legend.title = element_text(size = 24, colour = '#454545'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 24, colour = '#454545'),
        axis.text = element_text(size = 24, colour = '#454545'),
        axis.title = element_text(size = 24, colour = '#454545'))
ggsave('plots/overviePolygonswMap.pdf', overviewMap, width = 12, height = 8)
# ------------------------------------------------------------------------------------------- #
# Visualize sea ice concentration during winter (data: 10.48670/moi-00169)
southOrkneys <- crop(coastline, ext(-49, -41,-62, -58))

bathy <- rast('~/github/SOdata/IBCSO_v2_ice-surface_WGS84.nc')
southOrkneysBathy <- crop(bathy, ext(-49, -41,-62, -58))
southOrkneysBathy <- as.data.frame(southOrkneysBathy, xy=T) %>% 
  mutate(elevation = ifelse(z>0,0,z))

# sea ice edge contour line for >0.15 sea ice concentration
seaIce <- rast('data/seaIceData.nc')
seaIce <- crop(seaIce, ext(-49, -41,-62, -58))
seaIceTime <- time(seaIce)
seaIceInd <- which(between(seaIceTime, as.POSIXct('2021-06-25 00:00:00'), as.POSIXct('2021-07-29 00:00:00')))
seaIce <- subset(seaIce, seaIceInd)
seaIceDf <- as.data.frame(seaIce, xy=T) 
names(seaIceDf)[3:(2 + length(seaIceInd))] <- as.character(seaIceTime[seaIceInd])
seaIceDf <- seaIceDf %>% 
  pivot_longer(-c(x, y), names_to = 'date') %>% 
  mutate(date = as.POSIXct(date),
         dateUTC = force_tz(date, 'UTC'))

seaIceDfWeekly <- seaIceDf %>% 
  filter(dateUTC %in% seq(as.POSIXct('2021-06-27 12:00:00'), as.POSIXct('2021-07-25 12:00:00'), '7 days'))

positionWinter <- read.csv('~/github/krillBehaviour/data/behaviourData.csv') %>% 
  mutate(localTime = as.POSIXct(localTime, format = "%Y-%m-%d %H:%M:%S"),
         localTime = force_tz(localTime, "Etc/GMT+3"),
         localTime = round_date(localTime, unit = 'minute'),
         date = as.Date(as.POSIXct(localTime), tz = 'Etc/GMT+3'),
         sunrise = force_tz(as.POSIXct(sunrise), "Etc/GMT+3"),
         sunset = force_tz(as.POSIXct(sunset), "Etc/GMT+3")) %>% 
  filter(lat < -57.5) %>% 
  distinct(localTime, lat, long) %>% 
  mutate(date = as.Date(localTime, tz = 'Etc/GMT+3')) %>% 
  filter(between(localTime, as.POSIXct('2021-06-25 00:00:00'), as.POSIXct('2021-07-29 00:00:00')))
  
iceEdges <- c(0.15, 0.5, 0.85)
plotlist <- list()

for(i in 1:length(iceEdges)){
  
plotlist[[i]] <- ggplot() +
  geom_raster(data = southOrkneysBathy, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = '#616161', high = '#fafafa',
                      limits = c(-6000, 0),
                      breaks = seq(-6000, 0, by = 2000)) +
  geom_spatvector(data = southOrkneys, size = 0.1, 
                  colour = NA, fill = '#6e6e6e') +
  geom_contour(data = seaIceDfWeekly,
               aes(x = x, y = y, group = date, colour = date, z = value), 
               breaks = iceEdges[i],
               size = 1.25) +
  metR::geom_text_contour(data = mutate(seaIceDfWeekly, 
                                  lineLabel = paste(day(date), month(date, abbr = T, label = T))),
                    aes(x = x, y = y, group = date, colour = date, z = value, label = lineLabel), 
                    breaks = iceEdges[i],
                    label.placer = metR::label_placer_fraction(
                      frac = 0.75,
                      rot_adjuster = isoband::angle_halfcircle_bottom()
                    ),
                    size = 7,
                    nudge_y = 0.12,
                    nudge_x = 0.12,
                    skip = 0) +
  geom_path(data = positionWinter, aes(x = long, y = lat), size = 1, colour = '#c25425') +
  scale_colour_gradientn(colors = viridis::viridis(10),
                         limits = c(as.numeric(as.POSIXct('2021-06-25 00:00:00')), 
                                    as.numeric(as.POSIXct('2021-07-28 00:00:00'))),
                         breaks = c(seq(as.numeric(as.POSIXct('2021-06-25 00:00:00')), 
                                        as.numeric(as.POSIXct('2021-07-28 00:00:00')),
                                        by = 86400 * 7)),
                         labels = seq(as.POSIXct('2021-06-25 00:00:00'), 
                                      as.POSIXct('2021-07-28 00:00:00'),
                                      '7 days'),
                         guide = 'none') +
  scale_x_continuous(breaks = c(-48,-45, -42)) +
  scale_y_continuous(breaks = c(-62,-60, -58)) +
  labs(x = '', y = '', 
       title = paste('isolines: ', iceEdges[i] * 100, ' % ice cover', sep = '')) +
  theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        plot.title = element_text(size = 25, hjust = 0.5),
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


legendPlot <- get_legend(plotlist[[i]] + theme(legend.key.width=unit(3,"cm"),
                                             legend.key.height=unit(0.4,"cm"),
                                             legend.text = element_text(size = 22),
                                             legend.title = element_blank(),
                                             legend.position = 'bottom') +
                           guides(fill = guide_colourbar(title.position = "top")))
  print(i)
}
prow <- plot_grid(plotlist = plotlist, ncol = 3)
seaIcePlots <- plot_grid(prow, NULL, legendPlot, ncol = 1, rel_heights = c(1,0.1,0.1))
ggsave('plots/seaIce2.pdf', plot = seaIcePlots, width = 22, height = 7)




