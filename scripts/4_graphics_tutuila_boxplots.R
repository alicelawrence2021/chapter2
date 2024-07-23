## Visualisation of dispersion results - SCENE SETTING
## Description: Tutuila level data boxplots
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 15 Jan 2024


## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("gridExtra")   # Grid graphic objects
install.packages("forcats")     # 
install.packages("terra")       # for viridis colour palette?
install.packages("ggpubr")

library(tidyverse) 
library(gridExtra)  
library(forcats)
library(terra)    
library(ggpubr)



#### General scene setting - Tutuila sectors benthic % ####
# load data
benthic_transects_mean_major <- read.csv("data_output/benthic_transects_mean_major.csv")

biophysical_site <- read.csv("data_output/biophysical_site.csv")


# test sector list
unique(biophysical_site$sector)

#### select appropriate columns from each dataset ####
# select sector column only
sector_site <- select(biophysical_site, c(sitename, sector))

# remove MA column 
benthic_transects_mean_major <- select(benthic_transects_mean_major, -c(MA))


# create dataframe with benthic % cover, BSR, Dispersion, sitename, sector, site_transect
tutuila_sector_graphics <- merge(sector_site, benthic_transects_mean_major, by = "sitename")

write.csv(file = "data_output/benthic_site_sector_major.csv", tutuila_sector_graphics)


# tutuila summary per sector

tutuila_sum_sector <- group_by(tutuila_sector_graphics, sector)

tutuila_sum_sector_mean <- summarise_all(tutuila_sum_sector, mean)

tutuila_sum_sector_mean <- select(tutuila_sum_sector_mean, -c(2:5))

write.csv(file = "data_output/tutuila_sum_sector_mean.csv", tutuila_sum_sector_mean)

# same for se
# create function to calculate Standard Error SE...
std.error<-function(x){
  sqrt(var(x)/length(x))
}

tutuila_sum_sector_se <- summarise_all(tutuila_sum_sector, std.error)

tutuila_sum_sector_se <- select(tutuila_sum_sector_se, -c(2:4))

write.csv(file = "data_output/tutuila_sum_sector_se.csv", tutuila_sum_sector_se)


# tutuila summary per geography (north / south)

# tutuila_sum_geog <- group_by(tutuila, geography)
# 
# tutuila_sum_geog_mean <- summarise_all(tutuila_sum_geog, mean)
# 
# tutuila_sum_geog_mean <- select(tutuila_sum_geog_mean, -c(2:8))
# 
# write.csv(file = "data_output/tutuila_sum_geog_mean.csv", tutuila_sum_geog_mean)
# 
# # same for sd
# tutuila_sum_geog_sd <- summarise_all(tutuila_sum_geog, sd)
# 
# tutuila_sum_geog_sd <- select(tutuila_sum_geog_sd, -c(2:8))
# 
# write.csv(file = "data_output/tutuila_sum_geog_sd.csv", tutuila_sum_geog_sd)



### **************************************************************need to redo with site_transect data ************************

#### create tutuila sector boxplot ####

# change data structure
tutuila_long <- pivot_longer(tutuila_sector_graphics, cols = 6:15, 
                             names_to = "benthic_major",
                             values_to = "mean_cover")
             

# convert to factors: sector
  tutuila_long %>% 
  mutate(across (c(sector) , as.factor))

str(tutuila_long)

write.csv(file = "data_output/tutuila_long.csv", tutuila_long)


# order benthic_major by values: Coral, CCA, FCA, Turf, Macroalgae, BCA, OtherInverts, Sand, Rubble
# reorder levels in benthic_major category factor
tutuila_long$benthic_major <- factor(tutuila_long$benthic_major,
                                 levels = c('CORAL', 'CCA', 'FCA', 'TURF', 'MA_noHALI', 'BCA', 'HALI', 'OI', 'SAND', 'RUB'),
                                 labels = c('Coral', 'CCA', 'FCA', 'Turf', 'Macroalgae_nc', 'BCA', 'Halimeda', 'Other Inverts', 'Sand', 'Rubble'))

levels(tutuila_long$benthic_major)
unique(tutuila_long$benthic_major)

# sector
tutuila_long$sector <- factor(tutuila_long$sector,
                                     levels = c('NE', 'NW', 'SE', 'SW'),
                                     labels = c('NE', 'NW', 'SE', 'SW'))
levels(tutuila_long$sector)
unique(tutuila_long$sector)


# boxplot           **************************figure out why plot text sizes not changing *****************************
tut_sect <- (ggplot(tutuila_long, aes(x = benthic_major,
                                      y = mean_cover,
                                      fill = sector, 
                                      width = 0,4))) +
  geom_boxplot(position=position_dodge(width = 1)) +
  #geom_jitter(width = 0.05, colour = "grey", alpha = 0.7) + 
  scale_fill_manual(values = c("#238443", "#ffffcc", "#ae017e", "#fbb4b9"), name = "Sector", labels=c("NE", "NW", "SE", "SW") ,) + 
  labs(x = "", 
       y = "Mean % cover",
       #title = "Calcifying substrate",
       #subtitle = "(i) Hard Coral") +
  theme_bw() +
  theme(#plot.title = element_text(size = 30, hjust = 0.5), 
    #plot.subtitle = element_text(size = 20, hjust = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 28),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 28),
    legend.position = "right")) 
plot(tut_sect)


ggsave("graphs/Figure3A_tutuila.pdf", width = 10, height = 6)
ggsave("graphs/Figure3A_tutuila.tiff", width = 10, height = 6)

