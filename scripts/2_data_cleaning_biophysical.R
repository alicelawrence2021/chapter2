## Cleaning site biophysical data
## Description: importing site biophysical data, creating categories using quartiles, compine to dispersion data, change levels
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 15 Jan 2024

## load libraries ------------------------------------------
install.packages("tidyverse")   # collection of packages (dplyr, ggplot2, readr tidyr etc)
install.packages("data.table")  # used for photo selection randomisation process
install.packages("vegan")       # used to calculate dispersion
install.packages("ade4")        # used to calculate dispersion

library(tidyverse) 
library(data.table)
library(vegan)
library(ade4)


# load site_env_simple data (fileEncoding="UTF-8-BOM" removes the ï..sitename)

biophys <- read.csv("data/site_metadata.csv", header = TRUE, fileEncoding="UTF-8-BOM")  

#biophys <- biophys %>%
#  dplyr::rename(sitename = ï..sitename) 

## wave mean category ----
# order wave_mean from low to high
biophysical <- setorder(biophys, cols = wave_mean)

# create quartile column 
biophysical <- within(biophysical,
                   wave_quartile <- cut (x = wave_mean,
                                         breaks = quantile(wave_mean, probs = seq(0, 1, 0.25)),
                                         labels = FALSE, 
                                         include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$wavemean_cat <- NA                    # create a new empty column named dispersion

biophysical$wavemean_cat[biophysical$wave_quartile == 1] <- "Low"
biophysical$wavemean_cat[biophysical$wave_quartile > 1 & biophysical$wave_quartile <= 3] <- "Med"
biophysical$wavemean_cat[biophysical$wave_quartile == 4] <- "High"



## wq mean category ----
# order mean_din from low to high
biophysical <- setorder(biophysical, cols = mean_din)

# create wq_quartile column 
biophysical <- within(biophysical,
                   wq_quartile <- cut (x = mean_din,
                                       breaks = quantile(mean_din, probs = seq(0, 1, 0.25)),
                                       labels = FALSE, 
                                       include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$wqmean_cat <- NA                    # create a new empty column named dispersion

biophysical$wqmean_cat[biophysical$wq_quartile == 1] <- "Low"
biophysical$wqmean_cat[biophysical$wq_quartile > 1 & biophysical$wq_quartile <= 3] <- "Med"
biophysical$wqmean_cat[biophysical$wq_quartile == 4] <- "High"


## wq max category ----
# order max_din from low to high
biophysical <- setorder(biophysical, cols = max_din)

# create wq_quartile column 
biophysical <- within(biophysical,
                   wqmax_quartile <- cut (x = max_din,
                                          breaks = quantile(max_din, probs = seq(0, 1, 0.25)),
                                          labels = FALSE, 
                                          include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$wqmax_cat <- NA                    # create a new empty column named dispersion

biophysical$wqmax_cat[biophysical$wqmax_quartile == 1] <- "Low"
biophysical$wqmax_cat[biophysical$wqmax_quartile > 1 & biophysical$wqmax_quartile <= 3] <- "Med"
biophysical$wqmax_cat[biophysical$wqmax_quartile == 4] <- "High"



## disturbed by total area category ----
# order disturbed_by_totalarea from low to high
biophysical <- setorder(biophysical, cols = disturbed_by_totalarea)

# create disturbed_quartile column 
biophysical <- within(biophysical,
                   disturbed_quartile <- cut (x = disturbed_by_totalarea,
                                              breaks = quantile(disturbed_by_totalarea, probs = seq(0, 1, 0.25)),
                                              labels = FALSE, 
                                              include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$prop_disturbed_cat <- NA                    # create a new empty column named dispersion

biophysical$prop_disturbed_cat[biophysical$disturbed_quartile == 1] <- "Low"
biophysical$prop_disturbed_cat[biophysical$disturbed_quartile > 1 & biophysical$disturbed_quartile <= 3] <- "Med"
biophysical$prop_disturbed_cat[biophysical$disturbed_quartile == 4] <- "High"


## habitat complexity category ----
# order complexity from low to high
biophysical <- setorder(biophysical, cols = complexity)

# create complexity_quartile column 
biophysical <- within(biophysical,
                   complexity_quartile <- cut (x = complexity,
                                               breaks = quantile(complexity, probs = seq(0, 1, 0.25)),
                                               labels = FALSE, 
                                               include.lowest = TRUE))

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$complexity_cat <- NA                    # create a new empty column named dispersion

biophysical$complexity_cat[biophysical$complexity_quartile == 1] <- "Low"
biophysical$complexity_cat[biophysical$complexity_quartile > 1 & biophysical$complexity_quartile <= 3] <- "Med"
biophysical$complexity_cat[biophysical$complexity_quartile == 4] <- "High"



## site steepness category ----
# order steepness from low to high
biophysical <- setorder(biophysical, cols = steepness)

# rename quartile column labels to 1= Low, 2 = Med, 3 = Med, 4 = High
biophysical$steepness_cat <- NA                    # create a new empty column named dispersion

biophysical$steepness_cat[biophysical$steepness == 2] <- "Low"
biophysical$steepness_cat[biophysical$steepness == 3] <- "Med"
biophysical$steepness_cat[biophysical$steepness == 4] <- "High"

str(biophysical)
levels(biophysical$steepness_cat)


# convert to factors

# need to convert to factors
biophysical <-
  biophysical %>%
  mutate(across (c(sitename, sector, geography, reeftype, watershed, wavemean_cat, wqmean_cat, wqmax_cat, prop_disturbed_cat, complexity_cat, steepness_cat) , as.factor))

str(biophysical)


# check order of factor levels (reorder levels from H, L, M (alphabetic) to Low, Med, High)
levels(biophysical$wavemean_cat)

biophysical$wavemean_cat <- factor(biophysical$wavemean_cat,
                                   levels = c("Low", "Med", "High"),    # use this if want different labels
                                   labels = c("Low", "Med", "High"))              

levels(biophysical$wqmean_cat) <- c("Low", "Med", "High")  
levels(biophysical$wqmax_cat) <- c("Low", "Med", "High")  
levels(biophysical$prop_disturbed_cat) <- c("Low", "Med", "High")  
levels(biophysical$complexity_cat) <- c("Low", "Med", "High")  
levels(biophysical$steepness_cat) <- c("Low", "Med", "High")  

str(biophysical)    # check if changed

# prepare data for export
# remove quartile number columns
biophysical <- biophysical [, -c(15, 17, 19, 21, 23)]

write.csv(file = "data_output/biophysical_site.csv", biophysical)





