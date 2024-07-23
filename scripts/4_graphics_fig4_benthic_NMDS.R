## Loading ggvegan and creating nMDS
## Description: 
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 24th November 2023


#### load libraries ####
# ggvegan not available on CRAN

install.packages("remotes")

remotes::install_github("gavinsimpson/ggvegan")

library(tidyverse)
library(vegan)
library(ggvegan)


#### how to change graphic font ####

install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()

# and then add to your ggplot:
# theme(text=element_text(family="Arial", size=12) # try 12 or 14 to see how they x and y labs fit

#### creating nMDS with ggvegan ####

# use 'metaMDS' function

# weblink: https://rstudio-pubs-static.s3.amazonaws.com/618343_95ac7f2f95af4866bbc876c91a75d8dc.html

#  imagine NMDS as a reduction of axes, where all your “axes” are the species within a sample, and each sample exists relative to others on the axes.


#### load data ####
major_site_trans <- read.csv("data_output_old/benthic_transects_mean_major.csv", header = TRUE)  

# remove MA_noHALI
major_site_trans <- select(major_site_trans, c(-MA_noHALI))


# dispersion data
dispersion <- read.csv("data/dispersion_transect.csv", header = TRUE)  

# change column name ... dispersion to Dispersion
dispersion <- dispersion %>%
  dplyr::rename(Dispersion = dispersion)

str(dispersion)

# reorder levels in dispersion category factor 
dispersion$Dispersion <- factor(dispersion$Dispersion,
                                      levels = c('High', 'Med', 'Low'),
                                      labels = c('High', 'Med', 'Low'))

levels(dispersion$Dispersion)
unique(dispersion$Dispersion)

# change column name ï..sitename to sitename
# dispersion <- dispersion %>%
#   dplyr::rename(sitename = ï..sitename)


# set up a "metadata"frame - for plotting later ********can add more abiotic factors later *************************
dispersion_cat <- dispersion %>% 
  select(site_transect, Dispersion)


# change rownames to site_transect names
rownames(major_site_trans) <- major_site_trans$site_transect 


# remove unwanted columns
major_site_trans <- select(major_site_trans, -c(1:4))        # remove blank columns 


#### create nMDS ####
major_nmds <- metaMDS(major_site_trans)

major_nmds    # stress 0.1748 - best solution not repeated after 20 tries

#### stressplot ####

# look at stressplot to evaluate how well the ordination represented the complexity in your data
# the x-axis is the observed dissimilarity, and the y-axis is the ordination distance
# The stressplot shows you how closely the ordination (y-axis) represents the dissimilarities calculated (x-axis)
# The points around the red stair steps are the communities, and the distance from the line represents the “stress”, 
# or how they are pulled from their original position to be represented in their ordination.

stressplot(major_nmds)


# plot NMDS output in base R
plot(major_nmds)


# extract elements from the output to plot in ggplot
plot_df <- scores(major_nmds, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site_transect") %>% 
  full_join(dispersion_cat, by = "site_transect")

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Dispersion)) +
  geom_point(size = 3, alpha = 0.8) +
  # scale_color_manual(values = dispersion) +
  #   clean_background +
  stat_ellipse(linetype = 2, size = 1) +
  labs(title = "NMDS")

plot_nmds


# adding overlays
# envfit() takes the output of metaMDS() and the species matrix you created
fit <- envfit(major_nmds, major_site_trans, perm = 999) 

# extract p-values for each species
fit_pvals <- fit$vectors$pvals %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  dplyr::rename("pvals" = ".")


# extract coordinates for species, only keep species with p-val = 0.001
fit_spp <- fit %>% 
  scores(., display = "vectors") %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  full_join(., fit_pvals, by = "species") %>% 
  filter(pvals == 0.001)

#test   scale_color_manual(values = c("#e31a1c", "#fecc5c", "#fd8d3c")) +

# new plot
(nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(size = "CORAL", color = Dispersion), size = 3, alpha = 0.8) +      # size not working
  stat_ellipse(aes(color = Dispersion)) +
  #scale_color_manual(values = dispersion) +
  scale_color_manual(values = c("#D55E00", "#56B4E9", "#999999")) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black")+
  geom_text(data = fit_spp, aes(label = species)) +
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size =14), 
        axis.title.x = element_text(color = "grey20", size = 14, margin=ggplot2::margin(t=12)),
        axis.text.y = element_text(color = "grey20", size = 14), 
        axis.title.y = element_text(color = "grey20", size = 14, margin=ggplot2::margin(r=12)),
        axis.text = element_text(family="Arial", size=14), 
        plot.title = element_text(color = "grey20", size = 14,face="bold",hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.text=element_text(family="sans", size=14), legend.title=element_text(size=14), legend.position="right") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
)



ggsave("graphs/Figure4v3_nmds.tiff", width = 12, height = 8)
ggsave("graphs/Figure4v3_nmds.pdf", width = 12, height = 8, dpi=300)


# ### example from https://stackoverflow.com/questions/59153955/different-font-sizes-for-different-portions-of-text-label-in-ggplot
# theme(axis.text.x = element_text(color = "grey20", size =11), 
# axis.title.x = element_text(color = "grey20", size = 14, face="bold", margin=ggplot2::margin(t=12)),
# axis.text.y = element_text(color = "grey20", size = 11), 
# axis.title.y = element_text(color = "grey20", size = 14, face="bold", margin=ggplot2::margin(r=12)),
# legend.key = element_rect(fill = "white"),
# plot.title = element_text(color = "grey20", size = 18,face="bold",hjust = 0.5),
# plot.subtitle = element_text(hjust = 0.5),
# legend.text=element_text(size=12), legend.title=element_text(size=14), legend.position="top") +
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))
  

  
  
  
  
  
  
