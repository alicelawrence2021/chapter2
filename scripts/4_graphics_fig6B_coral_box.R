## Visualisation of dispersion results - CORAL GENERA
## Description: Boxplots for dispersion cateogires for kwy coral genera
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 2nd July 2024


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


#import data
coral_trans <- read.csv("data_output/benthic_transects_mean_coral.csv")

disp_cat <- read.csv("data_output/dispersion_major_site.csv")


# split site_transect column into 2 new ones
coral_trans <- coral_trans %>% 
  tidyr::separate(site_transect, c("sitename", "transect"), sep = "_", remove = FALSE) 
  

# merge dataframe with MAJOR & Disp categories
# benthic_characteristics_model <- merge(disp_major_site_model, benthic_characteristics_model, by = 'sitename', all.x=TRUE)
coral_trans_disp <- merge(disp_cat, coral_trans, by = 'sitename', all.x=TRUE)


# remove unwanted columns
coral_trans_disp <- select(coral_trans_disp, -c(2, 3, 4,5, 7)) 


# convert to factors: sitename, rank, dispersion
coral_site_trans <- 
  coral_trans_disp %>% 
  mutate(across (c(sitename, site_transect, dispersion) , as.factor))

str(coral_site_trans)
levels(coral_site_trans$sitename)
levels(coral_site_trans$dispersion)


# reorder levels in dispersion category factor #
coral_site_trans$dispersion <- factor(coral_site_trans$dispersion,
                                      levels = c('Low', 'Med', 'High'),
                                      labels = c('Low', 'Med', 'High'))

levels(coral_site_trans$dispersion)
unique(coral_site_trans$dispersion)

# summary plots - reorder x axis to L - M - H
level_orderX <- c("Low", "Med", "High")


# save to file
write.csv(file = "data_output/coral_site_trans.csv", coral_site_trans)



#### Figure 6B SUMMARY BOXPLOTS Coral - mean and SE values for each dispersion category ####

# MEAN coral genera per dispersion category

# coral_trans_disp_values <- select(coral_trans_disp, c(6, 8, 10, 11, 13:20)) # dont need this??

# coral_trans_disp_values <- group_by(coral_trans_disp_values, dispersion)

coral_trans_disp_values <- group_by(coral_trans_disp, dispersion)

coral_trans_disp_mean <- summarise_all(coral_trans_disp_values, mean)

coral_trans_disp_mean <- select(coral_trans_disp_mean, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_mean.csv", coral_trans_disp_mean)


# same for se
# create function to calculate Standard Error SE...
std.error<-function(x){
  sqrt(var(x)/length(x))
}

coral_trans_disp_se <- summarise_all(coral_trans_disp_values, std.error)

coral_trans_disp_se <- select(coral_trans_disp_se, -c(2,3,4))

write.csv(file = "data_output/coral_trans_disp_se.csv", coral_trans_disp_se)


#### Figure 4 SUMMARY PLOTS Benthic MAJOR ####

# original colour scheme - paste after geom_jitter
# viridis::scale_fill_viridis(alpha = 1, begin = 0.4, end = 0.9, option = "F", discrete=TRUE, 
#                             name = "Dispersion", labels=c("Low", "Medium", "High"), ) +

# Dispersion summary plot
# (c_disp <- ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
#                                         y = CORAL, 
#                                         fill = dispersion,
#                                         width = 0.4)) +                                    
#    geom_boxplot(width = 0.3) +
#    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+               
#    #ylim(0, 60) +
#    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
#                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +   
#    labs(x = "", 
#         y = "Mean % cover",
#         #title = "Calcifying substrates",
#         subtitle = "(i) Hard Coral") +
#    theme_bw() +
#    theme(plot.title = element_text(size = 14, hjust = 0.5), 
#          plot.subtitle = element_text(size = 14, hjust = 0),
#          text=element_text(family="sans", size=14), 
#          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
#          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
#          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
#          legend.position = "none")
#)


#### coral graphs ####
# Figure Coral genera - (MeanTotalCoral), MONTI, PORRUS, ACROP, ISOP, PAV, POC (try MERU, GAL, MONT)

# MONTI
(monti <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                        y = MONTI,
                                        fill = dispersion,
                                        width = 0.4)) +
   geom_boxplot(width = 0.3) +
   geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
   #ylim(0, 60) +
   viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                               name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
   labs(x = "",
        y = "Mean % cover",
        #title = "Calcifying substrates",
        subtitle = expression('(i)'~italic(Montipora)~'spp.')) +
   theme_bw() +
   theme(plot.title = element_text(size = 14, hjust = 0.5),
         plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
         text=element_text(family="sans", size=14),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
         axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
         axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
         legend.position = "none")
)


# all Acropora growth forms
#ACROPORA ALL
(acropall <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                          y = c(ACROTBL + ACROP +ACROPARB + ACROTBL + ACROP),
                                          fill = dispersion,
                                          width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(ii)'~italic(Acropora)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


# PAV
(pav <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                       y = PAV,
                                       fill = dispersion,
                                       width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(iii)'~italic(Pavona)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)



# POC
(poc <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                       y = POC,
                                       fill = dispersion,
                                       width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(iv)'~italic(Pocilliopora)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#ISOP
(isop <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                      y = ISOP,
                                      fill = dispersion,
                                      width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(v)'~italic(Isopora)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#PORRUS
(porrus <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                       y = PORRUS,
                                       fill = dispersion,
                                       width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         subtitle = expression('(vi)'~italic(Porites)~italic(rus)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)




#MONT
(mont <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                        y = MONT,
                                        fill = dispersion,
                                        width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(vi)'~italic(Montastrea)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#MERU
(meru <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                      y = MERU,
                                      fill = dispersion,
                                      width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(vi)'~italic(Merulina)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#GAL
(gal <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                      y = GAL,
                                      fill = dispersion,
                                      width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = expression('(vi)'~italic(Galaxea)~'spp.')) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#ACROP
(acrop <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                       y = ACROP,
                                       fill = dispersion,
                                       width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(vi) Acropora spp.") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#ACROPARB
(acroparb <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                          y = ACROPARB,
                                          fill = dispersion,
                                          width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(vi) Acropora arborescent") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#ACROST
(acrost <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                        y = ACROST,
                                        fill = dispersion,
                                        width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(vi) Acropora staghorn") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)


#ACROTBL
(acrotbl <- ggplot(coral_site_trans, aes(x = factor(dispersion, level = level_orderX),
                                         y = ACROTBL,
                                         fill = dispersion,
                                         width = 0.4)) +
    geom_boxplot(width = 0.3) +
    geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+
    #ylim(0, 60) +
    viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE,
                                name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
    labs(x = "",
         y = "Mean % cover",
         #title = "Calcifying substrates",
         subtitle = "(vi) Acropora table") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
          text=element_text(family="sans", size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
          legend.position = "none")
)




#### Figure 6B Coral genera boxplots ####
ggarrange(monti, acropall, pav, poc, isop, porrus, 
          ncol=3, nrow=2, 
          common.legend = TRUE, 
          legend="none")

ggsave("graphs/Figure6B_coral.pdf", width = 12, height = 10)
ggsave("graphs/Figure6B_coral.tiff", width = 12, height = 10)
ggsave("graphs/Figure6B_coral.png", width = 12, height = 10)











