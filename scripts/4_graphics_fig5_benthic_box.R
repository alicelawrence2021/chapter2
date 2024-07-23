## Figure 4B & 4C Benthic Dispersion category summary boxplots
## Description: Summary boxplots for dispersion categories - MAJOR & BSR
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 17 Jan 2024


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

#### how to change graphic font ####

install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()

windowsFonts()


# and then add to your ggplot:
# theme(text=element_text(family="Arial", size=12) # try 12 or 14 to see how they x and y labs fit


# import data
major_trans <- read.csv("data_output/benthic_transects_mean_major.csv")

disp_cat <- read.csv("data_output/dispersion_major_site.csv")


# merge dataframe with MAJOR & Disp categories
# benthic_characteristics_model <- merge(disp_major_site_model, benthic_characteristics_model, by = 'sitename', all.x=TRUE)
major_trans_disp <- merge(disp_cat, major_trans, by = 'sitename', all.x=TRUE)


# calculate and add BSR column 
major_site_trans <- mutate(major_trans_disp, BSR = ((CORAL + CCA + BCA + HALI) / (MA_noHALI + TURF + FCA)))


# convert to factors: sitename, rank, dispersion
major_site_trans <- 
  major_site_trans %>% 
  mutate(across (c(sitename, site_transect, dispersion) , as.factor))


# checks
str(major_site_trans)

levels(major_site_trans$sitename)


# summary plots - reorder x axis to L - M - H
level_orderX <- c("Low", "Med", "High")

# reorder levels in dispersion category factor ######## *********************************not working *********************
major_site_trans$dispersion <- factor(major_site_trans$dispersion,
                                 levels = c('Low', 'Med', 'High'),
                                 labels = c('Low', 'Med', 'High'))

levels(major_trans$dispersion)
unique(major_trans$dispersion)

# save to file
write.csv(file = "data_output/major_site_trans.csv", major_site_trans)


#### Figure 4 SUMMARY PLOTS Benthic MAJOR - mean and SE values for each dispersion category ####

# MEAN benthic major per dispersion category

major_trans_disp_values <- select(major_trans_disp, c(6, 8, 10, 11, 13:20))

major_trans_disp_values <- group_by(major_trans_disp_values, dispersion)

major_trans_disp_mean <- summarise_all(major_trans_disp_values, mean)

major_trans_disp_mean <- select(major_trans_disp_mean, -c(2))

write.csv(file = "data_output/major_trans_disp_mean.csv", major_trans_disp_mean)

# same for se
# create function to calculate Standard Error SE...
std.error<-function(x){
  sqrt(var(x)/length(x))
}

major_trans_disp_se <- summarise_all(major_trans_disp_values, std.error)

major_trans_disp_se <- select(major_trans_disp_se, -c(2))

write.csv(file = "data_output/major_trans_disp_se.csv", major_trans_disp_se)


#### Figure 4 SUMMARY PLOTS Benthic MAJOR ####

# original colour scheme - paste after geom_jitter
# viridis::scale_fill_viridis(alpha = 1, begin = 0.4, end = 0.9, option = "F", discrete=TRUE, 
#                             name = "Dispersion", labels=c("Low", "Medium", "High"), ) +

# Dispersion summary plot
(c_disp <- ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                        y = CORAL, 
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
                  subtitle = "(i) Hard Coral") +
             theme_bw() +
             theme(plot.title = element_text(size = 14, hjust = 0.5), 
                   plot.subtitle = element_text(size = 14, hjust = 0),
                   text=element_text(family="sans", size=14), 
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                   axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                   axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                   legend.position = "none")
)


#### turf ####
# Dispersion summary plot
(ta_disp <- ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                         y = TURF, 
                                         fill = dispersion,
                                         width = 0.4)) +
              geom_boxplot(width = 0.3) +
              geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
              viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                          name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
              labs(x = "", 
                   y = "Mean % cover",
                   #title = "Dispersion categories",
                   subtitle = "(v) Turf algae") +
              theme_bw() +
              theme(plot.title = element_text(size = 14, hjust = 0.5), 
                    plot.subtitle = element_text(size = 14, hjust = 0),
                    text=element_text(family="sans", size=14), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                    legend.position = "none")
)


#### cca ####
# Dispersion summary plot
(cca_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                          y = CCA, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "", 
                    y = "Mean % cover",
                    # title = "Dispersion categories",
                    subtitle = "(ii) Crustose Coralline Algae (CCA)") +
               theme_bw() +
               theme(plot.title = element_text(size = 14, hjust = 0.5), 
                     plot.subtitle = element_text(size = 14, hjust = 0),
                     text=element_text(family="sans", size=14), 
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                     legend.position = "none")) 
)


#### macroalgae ####
# Dispersion summary plot
(ma_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                         y = MA_noHALI, 
                                         fill = dispersion,
                                         width = 0.4)) +
              geom_boxplot(width = 0.3) +
              geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
              viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                          name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
              labs(x = "", 
                   y = "Mean % cover",
                   # title = "Dispersion categories",
                   subtitle = "(vi) Macroalgae (non-calcified)") +
              theme_bw() +
              theme(plot.title = element_text(size = 14, hjust = 0.5), 
                    plot.subtitle = element_text(size = 14, hjust = 0),
                    text=element_text(family="sans", size=14), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                    legend.position = "none")) 
)


#### halimeda ####
# Dispersion summary plot
(ha_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                         y = HALI, 
                                         fill = dispersion,
                                         width = 0.4)) +
              geom_boxplot(width = 0.3) +
              geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
              viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                          name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
              labs(x = "", 
                   y = "Mean % cover",
                   # title = "Dispersion categories",
                   subtitle = expression('(iv)'~italic(Halimeda)~'spp.')) +
              theme_bw() +
              theme(plot.title = element_text(size = 14, hjust = 0.5), 
                    plot.subtitle = element_text(size = 14, hjust = 0, face = "italic"),
                    
                    text=element_text(family="sans", size=14), 
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                    legend.position = "none")) 
)


#### bca ####
# Dispersion summary plot
(bca_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                          y = BCA, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "", 
                    y = "Mean % cover",
                    #title = "",
                    subtitle = "(iii) Branching coralline algae") +
               theme_bw() +
               theme(plot.title = element_text(size = 14, hjust = 0.5), 
                     plot.subtitle = element_text(size = 14, hjust = 0),
                     text=element_text(family="sans", size=14), 
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                     legend.position = "none")) 

)


#### fca ####
# Dispersion summary plot
(fca_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                          y = FCA, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "", 
                    y = "Mean % cover",
                    #title = "Non-calcifying substrates",
                    subtitle = "(vii) Fleshy coralline algae") +
               theme_bw() +
               theme(plot.title = element_text(size = 14, hjust = 0.5), 
                     plot.subtitle = element_text(size = 14, hjust = 0),
                     text=element_text(family="sans", size=14), 
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 14),
                     axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                     legend.position = "none")) 
)


#### BSR ####
# Dispersion summary plot
(bsr_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                          y = BSR, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               #ylim(0, 60) +
               viridis::scale_fill_viridis(alpha = 1, begin = 0.9, end = 0.4, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "", 
                    y = "Mean % cover calcifying:non-calcifying",
                    #title = "Dispersion categories",
                    subtitle = "(viii) Benthic Substrate Ratio (BSR)") +
               theme_bw() +
               theme(plot.title = element_text(size = 14, hjust = 0.5), 
                     plot.subtitle = element_text(size = 14, hjust = 0),
                     text=element_text(family="sans", size=14), 
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
                     legend.position = "none")) 
)

#### BSR - no Amanave 5 ####

# remove Amanave 5 row
# updated_myData <- subset(myData, id!= 6)
# print (updated_myData)
#major_site_trans_noAman <- subset(major_site_trans, id!= "Amanave_5") ############ *******error ############

major_site_trans_noaman <- read.csv("data_output/major_site_trans_noaman.csv")


# Dispersion summary plot
(bsr_disp_noaman <- (ggplot(major_site_trans_noaman, aes(x = factor(dispersion, level = level_orderX), 
                                          y = BSR, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               #ylim(0, 60) +
               viridis::scale_fill_viridis(alpha = 1, begin = 0.4, end = 0.9, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "", 
                    y = "Mean % cover calcifying:non-calcifying",
                    #title = "Dispersion categories",
                    subtitle = "(viii) Benthic Substrate Ratio (BSR)") +
               theme_bw() +
               theme(plot.title = element_text(size = 14, hjust = 0.5), 
                     plot.subtitle = element_text(size = 14, hjust = 0),
                     text=element_text(family="sans", size=14), 
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 14),
                     axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
                     axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 12),
                     legend.position = "none")) 
)

#### ggarrange - summary - MAJOR ####

ggarrange(c_disp, cca_disp, bca_disp, ha_disp, ta_disp, ma_disp, fca_disp, bsr_disp_noaman, 
          ncol=2, nrow=4, 
          common.legend = TRUE, 
          legend="none")

ggsave("graphs/Figure5_summary.pdf", width = 10, height = 16)
ggsave("graphs/Figure5_summary.tiff", width = 10, height = 16)
ggsave("graphs/Figure5_summary.png", width = 10, height = 16)


# 
# #### Figure 1b_land SUMMARY PLOTS Benthic Major #### - change to landscape
# ggarrange(c_disp, cca_disp, bca_disp, fca_disp, ta_disp, ma_disp, 
#           ncol=3, nrow=2, 
#           common.legend = TRUE, 
#           legend="bottom")
# 
# ggsave("graphs/Figure5v2_summary.pdf", width = 6, height = 4)
# ggsave("graphs/Figure5v2_summary.tiff", width = 6, height = 4)




#### BSR ####
# Dispersion summary plot
(bsr_disp <- (ggplot(major_site_trans, aes(x = factor(dispersion, level = level_orderX), 
                                          y = BSR, 
                                          fill = dispersion,
                                          width = 0.4)) +
               geom_boxplot(width = 0.3) +
               geom_jitter(width = 0.05, colour = "grey", alpha = 0.7)+ 
               #ylim(0, 60) +
               viridis::scale_fill_viridis(alpha = 1, begin = 0.4, end = 0.9, option = "F", discrete=TRUE, 
                                           name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
               labs(x = "Dispersion", 
                    y = "Benthic substrate ratio (BSR)",
                    #title = "Dispersion categories",
                    subtitle = "Benthic Substrate Ratio (BSR)") +
               theme_bw() +
               theme(plot.title = element_text(size = 12, hjust = 0.5), 
                     #subtitle = element_text(size = 12, hjust = 0),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                     legend.position = "none")) 
)

ggsave("graphs/FigureX3b_bsr_summary.pdf", width = 8, height = 6)
ggsave("graphs/FigureX3b_bsr_summary.tiff", width = 12, height = 6)



