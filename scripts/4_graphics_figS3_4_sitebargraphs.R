## Visualisation of dispersion results
## Description: Calculate BSR, bar graphs sites ranked with benthic major and coral genera, MDS plots
## Alice Lawrence | alice.lawrence@bangor.ac.uk
## 1st May 2023


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


# import data
major_trans <- read.csv("data_output_old/benthic_trans_major_4graphs.csv")

coral_trans <-read.csv("data_output_old/benthic_trans_coral_4graphs.csv")


#### prepare data for graphics ####

# Figure 1 - Benthic Major % cover ranked and coloured by disp category
# use 'results_mean_1000' and add 

# Figure 2 - BSR Benthic Major % cover

# Figure 3 - Coral genera % cover ranked and coloured by disp category


# convert to factors: sitename, rank, dispersion
major_trans <- 
  major_trans %>% 
  mutate(across (c(sitename, rank, dispersion) , as.factor))

coral_trans <- 
  coral_trans %>% 
  mutate(across (c(sitename, rank, dispersion) , as.factor))

str(major_trans)
str(coral_trans)
levels(major_trans$sitename)


# remove X column to avoid confusion with rank?
# major <- select(major, -X)
  
# coral <- select(coral, -X)


# reorder levels in dispersion category factor
major_trans$dispersion <- factor(major_trans$dispersion,
                           levels = c('Low', 'Med', 'High'),
                           labels = c('Low', 'Med', 'High'))

levels(major_trans$dispersion)

unique(major_trans$dispersion)

# reorder factor levels
# major$sitename <- factor(major$sitename,
#                          levels = c('Amanave', 'Sita', 'Nuuuli', ' Maloata', 'Poloa', 'Oa', 'Amalau', 'Leone2', 'Fagaitua', 'Masausi', 'Fagatele', 'Fagaalu', 'Fagamalo', 'Amouli', 'Alega', 'Amaua', 'Afono', 'Matuu', 'Aoa', 'Alofau', 'Nua', 'Vatia', 'Fagasa2', 'Amaluia', 'Aasu', 'Masefau', 'Tafeu', 'Laulii'))
# levels(major$sitename)
# 
# unique(major$sitename)


# graphic 1 - benthic major % cover ordered by dispersion rank on x
# graphic1 <- ggplot(major_trans, aes(x = sitename, y = Coral)) +
#   geom_col()
# 
# plot(graphic1)


#### benthic major graphs ####
# Major Benthic categories
c <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                                  y = Coral, 
                                  fill = dispersion,
                                  width = 0.4)) +
  geom_col() +
    ylim(0, 60) +
  viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                              name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
  labs(x = "", 
       y = "% cover",
       title = "Calcifying substrate",
       subtitle = "(i) Hard Coral") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5), 
        #subtitle = element_text(size = 12, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")) 
plot(c)

#try lollipop

# ggplot(aes(x = sitename, y=Coral)) +
#   geom_segment(aes(x = sitename, xend=sitename, y = 0, yend = Coral, colour = dispersion), 
#                linewidth = 0.8, lineend = "round", data = major) +
#   geom_point(aes(x = sitename, y = Coral, colour = dispersion)) #+


# turf
ta <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                        y = Turf, 
                        fill = dispersion,
                        width = 0.4)) +
         geom_col() +
             ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
        labs(x = "", 
             y = "% cover",
             title = "",
             subtitle = "(v) Turf Algae") +
        theme_bw() +
        theme(plot.title = element_text(size = 12, hjust = 0), 
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")) 

plot(ta)

# cca
cca <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                         y = CCA, 
                         fill = dispersion,
                         width = 0.4)) +
          geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "",
               subtitle = "(ii) Crustose Coralline Algae (CCA)") +
          theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 

plot(cca)

# macroalgae
ma <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                         y = Macroalgae, 
                         fill = dispersion,
                         width = 0.4)) +
         geom_col() +
         ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
         labs(x = "Site", 
              y = "% cover",
              title = "",
              subtitle = "(vi) Macroalgae") +
         theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 
plot(ma)

# BCA
bca <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                         y = BCA, 
                         fill = dispersion,
                         width = 0.4)) +
         geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                     name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "Site", 
               y = "% cover",
               title = "",
               subtitle = "(iii) Branching Coralline Algae (BCA)") +
         theme_bw() +
         theme(plot.title = element_text(size = 12, hjust = 0), 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
               legend.position = "none")) 
plot(bca)


# FCA
fca <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                                y = FCA, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "Non-calcifying substrate",
               subtitle = "(iv) Fleshy Coralline Algae (FCA)") +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(fca)


# Sand
sand <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                                y = Sand, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
           ylim(0, 60) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", y = "% cover") +
          ggtitle('(i) Sand') +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(sand)


# Rubble
rub <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                                 y = Rubble, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
          ylim(0, 60) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", y = "% cover") +
           ggtitle('(ii) Rubble') +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0), 
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(rub)


# Other Inverts
oi <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                                y = OtherInverts, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
         ylim(0, 60) +
         viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "Site", y = "% cover") +
          ggtitle('(iii) Other Invertebrates') +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(oi)


# BSR
bsr <- (ggplot(major_trans, aes(x = fct_inorder(sitename), 
                          y = BSR, 
                          fill = dispersion,
                          width = 0.4)) +
          geom_col() +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "Site", y = "Benthic Substrate Ratio (BSR)") +
          # ggtitle('Benthic Substrate Ratio (BSR)') +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
                axis.text.y = element_text(size = 10, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(bsr)
ggsave("graphs/Figure2_bsr.tiff", width = 10, height = 6)


# figure1 <- ggarrange(c, ta, cca, ma, 
#                      common.legend = TRUE, 
#                      legend="top",
#                      labels = c("Hard Coral", "Turf Algae", "Crustose Coralline Algae (CCA)", "Macroalgae"),
#                      font.label = list(size = 10, colour = "lightblue"),
#                      ncol=2, nrow=2
#                      )
# 
# ggsave("graphs/Figure1_trans.pdf", width = 10, height = 8)


# Figure 1 Benthic Major
ggarrange(c, fca, cca, ta, bca, ma, 
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

ggsave("graphs/Figure1b_trans.pdf", width = 10, height = 12)
ggsave("graphs/Figure1b_trans.tiff", width = 10, height = 12)


# Figure Benthic Major - supplementary
ggarrange(sand, rub, oi, 
          ncol=1, nrow=3, 
          common.legend = TRUE, 
          legend="top")

ggsave("graphs/FigureS1_major.pdf", width = 5, height = 10)
ggsave("graphs/FigureS1_major.tiff", width = 5, height = 10)


#### coral graphs ####
# Figure Coral genera - (MeanTotalCoral), MONTI, PORRUS, COSC, ACROP, ISOP, (PAV)

monti <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                              y = MONTI, 
                              fill = dispersion,
                              width = 0.4)) +
        geom_col() +
        ylim(0, 40) +
        viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                    name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
        labs(x = "", 
             y = "% cover",
             title = "",
             subtitle = "(i) Montipora spp.") +
        theme_bw() +
        theme(plot.title = element_text(size = 12, hjust = 0.5), 
              #subtitle = element_text(size = 12, hjust = 0),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")) 
plot(monti)

# PORRUS
porrus <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                  y = PORRUS, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 title = "",
                 subtitle = "(ii) Porites rus") +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(porrus)


# ACROP
acrop <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                 y = ACROP, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                title = "",
                subtitle = "(iii) Acropora spp.") +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(acrop)

# ISOP
isop <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                  y = ISOP, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 title = "",
                 subtitle = "(iv) Isopora spp.") +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(isop)


# PAV
pav <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                 y = PAV, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                title = "",
                subtitle = "(v) Pavona spp.") +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(pav)


# ACROTBL
actbl <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                y = ACROTBL, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 40) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "",
               subtitle = "(vi) Acropora table") +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                #subtitle = element_text(size = 12, hjust = 0),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(actbl)


# POC
poc <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                  y = POC, 
                                  fill = dispersion,
                                  width = 0.4)) +
            geom_col() +
            ylim(0, 40) +
            viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                        name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
            labs(x = "", 
                 y = "% cover",
                 title = "",
                 subtitle = "(vii) Pocillopora spp.") +
            theme_bw() +
            theme(plot.title = element_text(size = 12, hjust = 0.5), 
                  #subtitle = element_text(size = 12, hjust = 0),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.position = "none")) 
plot(poc)


# PORMAS
pormas <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                y = PORMAS, 
                                fill = dispersion,
                                width = 0.4)) +
          geom_col() +
          ylim(0, 40) +
          viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                      name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
          labs(x = "", 
               y = "% cover",
               title = "",
               subtitle = "(vi) Porites massive") +
          theme_bw() +
          theme(plot.title = element_text(size = 12, hjust = 0.5), 
                #subtitle = element_text(size = 12, hjust = 0),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none")) 
plot(pormas)


# LEPT
lept <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                   y = LEPT, 
                                   fill = dispersion,
                                   width = 0.4)) +
             geom_col() +
             ylim(0, 40) +
             viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                         name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
             labs(x = "", 
                  y = "% cover",
                  title = "",
                  subtitle = "(ix) Leptastrea spp.") +
             theme_bw() +
             theme(plot.title = element_text(size = 12, hjust = 0.5), 
                   #subtitle = element_text(size = 12, hjust = 0),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                   legend.position = "none")) 
plot(lept)


# LOBOPH
lobo <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                 y = LOBOPH, 
                                 fill = dispersion,
                                 width = 0.4)) +
           geom_col() +
           ylim(0, 40) +
           viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                       name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
           labs(x = "", 
                y = "% cover",
                title = "",
                subtitle = "(x) Lobophyllia spp.") +
           theme_bw() +
           theme(plot.title = element_text(size = 12, hjust = 0.5), 
                 #subtitle = element_text(size = 12, hjust = 0),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 legend.position = "none")) 
plot(lobo)



# COSC
cosc <- (ggplot(coral_trans, aes(x = fct_inorder(sitename), 
                                   y = COSC, 
                                   fill = dispersion,
                                   width = 0.4)) +
             geom_col() +
             ylim(0, 40) +
             viridis::scale_fill_viridis(alpha = 1, begin = 0.1, end = 0.9, option = "C", discrete=TRUE, 
                                         name = "Dispersion", labels=c("Low", "Medium", "High"), ) +
             labs(x = "", 
                  y = "% cover",
                  title = "",
                  subtitle = "(x) Coscinarea") +
             theme_bw() +
             theme(plot.title = element_text(size = 12, hjust = 0.5), 
                   #subtitle = element_text(size = 12, hjust = 0),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                   legend.position = "none")) 
plot(cosc)


# Figure 3 Coral genera
ggarrange(monti, porrus, acrop, isop, pav, pormas, 
          ncol=2, nrow=3, 
          common.legend = TRUE, 
          legend="bottom")

ggsave("graphs/Figure3_coral.pdf", width = 10, height = 12)
ggsave("graphs/Figure3_coral.tiff", width = 10, height = 12)



# jyodee code ***
# 
# major %>% 
#   mutate(sitename = fct_relevel(sitename, 'Amanave', 'Sita', 'Nuuuli', 'Maloata', 'Poloa', 
#                                 'Oa', 'Amalau', 'Leone2', 'Fagaitua', 'Masausi', 'Fagatele', 
#                                 'Fagaalu', 'Fagamalo', 'Amouli', 'Alega', 'Amaua', 'Afono', 
#                                 'Matuu', 'Aoa', 'Alofau', 'Nua', 'Vatia', 'Fagasa2', 'Amaluia', 
#                                 'Aasu', 'Masefau', 'Tafeu', 'Laulii')) %>%
#   ggplot(aes(x= sitename, y = Coral, fill = dispersion, width = 0.2)) +
#   geom_col()




# filter then patchwork - grid arrange 

#### lollipop test ####
# try lollipop chart
lollipop <- ggplot(major, aes(x=sitename, y=Coral)) +
  geom_point() + 
  geom_segment( aes(x=sitename, xend=sitename, y=0, yend=Coral))

# or try...
# ggplot(counts, aes(x, Freq)) +
#   geom_segment(aes(xend = x, yend = 0), linewidth = 10, lineend = "butt")

lollipoptest <- ggplot(major, aes(x = fct_inorder(sitename), y=Coral)) +
  geom_point(size = 2, colour = 'darkgreen') +
  geom_segment(aes(xend = sitename, yend = 0), linewidth = 1.5, lineend = "round", colour = "brown", data = major)

# colour = 'red', 'blue', 'green'

plot(lollipoptest)

# lollipop horizontal with groups
ggplot(major, aes(x = fct_inorder(sitename), y=Coral)) +
  geom_segment(aes(x = sitename, xend=sitename, y = 0, yend = Coral, colour = dispersion), linewidth = 0.8, lineend = "round", data = major) +
  geom_point(aes(x = sitename, y = Coral, colour = dispersion)) +
  coord_flip() +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10)
  )+
  xlab("sitename") +
  ylab("% Coral Cover") +
  facet_wrap(~dispersion, ncol=1, scale="free_y")


# lollipop vertical without groups # need to figure out how to have in order
major %>% 
  mutate(sitename = fct_relevel(sitename, 'Amanave', 'Sita', 'Nuuuli', 'Maloata', 'Poloa', 
                                'Oa', 'Amalau', 'Leone2', 'Fagaitua', 'Masausi', 'Fagatele', 
                                'Fagaalu', 'Fagamalo', 'Amouli', 'Alega', 'Amaua', 'Afono', 
                                'Matuu', 'Aoa', 'Alofau', 'Nua', 'Vatia', 'Fagasa2', 'Amaluia', 
                                'Aasu', 'Masefau', 'Tafeu', 'Laulii')) %>%
  ggplot(aes(x = sitename, y=Coral)) +
  geom_segment(aes(x = sitename, xend=sitename, y = 0, yend = Coral, colour = dispersion), 
               linewidth = 0.8, lineend = "round", data = major) +
  geom_point(aes(x = sitename, y = Coral, colour = dispersion)) #+
 # coord_flip() +
 #  theme(
 #    legend.position = "right",
 #    panel.border = element_blank(),
 #    panel.spacing = unit(0.1, "lines"),
 #    #strip.text.x = element_text(size = 10))+
 #  xlab("sitename") +
 #  ylab("Coral Cover (%)")) 
 # # facet_wrap(~dispersion, nrow=1, scale="free_y")
 # 

