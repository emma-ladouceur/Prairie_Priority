
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(devtools)
library(patchwork)
library(hypervolume)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")
sp <- read.csv("prairie.prep.treats.csv", header= TRUE)

head(sp)

plot_n <- sp %>% select(samp_id, Treatment_cat, Treatment_type) %>% distinct() %>%
  group_by(Treatment_cat, Treatment_type) %>% summarise(n_samps = n())

plot_n

traits <- read.csv("imputed_trait_matrix_fixed.csv",  header= TRUE)

View(traits)

traits 


prairie_traits <- sp_fix %>% left_join(traits) %>%
   select(pres, species:SSD) %>%
  group_by(Treatment_cat, Treatment_type) %>%
  distinct() %>% left_join(plot_n) %>%
  drop_na() %>% select(-pres)

 
 head(prairie_traits)
 
 
 # FIGURE 2 ----
 
 PCA         <- prcomp(prairie_traits[,5:14])
 summary(PCA) # check how much variance each PC explains : PC1 46%, PC2 25%
 PCAvalues   <- data.frame(Species = prairie_traits$species, Treatment_cat = prairie_traits$Treatment_cat, Treatment_type = prairie_traits$Treatment_type, PCA$x)# Extract PC axes for plotting
 PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)          # Extract loading of the variables
 

 p_my_theme <-  theme( axis.title.x= element_text(colour="black", size=7),
                       axis.title.y= element_text(colour="black", size=7),
                       axis.text.x= element_text(colour= "black", size=7),
                       axis.text.y= element_text(colour= "black", size=7),
                       panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                       panel.border=element_rect(fill=NA,colour="grey50"))
 
 # Island versus global trait space  
 
 PCAvalues$Treatment_cat <- factor(PCAvalues$Treatment_cat, levels = c("Nutrients",  "Assembly", "Invasion"))
 
 PCAvalues$Treatment_type<- as.factor(PCAvalues$Treatment_type)
 levels(PCAvalues$Treatment_type)
 PCAvalues$Treatment_type <- factor(PCAvalues$Treatment_type, 
                                          levels = c("Control", "Nutrients", 
                                                     "Both first", "Forbs first", "Grass first",
                                                     "Early", "Late"))
 
 Fig = ggplot(PCAvalues) +   
   facet_wrap(~Treatment_cat)+
   geom_point(size = 2, alpha=0.9, 
              aes(x = PC1, y = PC2, group = Treatment_type, colour = Treatment_type, size = Treatment_type),
              show.legend = TRUE) +
   #coord_fixed() + 
  # scale_colour_manual(values=c("gray65","blue")) +
   scale_colour_manual( values = c("#31688e","#35b779", "#443983","#90d743","#21918c","#fde725", "#440154") ) +  
   scale_y_reverse()   +
   geom_segment(data = PCAloadings, size = 0.25,
                aes(x = 0, xend = PC1*3.5, y = 0, yend = PC2*3.5),
                arrow = arrow(length = unit(0.1, "cm")),colour = "black")   +
   geom_text(data = PCAloadings, aes(x = PC1*3.6, y = PC2*3.6, label = Variables), size = 2.3,
            # hjust=c(0, 0, 0, 0, 0, 0) , vjust=c(0, 0, 0, 0, 0, 1)
   )    +
   xlab("PC1") + ylab("PC2")   +
   p_my_theme 
 
 Fig
 
 # Marginal density distribution island and global PCA  
 xplot <- ggdensity(PCAvalues, "PC1", fill = "ori", color = "ori", palette = c("gray65","blue"))
 yplot <- ggdensity(PCAvalues, "PC2", fill = "ori", color = "ori", palette = c("gray65","blue")) + rotate()
 
 cowplot::plot_grid(xplot, NULL, Fig, yplot, ncol = 2, align = "hv", 
                    rel_widths = c(2, 1), rel_heights = c(1, 2))
 
 # Island and global trait space overlap
 overall_bandwidth <- estimate_bandwidth(PCAvalues[,3:5])
 
 Tenerife  <- dplyr::filter(PCAvalues, ori== "Island_data")
 HV_Tenerife <-hypervolume_gaussian(Tenerife[,3:5], name = "Island volume",
                                    kde.bandwidth=overall_bandwidth, quantile.requested=0.95)
 
 Global  <- dplyr::filter(PCAvalues, ori== "Global_data")
 HV_Global <-hypervolume_gaussian(Global[, 3:5], name = "Global volume",
                                  kde.bandwidth=overall_bandwidth, quantile.requested=0.95)
 
 # Calculating S?rensen / overlap statistics
 HVs <- hypervolume_join (HV_Tenerife, HV_Global)
 
 HV_set <- hypervolume_set(HV_Tenerife, HV_Global, num.points.max = NULL,
                           verbose = TRUE, check.memory = F, distance.factor = 1)
 
 HV_global_island_stats <- hypervolume_overlap_statistics(HV_set)
 HV_global_island_stats
 
 
 
 
 
 
 
 
 