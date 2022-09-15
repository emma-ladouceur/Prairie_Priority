
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(devtools)
library(patchwork)
library(hypervolume)
library(scales)
library(alphahull)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")
sp <- read.csv("prairie.prep.treats.csv", header= TRUE)

head(sp)

plot_n <- sp %>% select(treat_id, samp_id) %>% distinct() %>%
  group_by(samp_id, treat_id) %>% summarise(n_samps = n())

plot_n

traits <- read.csv("imputed_trait_matrix_fixed.csv",  header= TRUE)

View(traits)

traits 


prairie_traits <- sp %>% left_join(traits) %>%
   select(treat_id, species, LA:SSD) %>%
  group_by(treat_id) %>%
  distinct() %>% left_join(plot_n) %>%
  drop_na() 

 
 head(prairie_traits)
 
 
 # FIGURE 2 ----
 colnames(prairie_traits)
 PCA         <- prcomp(prairie_traits[,3:12])
 summary(PCA) # check how much variance each PC explains : PC1 46%, PC2 25%
 PCAvalues   <- data.frame(Species = prairie_traits$species, Treatment = prairie_traits$treat_id,  PCA$x)# Extract PC axes for plotting
 PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)          # Extract loading of the variables
 head(PCAloadings)

 p_my_theme <-  theme( axis.title.x= element_text(colour="black", size=7),
                       axis.title.y= element_text(colour="black", size=7),
                       axis.text.x= element_text(colour= "black", size=7),
                       axis.text.y= element_text(colour= "black", size=7),
                       panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                       panel.border=element_rect(fill=NA,colour="grey50"),
                       legend.position = "bottom"
                       )
 
 # Island versus global trait space  
 
 PCAvalues$Treatment_cat <- factor(PCAvalues$Treatment_cat, levels = c("Nutrients",  "Assembly", "Invasion"))
 
 PCAvalues$Treatment_type<- as.factor(PCAvalues$Treatment_type)
 levels(PCAvalues$Treatment_type)
 PCAvalues$Treatment_type <- factor(PCAvalues$Treatment_type, 
                                          levels = c("Control", "Nutrients", 
                                                     "Both first", "Forbs first", "Grass first",
                                                     "Early", "Late"))
 
 PCAvalues
 
 
 # get colors
 q_colors =  12 
 v_colors =  viridis(q_colors, option = "C")
 v_colors
 
 Fig = ggplot(PCAvalues) +   
   #acet_wrap(~Treatment)+
   geom_point(size = 2, alpha = 0.9, 
              aes(x = PC1, y = PC2, group = Treatment, colour = Treatment, size = Treatment),
              show.legend = TRUE) +
  # coord_fixed() + 
   scale_colour_manual( values = c("#0D0887FF" ,"#3E049CFF" ,"#6300A7FF" ,"#8707A6FF" ,"#A62098FF" ,"#C03A83FF", "#D5546EFF", "#E76F5AFF", "#F58C46FF" ,"#FDAD32FF" ,"#FCD225FF", "#F0F921FF") ) +  
   #scale_y_reverse()   +
   # geom_segment(data = PCAloadings, size = 0.25,
   #              aes(x = 0, xend = PC1*3.5, y = 0, yend = PC2*3.5),
   #              arrow = arrow(length = unit(0.1, "cm")),colour = "black")   +
  # geom_text(data = PCAloadings, aes(x = PC1, y = PC2, label = Variables), size = 2.3,
  # hjust=c(0, 0, 0, 0, 0, 0,0,0,0,0) , vjust=c( 0, 0, 0, 0, 0,0,0,0,0, 1)
  # )    +
   xlab("PC1") + ylab("PC2")   +
   p_my_theme 
 
 Fig
 

 xplot <- ggdensity(PCAvalues, "PC1", fill = "Treatment", color = "Treatment", palette = c("#0D0887FF" ,"#3E049CFF" ,"#6300A7FF" ,"#8707A6FF" ,"#A62098FF" ,"#C03A83FF", "#D5546EFF", "#E76F5AFF", "#F58C46FF" ,"#FDAD32FF" ,"#FCD225FF", "#F0F921FF")) + theme(legend.position= "none")
 yplot <- ggdensity(PCAvalues, "PC2", fill = "Treatment", color = "Treatment", palette = c("#0D0887FF" ,"#3E049CFF" ,"#6300A7FF" ,"#8707A6FF" ,"#A62098FF" ,"#C03A83FF", "#D5546EFF", "#E76F5AFF", "#F58C46FF" ,"#FDAD32FF" ,"#FCD225FF", "#F0F921FF")) + rotate() + theme(legend.position= "none")
 
 # pca with dendsity plot on each axes
 cowplot::plot_grid(xplot, NULL, Fig, yplot, ncol = 2, align = "hv", 
                    rel_widths = c(2, 1), rel_heights = c(1, 2))
 
 
 # Island and global trait space overlap
 head(PCAvalues)
 PCAvalues$Treatment <- as.factor(PCAvalues$Treatment)
 levels(PCAvalues$Treatment)
 overall_bandwidth <- estimate_bandwidth(PCAvalues[,3:5])
 
 first  <- dplyr::filter(PCAvalues, Treatment== "0_e_g")
 HV_first <-hypervolume_gaussian(first[,3:5], name = "0_e_g",
                                    kde.bandwidth=overall_bandwidth, quantile.requested=0.95,
                                 quantile.requested.type = "probability",
                                 verbose = FALSE)
 
second  <- dplyr::filter(PCAvalues, Treatment== "1_l_b")
HV_second <-hypervolume_gaussian(second[, 3:5], name = "1_l_b",
                                  kde.bandwidth=overall_bandwidth, quantile.requested=0.95,
                                 quantile.requested.type = "probability",
                                 verbose = FALSE)
 
 # Calculating Sorensen / overlap statistics
 HVs <- hypervolume_join(HV_first, HV_second)
 
 HV_set <- hypervolume_set(HV_first, HV_second, num.points.max = NULL,
                           verbose = TRUE, check.memory = F, distance.factor = 1)
 
 HV_treats <- hypervolume_overlap_statistics(HV_set)
 HV_treats
 
 
 HV_first <- data.frame(#perm = i,
                     status = "first",
                     rich = kernel.alpha(HV_first),
                     eve = kernel.evenness(HV_first),
                     div = kernel.dispersion(HV_first))
 
 
 HV_second <- data.frame(#perm = i,
                     status = "second",
                     rich = kernel.alpha(HV_second),
                     eve = kernel.evenness(HV_second),
                     div = kernel.dispersion(HV_second))
 # Bind results
 rar <- rbind(rar, dry_i, wet_i)
 
 
 


  
 
 
 
 