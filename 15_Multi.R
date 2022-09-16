
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(alphahull)
library(ggExtra)
library(hypervolume)
library(BAT)
library(patchwork)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")
sp <- read.csv("prairie.prep.treats.csv", header= TRUE)

head(sp)

plot_n <- sp %>% select(Treatment_cat, Treatment_type, Treatment, samp_id) %>% distinct() %>%
  group_by(Treatment_cat, Treatment_type, Treatment) %>% summarise(n_samps = n())

plot_n


traits <- read.csv("imputed_trait_matrix_fixed.csv",  header= TRUE)

head(traits)


prairie_traits <- sp %>% left_join(traits) %>%
  select(Treatment_cat, Treatment_type, Treatment, species, LA:SSD) %>%
  group_by(Treatment_cat, Treatment_type) %>%
  distinct() %>% left_join(plot_n) %>%
  drop_na() 


head(prairie_traits)



traits = data.frame(species =  traits$species, 
                    # LA     =  log10(traits$LA),    
                    # LDMC      =  log10(traits$LDMC), 
                    # LN       =  log10(traits$LN),       
                    # PH       =  log10(traits$PH),      
                    # SDM     =  log10(traits$SDM),    
                    # SGR      =  log10(traits$SGR), 
                    # SLA       =  log10(traits$SLA),       
                    # SN       =  log10(traits$SN),  
                    # SSBL       =  log10(traits$SSBL + 10),       
                    # SSD       =  log10(traits$SSD)
                    LA     =  (traits$LA),    
                    LDMC      =  (traits$LDMC), 
                    LN       =  (traits$LN),       
                    PH       =  (traits$PH),      
                    SDM     =  (traits$SDM),    
                    SGR      =  (traits$SGR), 
                    SLA       =  (traits$SLA),       
                    SN       =  (traits$SN),  
                    SSBL       =  (traits$SSBL),       
                    SSD       =  (traits$SSD))
                    
row.names(traits) = traits$species 

head(traits)

PCA         <- prcomp(traits[2:11], scale. = TRUE)
biplot(PCA)
summary(PCA)

# Interesting website about PCA 
# https://builtin.com/data-science/step-step-explanation-principal-component-analysis

PCAvalues   <- data.frame(species = traits$species, PCA$x)    # Extract PC axes for plotting
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)   # Extract loadings of the variables

# Plot trait space of all species

PCAvalues %>% ggplot(aes(PC1,  PC2), size = 1)+ # Plot PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)),  
                  colour = "gray", bins = 10) +
  scale_fill_distiller(palette = "BuGn", direction = 1) +
  geom_jitter(alpha= 0.6,  size = 2  , colour = "turquoise4") +     #  Display the points, i.e., species 
  geom_text(data = PCAloadings, aes(x = PC1*4.7, y = PC2*4.7, label = Variables), size = 2.3) +
  geom_segment(data = PCAloadings, size = 0.2,    # Plots the loadings, i.e., traits 
               aes(x = 0, xend = PC1*4.2, y = 0, yend = PC2*4.2),
               arrow = arrow(length = unit(0.1, "cm")),colour = "black")  +
  ylim(-5, 5.5) + 
  theme_minimal()

# TASK: 
#  1. identify acquisitive and conservative plants in the trait space of Hawaiian trees 
#  2. Which plant strategies according to the species trait combinations can you identify in the trait space?

# Let's plot trait space of dry and wet species

head(PCAvalues)

datt_trait = dplyr::left_join(prairie_traits, PCAvalues, by =  "species") 

head(datt_trait)
datt_trait$Treatment <- as.factor(datt_trait$Treatment)
levels(datt_trait$Treatment)

nut_0 = dplyr::filter(datt_trait, Treatment == "Nutrients_Control") # Separating the data into dry and wet  
nut_1 = dplyr::filter(datt_trait, Treatment == "Nutrients_Nutrients")

ass_b = dplyr::filter(datt_trait, Treatment == "Assembly_Both first") # Separating the data into dry and wet  
ass_f = dplyr::filter(datt_trait, Treatment == "Assembly_Forbs first")
ass_g = dplyr::filter(datt_trait, Treatment == "Assembly_Grass first")

inv_e = dplyr::filter(datt_trait, Treatment == "Invasion_Early") # Separating the data into dry and wet  
inv_l = dplyr::filter(datt_trait, Treatment == "Invasion_Late")


colnames(dry)

# Summarize / aggregate trait values per species 
nut_0 <- dplyr::summarize(group_by(nut_0, species,  Treatment),
                         PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
nut_1 <- dplyr::summarize(group_by(nut_1, species,  Treatment),
                         PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))

ass_b <- dplyr::summarize(group_by(ass_b, species,  Treatment),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
ass_f <- dplyr::summarize(group_by(ass_f, species,  Treatment),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
ass_g <- dplyr::summarize(group_by(ass_g, species,  Treatment),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))


inv_e <- dplyr::summarize(group_by(inv_e, species,  Treatment),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
inv_l <- dplyr::summarize(group_by(inv_l, species,  Treatment),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))

trt <- nut_0 %>% bind_rows( nut_1, ass_b, ass_f, ass_g, inv_e, inv_l ) %>%
  left_join(plot_n)

head(trt)

trt$Treatment_cat <- factor(trt$Treatment_cat, levels = c("Nutrients",  "Assembly", "Invasion"))

trt$Treatment_type<- as.factor(trt$Treatment_type)
levels(trt$Treatment_type)
trt$Treatment_type <- factor(trt$Treatment_type, 
                                         levels = c("Control", "Nutrients", 
                                                    "Both first", "Forbs first", "Grass first",
                                                    "Early", "Late"))
nuts <- trt  %>% filter(Treatment_cat == "Nutrients") 
ass <- trt  %>% filter(Treatment_cat == "Assembly") 
inv <- trt  %>% filter(Treatment_cat == "Invasion") 

nuts_f = nuts %>%
  ggplot(aes(PC1,  PC2, color = Treatment_type))+ # Plot Tenerife PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)  ), alpha =0.2, bins = 3.5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
 # scale_color_manual(values = c("gold2", "turquoise4" )) +
  scale_colour_manual( values = c("#31688e","#35b779", "#443983","#90d743","#21918c","#fde725", "#440154") ) +  
  geom_jitter(alpha=0.5,  size = 2) +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal() + theme(legend.position = "bottom") 


nuts_f

nuts_f <- ggMarginal(nuts_f, type = "density", groupColour = TRUE, groupFill = TRUE) 

nuts_f

ass_f = ass %>%
  ggplot(aes(PC1,  PC2, color = Treatment_type))+ # Plot Tenerife PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)  ), alpha =0.2, bins = 3.5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  # scale_color_manual(values = c("gold2", "turquoise4" )) +
  scale_colour_manual( values = c("#443983","#90d743","#21918c","#fde725", "#440154") ) +  
  geom_jitter(alpha=0.5,  size = 2) +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal() + theme(legend.position = "bottom") 


ass_f

ass_f <- ggMarginal(ass_f, type = "density", groupColour = TRUE, groupFill = TRUE) 

ass_f

inv_f = inv %>%
  ggplot(aes(PC1,  PC2, color = Treatment_type))+ # Plot Tenerife PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)  ), alpha =0.2, bins = 3.5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  # scale_color_manual(values = c("gold2", "turquoise4" )) +
  scale_colour_manual( values = c("#fde725", "#440154") ) +  
  geom_jitter(alpha=0.5,  size = 2) +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal() + theme(legend.position = "bottom") 


inv_f

inv_f <- ggMarginal(inv_f, type = "density", groupColour = TRUE, groupFill = TRUE) 

inv_f


## two treats
combo_traits <- sp %>% left_join(traits) %>%
  select(treat_id, species, LA:SSD) %>%
  group_by(treat_id) %>%
  distinct() %>% 
  drop_na() 


head(combo_traits)
combo_traits$treat_id <- as.factor(combo_traits$treat_id)
levels(combo_traits$treat_id)

datt_combo_trait = dplyr::left_join(combo_traits, PCAvalues, by =  "species") 

trtc_0 = dplyr::filter(datt_combo_trait, treat_id == "0_e_b") # Separating the data into dry and wet  
trtc_1 = dplyr::filter(datt_combo_trait, treat_id == "1_l_b")

trtc_0 <- dplyr::summarize(group_by(trtc_0, species,  treat_id),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
trtc_1 <- dplyr::summarize(group_by(trtc_1, species,  treat_id),
                          PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))

trt_c <- trtc_0 %>% bind_rows( trtc_1 ) 
head(trt_c)

trt_c_f = trt_c %>%
  ggplot(aes(PC1,  PC2, color = treat_id))+ # Plot Tenerife PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)  ), alpha =0.2, bins = 3.5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  # scale_color_manual(values = c("gold2", "turquoise4" )) +
  scale_colour_manual( values = c("#fde725", "#440154") ) +  
  geom_jitter(alpha=0.5,  size = 2) +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal() + theme(legend.position = "bottom") 

trt_c_f

trt_c_f <- ggMarginal(trt_c_f, type = "density", groupColour = TRUE, groupFill = TRUE) 

trt_c_f

#_______________________________________________________________________________
# Do traits combinations of Hawaiian forest species overlap across rainfall gradients?
# how similar wet and dry are with respect to their trait diversity

##  https://benjaminblonder.org/hypervolume_faq.html

head(PCAvalues)

overall_bandwidth <- estimate_bandwidth(PCAvalues[,2:4])

head(trt1)
trt1_hv <-hypervolume_gaussian(trt1[,3:5], name = "Trt1 Volume",
                              kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

trt2_hv <-hypervolume_gaussian(trt2[,3:5], name = "Trt2 volume",
                              kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

# calculating overlap statistics
HVs <- hypervolume_join (trt1_hv, trt2_hv)

HV_set <- hypervolume_set(trt1_hv, trt2_hv, num.points.max = NULL,
                          verbose = TRUE, check.memory = F, distance.factor = 1)
plot(HVs)

hypervolume_overlap_statistics(HV_set)

#_______________________________________________________________________________
# Functional richness, evenness, dispersion (Blonder et al approach)
# The difference in species number between wet and dry affects FD estimations. 
# To solve this, we rarefied by the common minimum number of species. 

min_rar <- 25

nbperm <- 20 # Number of permutations

rar <- c()   

for (i in 1:nbperm){   # Loop of rarefaction
  # dry 
  trt1_i <- trt1[sample(1:nrow(trt1), size = min_rar),
                c("PC1", "PC2", "PC3")]
  
  # Compute hypervolume with a fixed bandwidth
  trt1_i <-  hypervolume_gaussian(
    trt1_i, kde.bandwidth = overall_bandwidth,
    quantile.requested = 0.95, quantile.requested.type = "probability",
    verbose = FALSE)
  
  trt1_i <- data.frame(perm = i,
                      status = "trt2",
                      rich = kernel.alpha(trt1_i),
                      eve = kernel.evenness(trt1_i),
                      div = kernel.dispersion(trt1_i))
  # wet
 trt2_i <- trt2[sample(1:nrow(trt2), size = min_rar),
                c("PC1", "PC2", "PC3")]
  
  # Compute hypervolume with a fixed bandwidth
  trt2_i <-  hypervolume_gaussian(
    trt2_i, kde.bandwidth = overall_bandwidth,
    quantile.requested = 0.95, quantile.requested.type = "probability",
    verbose = FALSE)
  
  trt2_i <- data.frame(perm = i,
                      status = "trt2",
                      rich = kernel.alpha(trt2_i),
                      eve = kernel.evenness(trt2_i),
                      div = kernel.dispersion(trt2_i))
  # Bind results
  rar <- rbind(rar, trt1_i, trt2_i)
  
  cat(paste0(round(100*i/nbperm, 0), " %; "))
}


unique(rar$status)
colnames(rar)

# Summarizing rarefied values
# https://www.scribbr.com/statistics/confidence-interval/

sum_fr <- rar %>%       
  group_by(status) %>%
  summarise( n = n(), mean = mean(rich), sd = sd(rich) ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( mean, metric = "Functional Richness") 

sum_fe <- rar %>%
  group_by(status) %>%
  summarise( n = n(), mean = mean(eve),sd = sd(eve) ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( mean, metric = "Functional Evenness") 

sum_fd <- rar %>%
  group_by(status) %>%
  summarise( n = n(), mean = mean(div),sd = sd(div) ) %>%
  mutate( se = sd/sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( mean, metric = "Functional Dispersion") 

sum_metrics <-  rbind(sum_fr, sum_fe, sum_fd)

#___________________________________ Plot functional trait diversity metrics

ggplot(sum_metrics) +
  geom_linerange(aes(factor(status), 
                     ymin= mean-ic, ymax= mean+ic, color = status),  size = 0.3,   # Error bar
                 show.legend = FALSE) +
  geom_point    (aes(x = status,  
                     y=mean, color = status),  size = 1,  # error mean point
                 show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free") 
