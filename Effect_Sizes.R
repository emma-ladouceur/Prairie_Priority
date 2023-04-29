



library(tidyverse)
library(brms)
library(viridis)
library(viridisLite)
library(patchwork)
library(metafor)

setwd("~/head(TDDropbox/_Projects/Prairie_Priority/")

# 14 3D_treat_block.R
TD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.TD.csv", header= TRUE)
PD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.PD.csv", header= TRUE)
FD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.FD.csv", header= TRUE)

head(TD)
View(TD)


alph_gam_TD  <- TD %>% 
  filter(nt == c(1, 80)) %>%
  separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
  select(c(Order.q, nt, Nutrients, Invasion, qD ,  qD.LCL, qD.UCL)) %>%
  gather(variable, value, -(Order.q:Invasion)) %>%
  unite(temp, Invasion, variable) %>%
  spread(temp, value) %>%
  mutate( ESqd = (Late_qD - Early_qD),
          ESqd.LCL = (Late_qD.LCL - Early_qD.LCL),
          ESqd.UCL = (Late_qD.UCL - Early_qD.UCL) ) %>% 
 # select(c( Order.q, nt, Nutrients)) %>%
  gather(trt, response, c(Early_qD:Late_qD.UCL)) %>%
  separate(trt, c("Invasion", "q"), extra = "merge", sep="_") %>% select(-c(Invasion, q, response)) %>% distinct() %>%
  mutate(nt = as.factor(nt),
         qD = "TD") 
  

head(alph_gam_TD)


q_0 <- ggplot() +  
  #facet_wrap(~Order.q) +
  geom_point(data = alph_gam_TD  %>% filter(Order.q ==  "q = 0" ),
             aes(x = nt, y = ESqd, colour = Nutrients, group = Nutrients), size = 3) +
  geom_errorbar(data = alph_gam_TD %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = nt, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
   # title= 'q = 0'
  ) + ylab("Forb Taxonomic Diversity")

q_2 <-ggplot() +  
  #facet_wrap(~Order.q) +
  geom_point(data = alph_gam_TD  %>% filter(Order.q ==  "q = 2" ),
             aes(x = nt, y = ESqd, colour = Nutrients, group = Nutrients), size = 3) +
  geom_errorbar(data = alph_gam_TD %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = nt, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0) +
  labs(x = '' # y=''
       ) +
  scale_color_viridis(discrete = T, option= "plasma")+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    # title= 'q = 0'
  ) + ylab("Forb Taxonomic Diversity")

(q_0 + q_2)




alph_gam_TD  <- TD %>% 
  filter(nt == c(1, 80)) %>%
  separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
  mutate( q_std_dev = ( (qD.UCL- qD.LCL) / 3.92) * sqrt(50) ) %>%
  arrange(Order.q, nt, Nutrients, desc(Invasion)) %>%
  mutate(group_size = 50) %>%
  select(Order.q, nt, Nutrients, Invasion, group_size, qD, q_std_dev) %>%
  #group_by(Order.q, Nutrients) %>%
  gather(variable, value, -(Order.q:Invasion)) %>%
  unite(temp, Invasion, variable) %>%
  spread(temp, value) 


head(alph_gam_TD)

lrr_TD <- escalc(measure = "ROM", m1i=Late_qD, m2i=Early_qD, sd1i= Late_q_std_dev, sd2i= Early_q_std_dev,
                 n1i=Late_group_size , n2i=Early_group_size,
       data=alph_gam_TD, slab=paste0(Order.q , ", ", nt, ", ",Nutrients), digits=4)

head(lrr_TD)


forest(lrr_TD$yi, lrr_TD$vi)


hd_TD <- escalc(measure = "SMD", m1i=Late_qD, m2i=Early_qD, sd1i= Late_q_std_dev, sd2i= Early_q_std_dev,
                 n1i=Late_group_size , n2i=Early_group_size,
                 data=alph_gam_TD, slab=paste0(Order.q , ", ", nt, ", ",Nutrients), digits=4)

head(hd_TD)


forest(hd_TD$yi, hd_TD$vi)

