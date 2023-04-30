



library(tidyverse)
library(brms)
library(viridis)
library(viridisLite)
library(patchwork)
library(metafor)

setwd("~/Dropbox/_Projects/Prairie_Priority/")

# 14 3D_treat_block.R
TD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.TD.csv", header= TRUE)
PD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.PD.csv", header= TRUE)
FD <- read.csv("Data/Treat Sep/prairie.hill.treats.sep.FD.csv", header= TRUE)

head(TD)
head(PD)
head(FD)

PD <- PD %>% mutate( qD = qPD, qD.LCL = qPD.LCL, qD.UCL = qPD.UCL ) %>%
  select( -c( qPD,  qPD.LCL, qPD.UCL )) %>%
  mutate(nt = as.factor(nt),
         D = "PD") 

FD <- FD %>% mutate( qD = qFD, qD.LCL = qFD.LCL, qD.UCL = qFD.UCL ) %>%
  select( -c( qFD,  qFD.LCL, qFD.UCL )) %>%
  mutate(nt = as.factor(nt),
         D = "FD") 

Div  <- TD %>% mutate(nt = as.factor(nt),  D = "TD") %>%
  bind_rows(FD, PD) %>%  
  filter(nt == c(1, 80)) %>% 
  separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
  select(c(Order.q, nt, Nutrients, Invasion, D, qD , qD.LCL, qD.UCL)) %>%
  gather(variable, value, -(Order.q:D)) %>%
  unite(temp, Invasion, variable) %>%
  spread(temp, value) %>%
  mutate( ESqd = (Late_qD - Early_qD),
          ESqd.LCL = (Late_qD.LCL - Early_qD.LCL),
          ESqd.UCL = (Late_qD.UCL - Early_qD.UCL),
          # log effect sizes
          ESqd_log = ( log(Late_qD) -  log(Early_qD) ),
          ESqd.LCL_log = ( log(Late_qD.LCL) -  log(Early_qD.LCL) ),
          ESqd.UCL_log = ( log(Late_qD.UCL) -  log(Early_qD.UCL) ),
          ) %>% 
  gather(trt, response, c(Early_qD:Late_qD.UCL)) %>%
  separate(trt, c("Invasion", "q"), extra = "merge", sep="_") %>% 
  select(-c(Invasion, q, response)) %>% distinct() 
  

View(Div)

# reorder Div's
Div$D <- factor(Div$D  , levels=c("TD","PD", "FD"))


q_0_smol <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
  ) + ylab("q = 0 \n Effect of late invasion")

q_0_smol

q_0_lorg <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
     title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
  ) + ylab("Effect of late invasion")

q_0_lorg

q_2_smol <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 1 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    # title= 'q = 0'
  ) + ylab("q = 2 \n Effect of late invasion")

q_2_smol

q_2_lorg <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    # title= 'q = 0'
  ) + ylab("Effect of late invasion")

q_2_lorg


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

es_legend <- g_legend(q_0_smol)

( (( (q_0_smol + theme(legend.position="none") ) + q_0_lorg) / (q_2_smol + q_2_lorg) ) / es_legend ) + plot_layout(heights = c(10, 10, 2))



# Log ratio

q_0_smol_log <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
  ) + ylab("q = 0 \n Log effect of late invasion")

q_0_smol_log

q_0_lorg_log <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 axis.title.x = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title=element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
  ) + ylab("Log effect of late invasion")

q_0_lorg_log

q_2_smol_log <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 1 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 1 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    # title= 'q = 0'
  ) + ylab("q = 2 \n Log effect of late invasion")

q_2_smol_log

q_2_lorg_log <- ggplot() +  
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    # title= 'q = 0'
  ) + ylab("Log effect of late invasion")

q_2_lorg_log


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

es_legend <- g_legend(q_0_smol_log)

( (( (q_0_smol_log + theme(legend.position="none") ) + q_0_lorg_log) / (q_2_smol_log + q_2_lorg_log) ) / es_legend ) + plot_layout(heights = c(10, 10, 2))



# 
# alph_gam_TD  <- TD %>% 
#   filter(nt == c(1, 80)) %>%
#   separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
#   mutate( q_std_dev = ( (qD.UCL- qD.LCL) / 3.92) * sqrt(50) ) %>%
#   arrange(Order.q, nt, Nutrients, desc(Invasion)) %>%
#   mutate(group_size = 50) %>%
#   select(Order.q, nt, Nutrients, Invasion, group_size, qD, q_std_dev) %>%
#   #group_by(Order.q, Nutrients) %>%
#   gather(variable, value, -(Order.q:Invasion)) %>%
#   unite(temp, Invasion, variable) %>%
#   spread(temp, value) 
# 
# 
# head(alph_gam_TD)
# 
# lrr_TD <- escalc(measure = "ROM", m1i=Late_qD, m2i=Early_qD, sd1i= Late_q_std_dev, sd2i= Early_q_std_dev,
#                  n1i=Late_group_size , n2i=Early_group_size,
#        data=alph_gam_TD, slab=paste0(Order.q , ", ", nt, ", ",Nutrients), digits=4)
# 
# head(lrr_TD)
# 
# 
# forest(lrr_TD$yi, lrr_TD$vi)
# 
# 
# hd_TD <- escalc(measure = "SMD", m1i=Late_qD, m2i=Early_qD, sd1i= Late_q_std_dev, sd2i= Early_q_std_dev,
#                  n1i=Late_group_size , n2i=Early_group_size,
#                  data=alph_gam_TD, slab=paste0(Order.q , ", ", nt, ", ",Nutrients), digits=4)
# 
# head(hd_TD)
# 
# 
# forest(hd_TD$yi, hd_TD$vi)


