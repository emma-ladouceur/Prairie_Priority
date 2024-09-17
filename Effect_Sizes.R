



library(tidyverse)
library(brms)
library(viridis)
library(viridisLite)
library(patchwork)
library(MetBrewer)

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

# Tables
Table_S1 <- bind_rows( 
TD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.character(as.factor(nt)), Div = "TD"),

PD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt)) %>% mutate(Div = D),

FD %>%  filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt))  %>% mutate(Div = D)
) %>% mutate(n_samples = nt) %>% select(-c(Assemblage, Reftime, Type,Tau, nt, Method)) %>% 
  mutate(qD_Lower_CI = round(qD.LCL, 2), qD_Upper_CI = round(qD.UCL, 2), qD = round(qD, 2) ) %>%
  select(Div, Treatment, n_samples, Order.q, qD, qD_Lower_CI, qD_Upper_CI) %>% 
  arrange(rev(Div), n_samples, Order.q, qD)

View(Table_S1)

write.csv(Table_S1, "Tables/Table_S1.csv")



Div  <- TD %>% mutate(nt = as.factor(nt),  D = "TD") %>%
  bind_rows(FD, PD) %>%  
  filter(nt %in% c(2, 80)) %>% 
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

head(Div)
head(Table_S1)

 Table_S2 <- Div %>% 
  mutate(n_samples = nt,
         Div = D) %>% select(-c( nt, D)) %>% 
   mutate(Esq_Lower_CI = round(ESqd.LCL, 2), Esqd_Upper_CI = round(ESqd.UCL, 2) , Esqd_Lower_CI_log =  round(ESqd.LCL_log, 2), Esqd_Upper_CI_log= round(ESqd.UCL_log, 2),
          ESqd = round(ESqd,2), ESqd_log = round(ESqd_log, 2) ) %>%
   select(Div, Nutrients, n_samples, Order.q, ESqd , Esq_Lower_CI ,  Esqd_Upper_CI,   ESqd_log, Esqd_Lower_CI_log, Esqd_Upper_CI_log) %>%
   arrange(rev(Div), n_samples, Order.q, ESqd) 
   
View(Table_S2)

write.csv(Table_S2, "Tables/Table_S2.csv")

View(Div)

# report results
Div %>% filter( D == "PD") %>% filter( Order.q == "q = 0") %>% filter(nt == 2)
Div %>% filter( D == "PD") %>% filter( Order.q == "q = 0") %>% filter(nt == 80)
#Div %>% filter( D == "TD") %>% filter( Order.q == "q = 2") %>% filter(nt == 1) # same as q=0
Div %>% filter( D == "PD") %>% filter( Order.q == "q = 2") %>% filter(nt == 80)



# reorder Div's
Div$D <- factor(Div$D  , levels=c("TD","PD", "FD"))


Div

q_0_smol <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 2 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 2 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "a)",
    title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
  ) + ylab("q = 0 \n Effect of late invasion")

q_0_smol

q_0_lorg <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "b)",
     title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
  ) + ylab("Effect of late invasion")

q_0_lorg

q_2_smol <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 2 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 2 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
   labs(tag= "c)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
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
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
   labs(tag= "d)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
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
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 2 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 2 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "a)",
    title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
  ) + ylab("q = 0 \n Log effect of late invasion")

q_0_smol_log

q_0_lorg_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 0" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(tag= "b)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
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
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 2 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 2 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(tag= "c)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
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
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = Div  %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
             aes(x = D, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div %>% filter(Order.q ==  "q = 2" ) %>% filter(nt == 80 ) ,
                aes(x = D, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "d)",
  ) + ylab("Log effect of late invasion")

q_2_lorg_log


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

es_legend <- g_legend(q_0_smol_log)

( (( (q_0_smol_log + theme(legend.position="none") ) + q_0_lorg_log) / (q_2_smol_log + q_2_lorg_log) ) / es_legend ) + plot_layout(heights = c(10, 10, 2))




