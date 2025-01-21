

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



TD %>% select(Assemblage) %>% distinct()

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

head(Table_S1)
Table_S1 %>% select(n_samples) %>% distinct()

beta_div <- left_join( 
   Table_S1 %>% filter(n_samples == 2) %>%
  mutate(qDa = qD, qDa_Lower_CI = qD_Lower_CI, qDa_Upper_CI = qD_Upper_CI) %>%
  select(Div, Treatment, Order.q, qDa, qDa_Lower_CI, qDa_Upper_CI),
  Table_S1 %>% filter(n_samples == 80) %>%
    mutate(qDg = qD, qDg_Lower_CI = qD_Lower_CI, qDg_Upper_CI = qD_Upper_CI) %>%
    select(Div, Treatment, Order.q, qDg, qDg_Lower_CI, qDg_Upper_CI)
 ) %>% mutate( qDb = round(qDg / qDa,3),
                qDb_Lower_CI = round(qDg_Lower_CI / qDa_Lower_CI, 3),
               qDb_Upper_CI = round(qDg_Upper_CI / qDa_Upper_CI,3)
               )
  

beta_div


###ES

ES_BDiv <- beta_div %>% 
  separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
  select(c(Order.q, Div,   Nutrients, Invasion, qDb , qDb_Lower_CI, qDb_Upper_CI)) %>%
  gather(variable, value, -(Order.q:Invasion)) %>%
  unite(temp, Invasion, variable) %>%
  spread(temp, value) %>%
  mutate( ESqdb = (Late_qDb - Early_qDb),
          ESqdb.LCL = (Late_qDb_Lower_CI - Early_qDb_Lower_CI),
          ESqdb.UCL = (Late_qDb_Upper_CI - Early_qDb_Upper_CI),
          # log effect sizes
          ESqdb_log = ( log(Late_qDb) -  log(Early_qDb) ),
          ESqdb.LCL_log = ( log(Late_qDb_Lower_CI) -  log(Early_qDb_Lower_CI) ),
          ESqdb.UCL_log = ( log(Late_qDb_Upper_CI) -  log(Early_qDb_Upper_CI) ),
  ) %>% 
  gather(trt, response, c(Early_qDb:Late_qDb_Upper_CI)) %>%
  separate(trt, c("Invasion", "q"), extra = "merge", sep="_") %>% 
  select(-c(Invasion, q, response)) %>% distinct() 

ES_BDiv



Table_S3 <- ES_BDiv %>% 
  # mutate(n_samples = nt,
  #        Div = D) %>% select(-c( nt, D)) %>% 
  mutate(Esq_Lower_CI = round(ESqdb.LCL, 2), ESqdb_Upper_CI = round(ESqdb.UCL, 2) , ESqdb_Lower_CI_log =  round(ESqdb.LCL_log, 2), ESqdb_Upper_CI_log= round(ESqdb.UCL_log, 2),
         ESqdb = round(ESqdb,2), ESqdb_log = round(ESqdb_log, 2) ) %>%
  select(Div, Nutrients,  Order.q, ESqdb , Esq_Lower_CI ,  ESqdb_Upper_CI,   ESqdb_log, ESqdb_Lower_CI_log, ESqdb_Upper_CI_log) %>%
  arrange(rev(Div),  Order.q, ESqdb) 

View(Table_S3)

write.csv(Table_S3, "Tables/Table_S2.csv")


# reorder Div's
ES_BDiv$Div <- factor(ES_BDiv$Div  , levels=c("TD","PD", "FD"))


ES_BDiv

q_0_b <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_BDiv  %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = Div, y = ESqdb, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_BDiv %>% filter(Order.q ==  "q = 0" )  ,
                aes(x = Div, ymin = ESqdb.LCL, ymax = ESqdb.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "a)",
        title= (expression(paste(italic(beta), "-diversity", sep = ' ')))
  ) + ylab("q = 0 \n Effect of late invasion")

q_0_b


q_2_b <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_BDiv  %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = Div, y = ESqdb, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_BDiv %>% filter(Order.q ==  "q = 2" )  ,
                aes(x = Div, ymin = ESqdb.LCL, ymax = ESqdb.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  # labs(x = '',
  #      y='') +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( tag= "b)",
        #title= (expression(paste(italic(beta), "-diversity", sep = ' ')))
  ) + ylab("q = 2 \n Effect of late invasion")

q_2_b

B_ES_Fig <- (q_0_b / q_2_b)
B_ES_Fig


