



library(tidyverse)
library(patchwork)
library(MetBrewer)
library(grid)
library(gridExtra)
library(ggtext)

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
# Table_S1
ag_Div_Dat <- bind_rows( 
TD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.character(as.factor(nt)), Div = "TD"),

PD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt)) %>% mutate(Div = D),

FD %>%  filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt))  %>% mutate(Div = D)
) %>% mutate(n_samples = nt) %>% select(-c(Assemblage, Reftime, Type,Tau, nt, Method)) %>% 
  mutate(qD_Lower_CI = round(qD.LCL, 4), qD_Upper_CI = round(qD.UCL, 4), qD = round(qD, 4) ) %>%
  select(Div, Treatment, n_samples, Order.q, qD, qD_Lower_CI, qD_Upper_CI) %>% 
  arrange(rev(Div), n_samples, Order.q, qD)

head(ag_Div_Dat)

#write.csv(Table_S1, "Tables/Table_S1.csv")


#Div 
Effect_Size_Dat  <- TD %>% mutate(nt = as.factor(nt),  D = "TD") %>%
  bind_rows(FD, PD) %>%  
  filter(nt %in% c(2, 80)) %>% 
  separate(Treatment, c("Treatment", "Invasion"), remove = F) %>% 
  select(c(Order.q, nt, Treatment, Invasion, D, qD , qD.LCL, qD.UCL)) %>%
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
  select(-c(Invasion, q, response)) %>% distinct() %>%
  mutate(
    scale = case_when(
      nt == 80 ~ "\u03B3",  # Greek letter gamma (γ)
      nt == 2  ~ "\u03B1",  # Greek letter alpha (α)
      TRUE     ~ NA_character_  # Default to NA if no match
    )
  ) %>% select(-nt)

head(Effect_Size_Dat)
head(ag_Div_Dat)

#  Table_S2 <- Div %>% 
#   mutate(n_samples = nt,
#          Div = D) %>% select(-c( nt, D)) %>% 
#    mutate(Esq_Lower_CI = round(ESqd.LCL, 4), Esqd_Upper_CI = round(ESqd.UCL, 4) , Esqd_Lower_CI_log =  round(ESqd.LCL_log, 4), Esqd_Upper_CI_log= round(ESqd.UCL_log, 4),
#           ESqd = round(ESqd,4), ESqd_log = round(ESqd_log, 4) ) %>%
#    select(Div, Treatment, n_samples, Order.q, ESqd , Esq_Lower_CI ,  Esqd_Upper_CI,   ESqd_log, Esqd_Lower_CI_log, Esqd_Upper_CI_log) %>%
#    arrange(rev(Div), n_samples, Order.q, ESqd) 
#    
# View(Table_S2)
# 
# write.csv(Table_S2, "Tables/Table_S2.csv")

View(Div)

#Div_Dat
Div_Dat <- left_join( 
  ag_Div_Dat %>% filter(n_samples == 2) %>%
    mutate(qDa = qD, qDa_Lower_CI = qD_Lower_CI, qDa_Upper_CI = qD_Upper_CI) %>%
    select(Div, Treatment, Order.q, qDa, qDa_Lower_CI, qDa_Upper_CI),
  ag_Div_Dat %>% filter(n_samples == 80) %>%
    mutate(qDg = qD, qDg_Lower_CI = qD_Lower_CI, qDg_Upper_CI = qD_Upper_CI, D = Div) %>%
    select(Div, Treatment, Order.q, qDg, qDg_Lower_CI, qDg_Upper_CI)
) %>% mutate( qDb = round(qDg / qDa,4),
              qDb_Lower_CI = round(qDg_Lower_CI / qDa_Lower_CI, 4),
              qDb_Upper_CI = round(qDg_Upper_CI / qDa_Upper_CI,4)
)


head(Div_Dat)
Div_Dat$Treatment <- factor(Div_Dat$Treatment, levels = c("Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" ))

###ES

Effect_Size_BDiv_Dat <- Div_Dat %>% 
  separate(Treatment, c("Treatment", "Invasion"), remove = F) %>% 
  select(c(Order.q, Div,   Treatment, Invasion, qDb , qDb_Lower_CI, qDb_Upper_CI)) %>%
  gather(variable, value, -(Order.q:Invasion)) %>%
  unite(temp, Invasion, variable) %>%
  spread(temp, value) %>%
  mutate( ESqd = (Late_qDb - Early_qDb),
          ESqd.LCL = (Late_qDb_Lower_CI - Early_qDb_Lower_CI),
          ESqd.UCL = (Late_qDb_Upper_CI - Early_qDb_Upper_CI),
          # log effect sizes
          ESqd_log = ( log(Late_qDb) -  log(Early_qDb) ),
          ESqd.LCL_log = ( log(Late_qDb_Lower_CI) -  log(Early_qDb_Lower_CI) ),
          ESqd.UCL_log = ( log(Late_qDb_Upper_CI) -  log(Early_qDb_Upper_CI) ),
  ) %>% 
  gather(trt, response, c(Early_qDb:Late_qDb_Upper_CI)) %>%
  separate(trt, c("Invasion", "q"), extra = "merge", sep="_") %>% 
  select(-c(Invasion, q, response)) %>% distinct() %>%
  mutate(scale = "\u03B2") %>% mutate(D = Div) %>% select(-Div)

Effect_Size_BDiv_Dat

# alpha "\u03B1"
# beta \u03B2
# gamma \u03B3

ES_Div_Dat <- Effect_Size_Dat %>% bind_rows(Effect_Size_BDiv_Dat) %>%
  mutate(Treatment = Treatment) %>% select(-Treatment) %>%
  select(D, Order.q, Treatment, scale, ESqd,  ESqd.LCL,  ESqd.UCL,   ESqd_log, ESqd.LCL_log, ESqd.UCL_log)
          
head(ES_Div_Dat)
summary(ES_Div_Dat)

ES_Div_Dat$D <- factor(ES_Div_Dat$D, levels = c("TD", "PD", "FD"))
ES_Div_Dat$scale <- factor(ES_Div_Dat$scale, levels = c("\u03B1", "\u03B3", "\u03B2"))
# alpha "\u03B1"
# beta \u03B2
# gamma \u03B3


# Table_S3 <- ES_BDiv %>% 
#   # mutate(n_samples = nt,
#   #        Div = D) %>% select(-c( nt, D)) %>% 
#   mutate(Esqd_Lower_CI = round(ESqd.LCL, 2), ESqd_Upper_CI = round(ESqd.UCL, 2) , ESqd_Lower_CI_log =  round(ESqd.LCL_log, 2), ESqd_Upper_CI_log= round(ESqd.UCL_log, 2),
#          ESqd = round(ESqd,2), ESqd_log = round(ESqd_log, 2) ) %>%
#   select(D, Treatment,  Order.q, ESqd , Esqd_Lower_CI ,  ESqd_Upper_CI,   ESqd_log, ESqd_Lower_CI_log, ESqd_Upper_CI_log) %>%
#   arrange(rev(D),  Order.q, ESqd) 
# 
# Table_S3
# View(Table_S3)
# 
# write.csv(Table_S3, "Tables/Table_S3.csv")



Fig_2a <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       title="a)",
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' ')))) +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Treatment early invasion", 
                              "Control late invasion", "Treatment late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom", 
                                ) +
  coord_cartesian() 

Fig_2a

Fig_2b <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))), 
       subtitle=(expression( paste( italic(gamma), "-scale", sep = ' '))),   title = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_2b


Fig_2c <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(beta), "-scale", sep = ' '))), , title = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_2c


Fig_2d <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "d)",
        subtitle= "Effect Sizes"
  ) + ylab("Effect of late invasion")

Fig_2d


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Fig2_l_div <- g_legend(Fig_2a)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

Fig_2ad <- ( secondary_label + (Fig_2a + theme(legend.position="none")) + Fig_2b + Fig_2c + Fig_2d ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
Fig_2ad

#q2
Fig_2e <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))),  title = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() 

Fig_2e

Fig_2f <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))),   title = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_2f


Fig_2g <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))),  , title = "g)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_2g

Fig_2h <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

Fig_2h


Fig2_l_es <- g_legend(Fig_2h)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

Fig_2eh <- (secondary_label + Fig_2e + Fig_2f + Fig_2g + (Fig_2h + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
Fig_2eh


Fig_2 <- (Fig_2ad)/(Fig_2eh)/(Fig2_l_div)/(Fig2_l_es)+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))
Fig_2

# PD

Fig_3a <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' '))), title = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Treatment early invasion", 
                              "Control late invasion", "Treatment late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() 

Fig_3a

Fig_3b <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))), 
       subtitle=(expression( paste( italic(gamma), "-scale", sep = ' '))),   title = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_3b


Fig_3c <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(beta), "-scale", sep = ' '))), , title = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_3c


Fig_3d <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "d)",
        subtitle= "Effect Sizes"
  ) + ylab("Effect of late invasion")

Fig_3d


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Fig3_l_div <- g_legend(Fig_3a)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

Fig_3ad <- ( secondary_label + (Fig_3a + theme(legend.position="none")) + Fig_3b + Fig_3c + Fig_3d ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
Fig_3ad

#q2
Fig_3e <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))),  title = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() 

Fig_3e

Fig_3f <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))),   title = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_3f


Fig_3g <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))),  , title = "g)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_3g

Fig_3h <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

Fig_3h


Fig3_l_es <- g_legend(Fig_3h)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

Fig_3eh <- (secondary_label + Fig_3e + Fig_3f + Fig_3g + (Fig_3h + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
Fig_3eh


Fig_3 <- (Fig_3ad)/(Fig_3eh)/(Fig3_l_div)/(Fig3_l_es)+  # Adjust the relative widths
  plot_annotation(title = "Phylogenetic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))


Fig_3
# FD

Fig_4a <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' '))), title = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Treatment early invasion", 
                              "Control late invasion", "Treatment late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() 

Fig_4a

Fig_4b <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))), 
       subtitle=(expression( paste( italic(gamma), "-scale", sep = ' '))),   title = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_4b


Fig_4c <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(beta), "-scale", sep = ' '))), , title = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_4c


Fig_4d <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "d)",
        subtitle= "Effect Sizes"
  ) + ylab("Effect of late invasion")

Fig_4d


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Fig4_l_div <- g_legend(Fig_4a)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

Fig_4ad <- ( secondary_label + (Fig_4a + theme(legend.position="none")) + Fig_4b + Fig_4c + Fig_4d ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Functional Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
Fig_4ad

#q2
Fig_4e <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))),  title = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() 

Fig_4e

Fig_4f <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(gamma), "-diversity", sep = ' '))),   title = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+

Fig_4f


Fig_4g <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = Div_Dat %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = Div_Dat %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( italic(beta), "-diversity", sep = ' '))),  , title = "g)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() #+
Fig_4g

Fig_4h <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

Fig_4h


Fig4_l_es <- g_legend(Fig_4h)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

Fig_4eh <- (secondary_label + Fig_4e + Fig_4f + Fig_4g + (Fig_4h + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
Fig_4eh


Fig_4 <- (Fig_4ad)/(Fig_4eh)/(Fig4_l_div)/(Fig4_l_es)+  # Adjust the relative widths
  plot_annotation(title = "Functional Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))

Fig_4
# ES log
head(ES_Div_Dat)

#TD
Fig_S1a <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "a)",
        subtitle= "q = 0"
  ) + ylab("log[ Effect of late invasion ] ")

Fig_S1a

Fig_S1b <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "b)", subtitle = "q = 2"
  ) + ylab("")

Fig_S1b

secondary_labelTD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Taxonomic Diversity", 
           angle = 90,  size= 6, hjust = 0.5)


Fig_S1ab <- secondary_labelTD  + (Fig_S1a) + (Fig_S1b + theme(legend.position="none") )+  # Adjust the relative widths
  # plot_annotation(title = "Taxonomic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

Fig_S1ab

#PD
Fig_S1c <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "c)",
  ) + ylab("log[ Effect of late invasion ]")

Fig_S1c

Fig_S1d <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  labs( title= "d)",
  ) + ylab("")

Fig_S1d

secondary_labelPD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Phylogenetic Diversity", 
           angle = 90,  size= 6, hjust = 0.5)


Fig_S1cd <- secondary_labelPD + (Fig_S1c) + (Fig_S1d)+  # Adjust the relative widths
  # plot_annotation(title = "Phylogenetic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

Fig_S1cd
#FD
Fig_S1e <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  labs( title= "e)",
  ) + ylab("log[ Effect of late invasion ]")

Fig_S1e

Fig_S1f <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Treatment, group = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Treatment, group = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Treatment" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  labs( title= "f)",
  ) + ylab("")

Fig_S1f

secondary_labelFD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Functional Diversity", 
           angle = 90,  size= 6, hjust = 0.5)

Fig_S1ef <- secondary_labelFD + (Fig_S1e) + (Fig_S1f)+  # Adjust the relative widths
  # plot_annotation(title = "Phylogenetic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

Fig_S1ef

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

FigS1_l <- g_legend(Fig_S1b)


(Fig_S1ab)/(Fig_S1cd)/(Fig_S1ef)/(FigS1_l) +
  plot_layout(heights = c(12,12,12,2))

