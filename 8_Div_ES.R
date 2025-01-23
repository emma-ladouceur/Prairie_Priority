



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
Table_S1 <- bind_rows( 
TD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.character(as.factor(nt)), Div = "TD"),

PD %>% filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt)) %>% mutate(Div = D),

FD %>%  filter(nt %in% c(2, 80)) %>% select(-c(SC, SC.LCL, SC.UCL)) %>%  mutate(nt = as.factor(nt))  %>% mutate(Div = D)
) %>% mutate(n_samples = nt) %>% select(-c(Assemblage, Reftime, Type,Tau, nt, Method)) %>% 
  mutate(qD_Lower_CI = round(qD.LCL, 4), qD_Upper_CI = round(qD.UCL, 4), qD = round(qD, 4) ) %>%
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
  select(-c(Invasion, q, response)) %>% distinct() %>%
  mutate(
    scale = case_when(
      nt == 80 ~ "\u03B3",  # Greek letter gamma (γ)
      nt == 2  ~ "\u03B1",  # Greek letter alpha (α)
      TRUE     ~ NA_character_  # Default to NA if no match
    )
  ) %>% select(-nt)

head(Div)
head(Table_S1)

 Table_S2 <- Div %>% 
  mutate(n_samples = nt,
         Div = D) %>% select(-c( nt, D)) %>% 
   mutate(Esq_Lower_CI = round(ESqd.LCL, 4), Esqd_Upper_CI = round(ESqd.UCL, 4) , Esqd_Lower_CI_log =  round(ESqd.LCL_log, 4), Esqd_Upper_CI_log= round(ESqd.UCL_log, 4),
          ESqd = round(ESqd,4), ESqd_log = round(ESqd_log, 4) ) %>%
   select(Div, Nutrients, n_samples, Order.q, ESqd , Esq_Lower_CI ,  Esqd_Upper_CI,   ESqd_log, Esqd_Lower_CI_log, Esqd_Upper_CI_log) %>%
   arrange(rev(Div), n_samples, Order.q, ESqd) 
   
View(Table_S2)

write.csv(Table_S2, "Tables/Table_S2.csv")

View(Div)


beta_div <- left_join( 
  Table_S1 %>% filter(n_samples == 2) %>%
    mutate(qDa = qD, qDa_Lower_CI = qD_Lower_CI, qDa_Upper_CI = qD_Upper_CI) %>%
    select(Div, Treatment, Order.q, qDa, qDa_Lower_CI, qDa_Upper_CI),
  Table_S1 %>% filter(n_samples == 80) %>%
    mutate(qDg = qD, qDg_Lower_CI = qD_Lower_CI, qDg_Upper_CI = qD_Upper_CI, D = Div) %>%
    select(Div, Treatment, Order.q, qDg, qDg_Lower_CI, qDg_Upper_CI)
) %>% mutate( qDb = round(qDg / qDa,4),
              qDb_Lower_CI = round(qDg_Lower_CI / qDa_Lower_CI, 4),
              qDb_Upper_CI = round(qDg_Upper_CI / qDa_Upper_CI,4)
)


beta_div
beta_div$Treatment <- factor(beta_div$Treatment, levels = c("Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" ))


###ES

ES_BDiv <- beta_div %>% 
  separate(Treatment, c("Nutrients", "Invasion"), remove = F) %>% 
  select(c(Order.q, Div,   Nutrients, Invasion, qDb , qDb_Lower_CI, qDb_Upper_CI)) %>%
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

ES_BDiv

# alpha "\u03B1"
# beta \u03B2
# gamma \u03B3

ES_Div_Dat <- Div %>% bind_rows(ES_BDiv)


Table_S3 <- ES_BDiv %>% 
  # mutate(n_samples = nt,
  #        Div = D) %>% select(-c( nt, D)) %>% 
  mutate(Esqd_Lower_CI = round(ESqd.LCL, 2), ESqd_Upper_CI = round(ESqd.UCL, 2) , ESqd_Lower_CI_log =  round(ESqd.LCL_log, 2), ESqd_Upper_CI_log= round(ESqd.UCL_log, 2),
         ESqd = round(ESqd,2), ESqd_log = round(ESqd_log, 2) ) %>%
  select(D, Nutrients,  Order.q, ESqd , Esqd_Lower_CI ,  ESqd_Upper_CI,   ESqd_log, ESqd_Lower_CI_log, ESqd_Upper_CI_log) %>%
  arrange(rev(D),  Order.q, ESqd) 

Table_S3
View(Table_S3)

write.csv(Table_S3, "Tables/Table_S3.csv")


beta_div$Treatment <- factor(beta_div$Treatment, levels = c("Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" ))
ES_Div_Dat$scale <- factor(ES_Div_Dat$scale, levels = c("\u03B1", "\u03B3", "\u03B2"))
# alpha "\u03B1"
# beta \u03B2
# gamma \u03B3

alpha_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       title="a)",
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' ')))) +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom", 
                                ) +
  coord_cartesian() 

alpha_TD_fig0

gamma_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
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

gamma_TD_fig0


beta_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
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
beta_TD_fig0


ES_TD0 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
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

ES_TD0


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

TD0_l <- g_legend(alpha_TD_fig0)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

TD0 <- ( secondary_label + (alpha_TD_fig0 + theme(legend.position="none")) + gamma_TD_fig0 + beta_TD_fig0 + ES_TD0 ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
TD0

#q2
alpha_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
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

alpha_TD_fig2

gamma_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
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

gamma_TD_fig2


beta_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
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
beta_TD_fig2

ES_TD2 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

ES_TD2


ES_TD2_l <- g_legend(ES_TD2)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

TD2 <- (secondary_label + alpha_TD_fig2 + gamma_TD_fig2 + beta_TD_fig2 + (ES_TD2 + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
TD2


(TD0)/(TD2)/(TD0_l)/(ES_TD2_l)+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))


# PD

alpha_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' '))), title = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() 

alpha_PD_fig0

gamma_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
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

gamma_PD_fig0


beta_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
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
beta_PD_fig0


ES_PD0 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
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

ES_PD0


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

PD0_l <- g_legend(alpha_PD_fig0)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

PD0 <- ( secondary_label + (alpha_PD_fig0 + theme(legend.position="none")) + gamma_PD_fig0 + beta_PD_fig0 + ES_PD0 ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
PD0

#q2
alpha_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
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

alpha_PD_fig2

gamma_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
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

gamma_PD_fig2


beta_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
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
beta_PD_fig2

ES_PD2 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

ES_PD2


ES_PD2_l <- g_legend(ES_PD2)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

PD2 <- (secondary_label + alpha_PD_fig2 + gamma_PD_fig2 + beta_PD_fig2 + (ES_PD2 + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
PD2


(PD0)/(PD2)/(PD0_l)/(ES_PD2_l)+  # Adjust the relative widths
  plot_annotation(title = "Phylogenetic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))



# FD

alpha_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression(  paste( italic(alpha), "-diversity", sep = ' '))), 
       subtitle=(expression( paste(italic(alpha), "-scale", sep = ' '))), title = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(), axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = -10)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() 

alpha_FD_fig0

gamma_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
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

gamma_FD_fig0


beta_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
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
beta_FD_fig0


ES_FD0 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
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

ES_FD0


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

FD0_l <- g_legend(alpha_FD_fig0)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 0", 
           angle = 0, size = 6, hjust = 0.5)

FD0 <- ( secondary_label + (alpha_FD_fig0 + theme(legend.position="none")) + gamma_FD_fig0 + beta_FD_fig0 + ES_FD0 ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))+  # Adjust the relative widths
  plot_annotation(title = "Taxonomic Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) 
FD0

#q2
alpha_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
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

alpha_FD_fig2

gamma_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
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

gamma_FD_fig2


beta_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
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
beta_FD_fig2

ES_FD2 <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL, ymax = ESqd.UCL, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "h)",
  ) + ylab("Effect of late invasion")

ES_FD2


ES_FD2_l <- g_legend(ES_FD2)

secondary_label <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "q = 2", 
           angle = 0,  size= 6, hjust = 0.5)

FD2 <- (secondary_label + alpha_FD_fig2 + gamma_FD_fig2 + beta_FD_fig2 + (ES_FD2 + theme(legend.position="none"))  ) + 
  plot_layout(widths = c(2.2, 10, 10, 10, 10))
FD2


(FD0)/(FD2)/(FD0_l)/(ES_FD2_l)+  # Adjust the relative widths
  plot_annotation(title = "Functional Diversity",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(heights = c(10,10,1,1))


# ES log


ES_Div_Dat

#TD
ES_TD0_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
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

ES_TD0_log

ES_TD2_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "TD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "bottom") +
  coord_cartesian() +
  labs( title= "b)", subtitle = "q = 2"
  ) + ylab("")

ES_TD2_log

secondary_labelTD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Taxonomic Diversity", 
           angle = 90,  size= 6, hjust = 0.5)


ES_TD_log <- secondary_labelTD  + (ES_TD0_log) + (ES_TD2_log + theme(legend.position="none") )+  # Adjust the relative widths
  # plot_annotation(title = "Taxonomic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

ES_TD_log

#PD
ES_PD0_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
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

ES_PD0_log

ES_PD2_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "PD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  labs( title= "d)",
  ) + ylab("")

ES_PD2_log

secondary_labelPD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Phylogenetic Diversity", 
           angle = 90,  size= 6, hjust = 0.5)


ES_PD_log <- secondary_labelPD + (ES_PD0_log) + (ES_PD2_log)+  # Adjust the relative widths
  # plot_annotation(title = "Phylogenetic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

ES_PD_log
#FD
ES_FD0_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 0" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
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

ES_FD0_log

ES_FD2_log <- ggplot() +  
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
             aes(x = scale, y = ESqd_log, colour = Nutrients, group = Nutrients), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = ES_Div_Dat  %>% filter(D == "FD") %>% filter(Order.q ==  "q = 2" ) ,
                aes(x = scale, ymin = ESqd.LCL_log, ymax = ESqd.UCL_log, colour = Nutrients, group = Nutrients),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  scale_color_manual(values=c("#bb292c", "#62205f"), name = "Effect size", labels=c("Control", "Nutrients" ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, margin = margin(b = 1)),
                               plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 1)),
                               strip.background = element_blank(),legend.position = "none") +
  coord_cartesian() +
  labs( title= "f)",
  ) + ylab("")

ES_FD2_log

secondary_labelFD <- ggplot() +
  theme_void() +  # Remove all default elements
  annotate("text", x = 1, y = 0.5, label = "Functional Diversity", 
           angle = 90,  size= 6, hjust = 0.5)

ES_FD_log <- secondary_labelFD + (ES_FD0_log) + (ES_FD2_log)+  # Adjust the relative widths
  # plot_annotation(title = "Phylogenetic Diversity",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) +
  plot_layout(widths = c(2, 10,10))

ES_FD_log

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ES_l <- g_legend(ES_TD2_log)


(ES_TD_log)/(ES_PD_log)/(ES_FD_log)/(ES_l) +
  plot_layout(heights = c(12,12,12,2))

