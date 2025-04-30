



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
  filter(nt %in% c(2, 80)) %>%  
  select(c( Treatment, nt,  D, Order.q, qD , qD.LCL, qD.UCL))
  
  
Div.q <- Div %>%  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, Order.q, variable) %>%
  spread(temp, value) 

head(Div.q)

Div.q$D <- factor(Div.q$D  , levels=c("TD","PD", "FD"))
Div$Treatment <- factor(Div$Treatment, levels = c("Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" ))


Fig_S2_top_row <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~D, scales="free") +
  # overall effects
  geom_point(data = Div.q,
             aes(x = `q = 0_qD`, y = `q = 2_qD`, colour = Treatment, shape= nt, group=nt
             ), size = 3) +
  geom_errorbar(data = Div.q,
                aes(x = `q = 0_qD`, ymin = `q = 2_qD.LCL`, ymax = `q = 2_qD.UCL`,  colour = Treatment, group=nt)) +
  geom_errorbarh(data = Div.q,
                 aes(y =`q = 2_qD`, xmin = `q = 0_qD.LCL`, xmax =  `q = 0_qD.UCL`,  colour = Treatment, group=nt )) +
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Tam", 4, ), guide = "none" )+
  scale_shape_discrete(labels=c( (expression(paste(italic(alpha), "-diversity (2 samples)", sep = ' '))) ,
                                 (expression(paste(italic(gamma), "-diversity (80 samples)", sep = ' ')))  ),name="Spatial scale")+
  labs(title= "") +
  ylab("Average D q = 2") +
  xlab("Average D q = 0") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")# +
#guides(shape=guide_legend(title="Spatial scale"))

Fig_S2_top_row



# alpha vs gamma
head(Div)


Div.s <- Div %>%  
  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, nt, variable) %>%
  spread(temp, value) 

head(Div.s)

Div.s$D <- factor(Div.s$D  , levels=c("TD","PD", "FD"))


Fig_S2_bottom_row <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~D, scales="free") +
  # overall effects
  geom_point(data = Div.s,
             aes(x = `2_qD`, y = `80_qD`,shape= Order.q , group=Order.q , colour = Treatment,
             ),   size = 3) +
  geom_errorbar(data = Div.s,
                aes(x = `2_qD`, ymin = `80_qD.LCL`, ymax = `80_qD.UCL`,  group=Order.q ,colour = Treatment )) +
  geom_errorbarh(data = Div.s,
                 aes(y =`80_qD`, xmin = `2_qD.LCL`, xmax =  `2_qD.UCL`,   group=Order.q , colour = Treatment )) +
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Tam", 4), guide = "none" )+
  labs(title= "")+
  ylab( (expression(paste(italic(gamma), "-diversity", sep = ' '))) ) +
  xlab( (expression(paste(italic(alpha), "-diversity", sep = ' '))) ) +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"), legend.position="bottom") +
guides(shape = guide_legend(title="Order of q") )

Fig_S2_bottom_row


Fig_S2_legend <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~D, scales="free") +
  # overall effects
  geom_point(data = Div.s,
             aes(x = `2_qD`, y = `80_qD`,group=Order.q , colour = Treatment,
             ),   size = 3) +
  geom_errorbar(data = Div.s,
                aes(x = `2_qD`, ymin = `80_qD.LCL`, ymax = `80_qD.UCL`,  group=Order.q ,colour = Treatment )) +
  geom_errorbarh(data = Div.s,
                 aes(y =`80_qD`, xmin = `2_qD.LCL`, xmax =  `2_qD.UCL`,   group=Order.q , colour = Treatment )) +
  scale_color_manual(values=met.brewer("Tam", 4), 
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion" ) )+
  labs(title= "")+
  ylab( (expression(paste(italic(gamma), "-diversity", sep = ' '))) ) +
  xlab( (expression(paste(italic(alpha), "-diversity", sep = ' '))) ) +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"), legend.position="bottom") +   guides(col = guide_legend(ncol = 2))

Fig_S2_legend


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Fig_S2_l <- g_legend(Fig_S2_legend)

# normal landscape dimensions

Fig_S2 <- (Fig_S2_top_row / Fig_S2_bottom_row / Fig_S2_l + plot_layout(heights = c(10,10,2) ) )

Fig_S2

# Ds

head(Div)

Div.d <- Div %>%  
  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, D, variable) %>%
  spread(temp, value) 

head(Div.d)


nt_names <- c(
  `2` = 'alpha-scale', 
  `80` = 'gamma-scale' 
)


Fig_S3_top_row <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~nt, scales="free",
             labeller = labeller(nt  = as_labeller(nt_names,  label_parsed)) )  +
  # overall effects
  geom_point(data = Div.d,
             aes(x = `TD_qD`, y = `PD_qD`, colour = Treatment, shape= Order.q , group=Order.q 
             ), size = 3) +
  geom_errorbar(data = Div.d,
                aes(x = `TD_qD`, ymin = `PD_qD.LCL`, ymax = `PD_qD.UCL`,  colour = Treatment, group=Order.q )) +
  geom_errorbarh(data = Div.d,
                 aes(y =`PD_qD`, xmin = `TD_qD.LCL`, xmax =  `TD_qD.UCL`,  colour = Treatment, group=Order.q  )) +
  scale_color_manual(values=met.brewer("Tam", 4) )+
  labs(title= "")+
  ylab("Average PD") +
  xlab("Average TD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") 

Fig_S3_top_row


Fig_S3_middle_row <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~nt, scales="free",
             labeller = labeller(nt  = as_labeller(nt_names,  label_parsed)) )  +
  # overall effects
  geom_point(data = Div.d,
             aes(x = `TD_qD`, y = `FD_qD`, colour = Treatment, shape= Order.q , group=Order.q 
             ), size = 3) +
  geom_errorbar(data = Div.d,
                aes(x = `TD_qD`, ymin = `FD_qD.LCL`, ymax = `FD_qD.UCL`,  colour = Treatment, group=Order.q )) +
  geom_errorbarh(data = Div.d,
                 aes(y =`FD_qD`, xmin = `TD_qD.LCL`, xmax =  `TD_qD.UCL`,  colour = Treatment, group=Order.q  )) +
  scale_color_manual(values=met.brewer("Tam", 4), guide = "none" )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average TD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") 

Fig_S3_middle_row



Fig_S3_bottom_row <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~nt, scales="free",
             labeller = labeller(nt  = as_labeller(nt_names,  label_parsed)) )  +
  # overall effects
  geom_point(data = Div.d,
             aes(x = `PD_qD`, y = `FD_qD`, colour = Treatment, shape= Order.q , group=Order.q 
             ), size = 3) +
  geom_errorbar(data = Div.d,
                aes(x = `PD_qD`, ymin = `FD_qD.LCL`, ymax = `FD_qD.UCL`,  colour = Treatment, group=Order.q )) +
  geom_errorbarh(data = Div.d,
                 aes(y =`FD_qD`, xmin = `PD_qD.LCL`, xmax =  `PD_qD.UCL`,  colour = Treatment, group=Order.q  )) +
  scale_color_manual(values=met.brewer("Tam", 4), guide = "none" )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average PD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
guides(shape = guide_legend(title="Order of q") )

Fig_S3_bottom_row


Fig_S3_legend <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~nt, scales="free") +
  # overall effects
  geom_point(data = Div.d,
             aes(x = `PD_qD`, y = `FD_qD`, colour = Treatment, group=Order.q 
             ), size = 3) +
  geom_errorbar(data = Div.d,
                aes(x = `PD_qD`, ymin = `FD_qD.LCL`, ymax = `FD_qD.UCL`,  colour = Treatment, group=Order.q )) +
  geom_errorbarh(data = Div.d,
                 aes(y =`FD_qD`, xmin = `PD_qD.LCL`, xmax =  `PD_qD.UCL`,  colour = Treatment, group=Order.q  )) +
  scale_color_manual(values=met.brewer("Tam", 4), labels=c("Control early invasion", "Nutrients early invasion", 
                                                                "Control late invasion", "Nutrients late invasion") )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average PD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(shape = guide_legend(title="Order of q") ) +   guides(col = guide_legend(ncol = 2))

Fig_S3_legend

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

Fig_S3_l <- g_legend(Fig_S3_legend)

# normal portrait
(Fig_S3_top_row / Fig_S3_middle_row / Fig_S3_bottom_row / Fig_S3_l + plot_layout(heights = c(10,10,10,2) )  )
