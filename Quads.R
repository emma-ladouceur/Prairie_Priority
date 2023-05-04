



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
  filter(nt == c(1, 80)) %>%  
  select(c( Treatment, nt,  D, Order.q, qD , qD.LCL, qD.UCL))
  
  
Div.q <- Div %>%  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, Order.q, variable) %>%
  spread(temp, value) 

head(Div.q)

Div.q$D <- factor(Div.q$D  , levels=c("TD","PD", "FD"))

Div.q_Fig <- ggplot()+
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
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  scale_shape_discrete(labels=c( (expression(paste(italic(alpha), "-diversity (1 sample)", sep = ' '))) ,
                                 (expression(paste(italic(gamma), "-diversity (80 samples)", sep = ' ')))  ),name="Spatial scale")+
  labs(title= "") +
  ylab("Average D q = 2") +
  xlab("Average D q = 0") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")# +
#guides(shape=guide_legend(title="Spatial scale"))

Div.q_Fig



# alpha vs gamma
head(Div)

Div.s <- Div %>%  
  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, nt, variable) %>%
  spread(temp, value) 

head(Div.s)

Div.s$D <- factor(Div.s$D  , levels=c("TD","PD", "FD"))

Div.s_Fig <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~D, scales="free") +
  # overall effects
  geom_point(data = Div.s,
             aes(x = `1_qD`, y = `80_qD`,shape= Order.q , group=Order.q , colour = Treatment,
             ),   size = 3) +
  geom_errorbar(data = Div.s,
                aes(x = `1_qD`, ymin = `80_qD.LCL`, ymax = `80_qD.UCL`,  group=Order.q ,colour = Treatment )) +
  geom_errorbarh(data = Div.s,
                 aes(y =`80_qD`, xmin = `1_qD.LCL`, xmax =  `1_qD.UCL`,   group=Order.q , colour = Treatment )) +
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Hokusai3", 4), guide = "none" )+
  labs(title= "")+
  ylab( (expression(paste(italic(gamma), "-diversity", sep = ' '))) ) +
  xlab( (expression(paste(italic(alpha), "-diversity", sep = ' '))) ) +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"), legend.position="bottom") +
guides(shape = guide_legend(title="Order of q") )

Div.s_Fig


Div.legend <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(~D, scales="free") +
  # overall effects
  geom_point(data = Div.s,
             aes(x = `1_qD`, y = `80_qD`,group=Order.q , colour = Treatment,
             ),   size = 3) +
  geom_errorbar(data = Div.s,
                aes(x = `1_qD`, ymin = `80_qD.LCL`, ymax = `80_qD.UCL`,  group=Order.q ,colour = Treatment )) +
  geom_errorbarh(data = Div.s,
                 aes(y =`80_qD`, xmin = `1_qD.LCL`, xmax =  `1_qD.UCL`,   group=Order.q , colour = Treatment )) +
  scale_color_manual(values=met.brewer("Hokusai3", 4), 
                     labels=c("Nutrients late invasion", "Control late invasion",
                              "Nutrients early invasion", "Control early invasion") )+
  labs(title= "")+
  ylab( (expression(paste(italic(gamma), "-diversity", sep = ' '))) ) +
  xlab( (expression(paste(italic(alpha), "-diversity", sep = ' '))) ) +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"), legend.position="bottom") 

Div.legend


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

quads.legend <- g_legend(Div.legend)


(Div.q_Fig / Div.s_Fig/ quads.legend + plot_layout(heights = c(10,10,2) ) )


# Ds

head(Div)

Div.d <- Div %>%  
  gather(variable, value, (qD:qD.UCL)) %>%
  unite(temp, D, variable) %>%
  spread(temp, value) 

head(Div.d)


nt_names <- c(
  `1` = 'alpha-scale', 
  `80` = 'gamma-scale' 
)


Div.tpd_Fig <- ggplot()+
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
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Hokusai3", 4) )+
  labs(title= "")+
  ylab("Average PD") +
  xlab("Average TD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") #+
#guides(color=guide_legend(title="Treatment", ncol = 3))

Div.tpd_Fig


Div.tfd_Fig <- ggplot()+
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
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Hokusai3", 4), guide = "none" )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average TD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") #+
#guides(color=guide_legend(title="Treatment", ncol = 3))

Div.tfd_Fig



Div.pfd_Fig <- ggplot()+
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
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual(values=met.brewer("Hokusai3", 4), guide = "none" )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average PD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
guides(shape = guide_legend(title="Order of q") )

Div.pfd_Fig


Div_D_legend <- ggplot()+
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
  scale_color_manual(values=met.brewer("Hokusai3", 4), labels=c("Nutrients late invasion", "Control late invasion",
                                                                "Nutrients early invasion", "Control early invasion") )+
  labs(title= "")+
  ylab("Average FD") +
  xlab("Average PD") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(shape = guide_legend(title="Order of q") )

Div_D_legend

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

quads.D.legend <- g_legend(Div_D_legend)


(Div.tpd_Fig / Div.tfd_Fig/ Div.pfd_Fig/quads.D.legend + plot_layout(heights = c(10,10,10,2) )  )
