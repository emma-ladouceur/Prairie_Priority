
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
  mutate(qD_Lower_CI = round(qD.LCL, 4), qD_Upper_CI = round(qD.UCL, 4), qD = round(qD, 4) ) %>%
  select(Div, Treatment, n_samples, Order.q, qD, qD_Lower_CI, qD_Upper_CI) %>% 
  arrange(rev(Div), n_samples, Order.q, qD)

head(Table_S1)
Table_S1 %>% select(n_samples) %>% distinct()
Table_S1 %>% filter(Div == "FD")


beta_div <- left_join( 
  Table_S1 %>% filter(n_samples == 2) %>%
    mutate(qDa = qD, qDa_Lower_CI = qD_Lower_CI, qDa_Upper_CI = qD_Upper_CI) %>%
    select(Div, Treatment, Order.q, qDa, qDa_Lower_CI, qDa_Upper_CI),
  Table_S1 %>% filter(n_samples == 80) %>%
    mutate(qDg = qD, qDg_Lower_CI = qD_Lower_CI, qDg_Upper_CI = qD_Upper_CI) %>%
    select(Div, Treatment, Order.q, qDg, qDg_Lower_CI, qDg_Upper_CI)
) %>% mutate( qDb = round(qDg / qDa,4),
              qDb_Lower_CI = round(qDg_Lower_CI / qDa_Lower_CI, 4),
              qDb_Upper_CI = round(qDg_Upper_CI / qDa_Upper_CI,4)
)


beta_div
beta_div$Treatment <- factor(beta_div$Treatment, levels = c("Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" ))


#alpha

alpha_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Taxonomic ", italic(alpha), "-diversity", sep = ' '))), 
       title='Taxonomic diversity', subtitle = 'q = 0', tag = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

alpha_TD_fig0

alpha_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(alpha), "-diversity", sep = ' '))), 
       title='', subtitle = 'q = 2', tag = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

alpha_TD_fig <- (alpha_TD_fig0 + alpha_TD_fig2)
alpha_TD_fig


alpha_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Phylogenetic ", italic(alpha), "-diversity", sep = ' '))), 
       title='Phylogenetic diversity', subtitle = '', tag = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

alpha_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(alpha), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "d)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

alpha_PD_fig <- (alpha_PD_fig0 + alpha_PD_fig2)
alpha_PD_fig


alpha_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Functional ", italic(alpha), "-diversity", sep = ' '))), 
       title='Functional diversity', subtitle = '', tag = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

alpha_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(alpha), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")
alpha_FD_fig2
alpha_FD_fig <- (alpha_FD_fig0 + alpha_FD_fig2)
alpha_FD_fig



# "Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" 
trt.leg <- ggplot() +
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDa, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDa_Lower_CI, ymax = qDa_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="C")  +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  # scale_colour_manual( values = c("#31688e","#35b779",# "#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154"),
  #                      labels=c("Nutrients late invasion", "Control late invasion",
  #                               "Nutrients early invasion", "Control early invasion")) +  
  labs( color="Treatments")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 2))

trt.leg

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
trt.legend <- g_legend(trt.leg)

# 11X13
alpha.div <- (alpha_TD_fig  / alpha_PD_fig / ( alpha_FD_fig + theme(legend.position="none") ) /  (trt.legend)   + plot_layout(heights = c(10,10,10,2))) 

alpha.div

#gamma


gamma_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Taxonomic ", italic(gamma), "-diversity", sep = ' '))), 
       title='Taxonomic diversity', subtitle = 'q = 0', tag = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

gamma_TD_fig0

gamma_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(gamma), "-diversity", sep = ' '))), 
       title='', subtitle = 'q = 2', tag = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

gamma_TD_fig <- (gamma_TD_fig0 + gamma_TD_fig2)
gamma_TD_fig


gamma_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Phylogenetic ", italic(gamma), "-diversity", sep = ' '))), 
       title='Phylogenetic diversity', subtitle = '', tag = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

gamma_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(gamma), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "d)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

gamma_PD_fig <- (gamma_PD_fig0 + gamma_PD_fig2)
gamma_PD_fig


gamma_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Functional ", italic(gamma), "-diversity", sep = ' '))), 
       title='Functional diversity', subtitle = '', tag = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

gamma_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(gamma), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(gamma), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")
gamma_FD_fig2
gamma_FD_fig <- (gamma_FD_fig0 + gamma_FD_fig2)
gamma_FD_fig



# "Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" 
trt.leg <- ggplot() +
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDg, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDg_Lower_CI, ymax = qDg_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="C")  +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  # scale_colour_manual( values = c("#31688e","#35b779",# "#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154"),
  #                      labels=c("Nutrients late invasion", "Control late invasion",
  #                               "Nutrients early invasion", "Control early invasion")) +  
  labs( color="Treatments")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 2))

trt.leg

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
trt.legend <- g_legend(trt.leg)

# 11X13
gamma.div <- (gamma_TD_fig  / gamma_PD_fig / ( gamma_FD_fig + theme(legend.position="none") ) /  (trt.legend)   + plot_layout(heights = c(10,10,10,2))) 

gamma.div

# beta




beta_TD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Taxonomic ", italic(beta), "-diversity", sep = ' '))), 
       title='Taxonomic diversity', subtitle = 'q = 0', tag = "a)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_TD_fig0

beta_TD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(beta), "-diversity", sep = ' '))), 
       title='', subtitle = 'q = 2', tag = "b)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_TD_fig <- (beta_TD_fig0 + beta_TD_fig2)
beta_TD_fig


beta_PD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Phylogenetic ", italic(beta), "-diversity", sep = ' '))), 
       title='Phylogenetic diversity', subtitle = '', tag = "c)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_PD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "PD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "PD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(beta), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "d)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_PD_fig <- (beta_PD_fig0 + beta_PD_fig2)
beta_PD_fig


beta_FD_fig0 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y=(expression( paste( "Functional ", italic(beta), "-diversity", sep = ' '))), 
       title='Functional diversity', subtitle = '', tag = "e)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_FD_fig2 <- ggplot() +  
  # geom_hline(yintercept = 0, lty = 2) +
  #facet_wrap(~Order.q, scales="free")+
  geom_point(data = beta_div %>% filter(Div == "FD") %>% filter(Order.q == "q = 2"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "FD")%>% filter(Order.q == "q = 2"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="", y="",#(expression( paste( "Taxonomic ", italic(beta), "-diversity", sep = ' '))), 
       title='', subtitle = '', tag = "f)") +
  scale_color_manual(values=met.brewer("Tam", 4), )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.title.x = element_blank(),axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18),
                               strip.background = element_blank(),legend.position = "none", plot.subtitle = element_text(hjust = 0.5)) +
  coord_cartesian() #+
#scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
# labs( tag= "a)",
#       title= (expression(paste(italic(alpha), "-diversity", sep = ' ')))
# ) + ylab("q = 0 \n Effect of late invasion")

beta_FD_fig <- (beta_FD_fig0 + beta_FD_fig2)
beta_FD_fig



# "Control_Early",  "Nutrients_Early",  "Control_Late", "Nutrients_Late" 
trt.leg <- ggplot() +
  geom_point(data = beta_div %>% filter(Div == "TD") %>% filter(Order.q == "q = 0"),
             aes(x = Treatment, y = qDb, colour = Treatment), size = 2,
             position = position_dodge(width = .60) ) +
  geom_errorbar(data = beta_div %>% filter(Div == "TD")%>% filter(Order.q == "q = 0"),
                aes(x = Treatment, ymin = qDb_Lower_CI, ymax = qDb_Upper_CI, colour = Treatment),
                size = 1, width = 0, position = position_dodge(width = .60)) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="C")  +
  scale_color_manual(values=met.brewer("Tam", 4),
                     labels=c("Control early invasion", "Nutrients early invasion", 
                              "Control late invasion", "Nutrients late invasion"
                     ))+
  # scale_colour_manual( values = c("#31688e","#35b779",# "#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154"),
  #                      labels=c("Nutrients late invasion", "Control late invasion",
  #                               "Nutrients early invasion", "Control early invasion")) +  
  labs( color="Treatments")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 2))

trt.leg

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
trt.legend <- g_legend(trt.leg)

# 11X13
beta.div <- (beta_TD_fig  / beta_PD_fig / ( beta_FD_fig + theme(legend.position="none") ) /  (trt.legend)   + plot_layout(heights = c(10,10,10,2))) 

beta.div

