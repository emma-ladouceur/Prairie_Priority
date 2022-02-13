

library(tidyverse)
library(brms)
library(viridis)
library(patchwork)

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/")

TD <- read.csv("Data/Block Treat/prairie.hill.block.treats.TD.csv", header= TRUE)
PD <- read.csv("Data/Block Treat/prairie.hill.block.treats.PD.csv", header= TRUE)
FD <- read.csv("Data/Block Treat/prairie.hill.block.treats.FD.csv", header= TRUE)



# ========================================================================================================== #
#  Taxonomic diversity
head(TD)


div0.TD <- TD %>% filter(Order.q == "q = 0",
              Method == "Observed") 

head(div0.TD)


div2.TD <- TD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 


TD_div0 <-  brm(qD ~  Nutrients * Invasion * Assembly  + (  Nutrients * Invasion * Assembly | Assemblage),
                   data = div0.TD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(TD_div0, file = '3D_Model_Fits/TD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div0.Rdata')


summary(TD_div0)

#plot(conditional_effects(TD_div0_rich), ask = FALSE)
TD_div0_nut <- conditional_effects(TD_div0, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

TD_div0_invasion <- conditional_effects(TD_div0, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

TD_div0_assembly <- conditional_effects(TD_div0, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects

head(div0.TD)

fig_TD_div0_nuts <- ggplot() + 
  geom_point(data = div0.TD,
             aes(x = Nutrients, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div0_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = TD_div0_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( 
    subtitle= 'Nutrients'
  ) + ylab("Forb Taxonomic Diversity")


fig_TD_div0_nuts


fig_TD_div0_inv <- ggplot() + 
  geom_point(data = div0.TD,
             aes(x = Invasion, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div0_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = TD_div0_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Invasion'
  ) + ylab("")  


fig_TD_div0_inv


fig_TD_div0_ass <- ggplot() + 
  geom_point(data = div0.TD,
             aes(x = Assembly, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div0_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = TD_div0_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
 # ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  ggtitle('Observed Taxonomic Diversity \n q = 0')+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Assembly'
  ) + ylab("")  


fig_TD_div0_ass


fig_TD_div0 <- (fig_TD_div0_nuts  + fig_TD_div0_ass + fig_TD_div0_inv)
fig_TD_div0


# q= 2

TD_div2 <-  brm(qD ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | Assemblage),
                  data = div2.TD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(TD_div2, file = '3D_Model_Fits/TD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div2.Rdata')


summary(TD_div2)

#plot(conditional_effects(TD_div2_rich), ask = FALSE)
TD_div2_nut <- conditional_effects(TD_div2, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

TD_div2_invasion <- conditional_effects(TD_div2, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

TD_div2_assembly <- conditional_effects(TD_div2, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects



fig_TD_div2_nuts <- ggplot() + 
  geom_point(data = div2.TD,
             aes(x = Nutrients, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div2_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = TD_div2_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'a) Nutrients'
    y = "Forb Taxonomic Diversity"
  ) #+ ylab( expression(paste(ENS[PIE])) ) 


fig_TD_div2_nuts


fig_TD_div2_inv <- ggplot() + 
  geom_point(data = div2.TD,
             aes(x = Invasion, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div2_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = TD_div2_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'c) Invasion'
  ) + ylab("")  


fig_TD_div2_inv


fig_TD_div2_ass <- ggplot() + 
  geom_point(data = div2.TD,
             aes(x = Assembly, y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div2_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = TD_div2_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle('q = 2')+
  #  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'b) Assembly'
  ) + ylab("")  


fig_TD_div2_ass


fig_TD_div2 <- (fig_TD_div2_nuts + fig_TD_div2_ass + fig_TD_div2_inv)
fig_TD_div2

fig_TD <- (fig_TD_div0 / fig_TD_div2)

fig_TD


# ========================================================================================================== #
#  Phylogenetic diversity
head(PD)

div0.PD <- PD %>% filter(Order.q == "q = 0",
                        Method == "Observed") 

div2.PD <- PD %>% filter(Order.q == "q = 2",
                        Method == "Observed") 

head(div0.PD)

PD_div0 <-  brm(qPD ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | Assemblage),
                data = div0.PD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(PD_div0, file = '3D_Model_Fits/PD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/PD_block_div0.Rdata')


summary(PD_div0)

#plot(conditional_effects(PD_div0_rich), ask = FALSE)
PD_div0_nut <- conditional_effects(PD_div0, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

PD_div0_invasion <- conditional_effects(PD_div0, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

PD_div0_assembly <- conditional_effects(PD_div0, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_PD_div0_nuts <- ggplot() + 
  geom_point(data = div0.PD,
             aes(x = Nutrients, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div0_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = PD_div0_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Nutrients'
  ) + ylab("Forb Phylogenetic Diversity")


fig_PD_div0_nuts


fig_PD_div0_inv <- ggplot() + 
  geom_point(data = div0.PD,
             aes(x = Invasion, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div0_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = PD_div0_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Invasion'
  ) + ylab("")  


fig_PD_div0_inv


fig_PD_div0_ass <- ggplot() + 
  geom_point(data = div0.PD,
             aes(x = Assembly, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div0_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = PD_div0_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  #ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  ggtitle('Observed Phylogenetic Diversity \n q = 0') +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Assembly'
  ) + ylab("")  


fig_PD_div0_ass


fig_PD_div0 <- (fig_PD_div0_nuts  + fig_PD_div0_ass + fig_PD_div0_inv)
fig_PD_div0


# q= 2

PD_div2 <-  brm(qPD ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | Assemblage),
                data = div2.PD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(PD_div2, file = '3D_Model_Fits/PD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/PD_block_div2.Rdata')


summary(PD_div2)

#plot(conditional_effects(PD_div2_rich), ask = FALSE)
PD_div2_nut <- conditional_effects(PD_div2, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

PD_div2_invasion <- conditional_effects(PD_div2, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

PD_div2_assembly <- conditional_effects(PD_div2, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects



fig_PD_div2_nuts <- ggplot() + 
  geom_point(data = div2.PD,
             aes(x = Nutrients, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div2_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = PD_div2_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'a) Nutrients'
    y= "Forb Phylogenetic Diversity"
  )# + ylab( expression(paste(ENS[PIE])) ) 


fig_PD_div2_nuts


fig_PD_div2_inv <- ggplot() + 
  geom_point(data = div2.PD,
             aes(x = Invasion, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div2_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = PD_div2_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'Invasion'
  ) + ylab("")  


fig_PD_div2_inv


fig_PD_div2_ass <- ggplot() + 
  geom_point(data = div2.PD,
             aes(x = Assembly, y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div2_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = PD_div2_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle('q = 2') +
  #  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'Assembly'
  ) + ylab("")  


fig_PD_div2_ass


fig_PD_div2 <- (fig_PD_div2_nuts + fig_PD_div2_ass + fig_PD_div2_inv)
fig_PD_div2

fig_PD <- (fig_PD_div0 / fig_PD_div2)
fig_PD

# ========================================================================================================== #
#  Functional  diversity


head(FD)

div0.FD <- FD %>% filter(Order.q == "q = 0",
                        Method == "Observed") 

div2.FD <- FD %>% filter(Order.q == "q = 2",
                        Method == "Observed") 

head(div0.FD)

FD_div0 <-  brm(qFD ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | Assemblage),
                data = div0.FD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(FD_div0, file = '3D_Model_Fits/FD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/FD_block_div0.Rdata')


summary(FD_div0)

#plot(conditional_effects(FD_div0_rich), ask = FALSE)
FD_div0_nut <- conditional_effects(FD_div0, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

FD_div0_invasion <- conditional_effects(FD_div0, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

FD_div0_assembly <- conditional_effects(FD_div0, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects



fig_FD_div0_nuts <- ggplot() + 
  geom_point(data = div0.FD,
             aes(x = Nutrients, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div0_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = FD_div0_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Nutrients'
  ) + ylab("Forb Functional Diversity")


fig_FD_div0_nuts


fig_FD_div0_inv <- ggplot() + 
  geom_point(data = div0.FD,
             aes(x = Invasion, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div0_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = FD_div0_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Invasion'
  ) + ylab("")  


fig_FD_div0_inv


fig_FD_div0_ass <- ggplot() + 
  geom_point(data = div0.FD,
             aes(x = Assembly, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div0_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = FD_div0_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  #ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  ggtitle('Observed Functional Diversity \n q = 0') +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Assembly'
  ) + ylab("")  


fig_FD_div0_ass


fig_FD_div0 <- (fig_FD_div0_nuts  + fig_FD_div0_ass + fig_FD_div0_inv)
fig_FD_div0


# q= 2

FD_div2 <-  brm(qFD ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | Assemblage),
                data = div2.FD, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(FD_div2, file = '3D_Model_Fits/FD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/FD_block_div2.Rdata')


summary(FD_div2)

#plot(conditional_effects(FD_div2_rich), ask = FALSE)
FD_div2_nut <- conditional_effects(FD_div2, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

FD_div2_invasion <- conditional_effects(FD_div2, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

FD_div2_assembly <- conditional_effects(FD_div2, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects



fig_FD_div2_nuts <- ggplot() + 
  geom_point(data = div2.FD,
             aes(x = Nutrients, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div2_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = FD_div2_nut$Nutrients,
                aes(x = Nutrients, ymin = lower__, ymax = upper__, colour = Nutrients),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#969696FF", "#FB9F53FF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'Nutrients',
        y = "Forb Functional Diversity"
  ) #+ ylab( expression(paste(ENS[PIE])) ) 


fig_FD_div2_nuts


fig_FD_div2_inv <- ggplot() + 
  geom_point(data = div2.FD,
             aes(x = Invasion, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div2_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = FD_div2_invasion$Invasion,
                aes(x = Invasion, ymin = lower__, ymax = upper__, colour = Invasion),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#EE0011FF","#0C5BB0FF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'Invasion'
  ) + ylab("")  


fig_FD_div2_inv


fig_FD_div2_ass <- ggplot() + 
  geom_point(data = div2.FD,
             aes(x = Assembly, y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div2_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = FD_div2_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle('q = 2') +
  #  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( #subtitle= 'Assembly'
  ) + ylab("")  


fig_FD_div2_ass


fig_FD_div2 <- (fig_FD_div2_nuts + fig_FD_div2_ass + fig_FD_div2_inv)
fig_FD_div2

fig_FD <- (fig_FD_div0 / fig_FD_div2)
fig_FD




