

library(tidyverse)
library(brms)
library(ggplot2)
library(stringr)
library(yarrr)
library(patchwork)




# alpha
alpha_subplot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(alpha_subplot)

treats <- alpha_subplot %>% select(treatment, nutrients, invasion, Grass.forbs) %>%
  distinct()

alpha_subplot$treatment <- as.factor(alpha_subplot$treatment)
levels(alpha_subplot$treatment)

alpha_subplot2 <- alpha_subplot %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment, Nutrients , Invasion , Assembly , remove = FALSE) 

head(alpha_subplot2)



alpha_pie <-  brm(alpha_forb_ENSPIE ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | block/plot),
                   data = alpha_subplot2, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(alpha_pie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/alpha_pie.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/alpha_pie.Rdata')


summary(alpha_pie)

#plot(conditional_effects(alpha_rich), ask = FALSE)
alpha_nut_pie <- conditional_effects(alpha_pie, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

alpha_invasion_pie <- conditional_effects(alpha_pie, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

alpha_assembly_pie <- conditional_effects(alpha_pie, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects

piratepal(palette = "all")
piratepal(palette = "basel")

fig_alpha_nuts_pie <- ggplot() + 
  geom_point(data = alpha_subplot2,
             aes(x = Nutrients, y = alpha_forb_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_nut_pie$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = alpha_nut_pie$Nutrients,
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
  labs( subtitle= 'a) Nutrients'
  ) + ylab( expression(paste(ENS[PIE])) ) 


fig_alpha_nuts_pie


fig_alpha_inv_pie <- ggplot() + 
  geom_point(data = alpha_subplot2,
             aes(x = Invasion, y = alpha_forb_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_invasion_pie$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = alpha_invasion_pie$Invasion,
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
  labs( subtitle= 'c) Invasion'
  ) + ylab("")  


fig_alpha_inv_pie


fig_alpha_ass_pie <- ggplot() + 
  geom_point(data = alpha_subplot2,
             aes(x = Assembly, y = alpha_forb_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_assembly_pie$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = alpha_assembly_pie$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
#  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_alpha_ass_pie


fig_alpha_pie <- (fig_alpha_nuts_pie + fig_alpha_ass_pie + fig_alpha_inv_pie)


# Gamma Plot
gamma_plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_plot_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(gamma_plot)

gamma_plot$treatment <- as.factor(gamma_plot$treatment)
levels(gamma_plot$treatment)

gamma_plot2 <- gamma_plot %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment, Nutrients , Invasion , Assembly , remove = FALSE) 

head(gamma_plot2)


gamma_plot_pie <-  brm(gamma_forb_plot_ENSPIE ~ Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | block ),
                         data = gamma_plot2, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)

summary(gamma_plot_pie)


save(gamma_plot_rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/gamma_plot_pie.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/gamma_plot_pie.Rdata')


gamma_plot_nut_pie <- conditional_effects(gamma_plot_pie, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

gamma_plot_invasion_pie <- conditional_effects(gamma_plot_pie, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

gamma_plot_assembly_pie <- conditional_effects(gamma_plot_pie, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_gamma_plot_nuts_pie <- ggplot() + 
  geom_point(data = gamma_plot2,
             aes(x = Nutrients, y = gamma_forb_plot_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_nut_pie$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = gamma_plot_nut_pie$Nutrients,
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
  labs( subtitle= 'a) Nutrients'
  ) + ylab( expression(paste(ENS[PIE])) ) 


fig_gamma_plot_nuts_pie


fig_gamma_plot_inv_pie <- ggplot() + 
  geom_point(data = gamma_plot2,
             aes(x = Invasion, y = gamma_forb_plot_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_invasion_pie$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = gamma_plot_invasion_pie$Invasion,
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
  labs( subtitle= 'c) Invasion'
  ) + ylab("")  


fig_gamma_plot_inv_pie


fig_gamma_plot_ass_pie <- ggplot() + 
  geom_point(data = gamma_plot2,
             aes(x = Assembly, y = gamma_forb_plot_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_assembly_pie$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = gamma_plot_assembly_pie$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  #ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_gamma_plot_ass_pie


fig_gamma_plot_pie <- (fig_gamma_plot_nuts_pie  + fig_gamma_plot_ass_pie + fig_gamma_plot_inv_pie)



#  Gamma Block
gamma_block <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_block_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(gamma_block)


gamma_block2 <- gamma_block %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment, Nutrients , Invasion , Assembly , remove = FALSE) 

head(gamma_block2)


gamma_block_pie <-  brm(gamma_forb_block_ENSPIE ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly),
                         data = gamma_block2, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)



summary(gamma_block_pie)


save(gamma_block_rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/gamma_block_pie.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/gamma_block_pie.Rdata')


gamma_block_nut_pie <- conditional_effects(gamma_block_pie, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

gamma_block_invasion_pie <- conditional_effects(gamma_block_pie, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

gamma_block_assembly_pie <- conditional_effects(gamma_block_pie, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_gamma_block_nuts_pie <- ggplot() + 
  geom_point(data = gamma_block2,
             aes(x = Nutrients, y = gamma_forb_block_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_nut_pie$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = gamma_block_nut_pie$Nutrients,
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
  labs( subtitle= 'a) Nutrients'
  ) + ylab( expression(paste(ENS[PIE])) ) 


fig_gamma_block_nuts_pie


fig_gamma_block_inv_pie <- ggplot() + 
  geom_point(data = gamma_block2,
             aes(x = Invasion, y = gamma_forb_block_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_invasion_pie$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = gamma_block_invasion_pie$Invasion,
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
  labs( subtitle= 'c) Invasion'
  ) + ylab("")  


fig_gamma_block_inv_pie


fig_gamma_block_ass_pie <- ggplot() + 
  geom_point(data = gamma_block2,
             aes(x = Assembly, y = gamma_forb_block_ENSPIE, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_assembly_pie$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = gamma_block_assembly_pie$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  +  
 # ggtitle( expression(atop(paste(italic(gamma),'-block-scale'),  paste('block = (18 subplots/2 plots = 81', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_gamma_block_ass_pie


fig_gamma_block_pie <- (fig_gamma_block_nuts_pie  + fig_gamma_block_ass_pie + fig_gamma_block_inv_pie)


#  beta Block
beta_block <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/beta_div_block.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(beta_block)


beta_block2 <- beta_block %>% 
  left_join(treats) %>% 
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first"))   %>%
  unite(treatment, Nutrients , Invasion , Assembly , remove = FALSE) 

head(beta_block2)

beta_block_pie <-  brm(beta_ENSPIE_block ~   Nutrients * Invasion * Assembly  + (  Nutrients * Invasion * Assembly  ),
                        data = beta_block2, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)

summary(beta_block_pie)


save(beta_block_rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/beta_block_pie.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/gamma_plot_pie.Rdata')


beta_block_nut_pie <- conditional_effects(beta_block_pie, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

beta_block_invasion_pie <- conditional_effects(beta_block_pie, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

beta_block_assembly_pie <- conditional_effects(beta_block_pie, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_beta_block_nuts_pie <- ggplot() + 
  geom_point(data = beta_block2,
             aes(x = Nutrients, y = beta_ENSPIE_block, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = beta_block_nut_pie$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = beta_block_nut_pie$Nutrients,
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
  labs( subtitle= 'a) Nutrients'
  ) +  ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) 


fig_beta_block_nuts_pie


fig_beta_block_inv_pie <- ggplot() + 
  geom_point(data = beta_block2,
             aes(x = Invasion, y = beta_ENSPIE_block, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = beta_block_invasion_pie$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = beta_block_invasion_pie$Invasion,
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
  labs( subtitle= 'c) Invasion'
  ) + ylab("")  


fig_beta_block_inv_pie


fig_beta_block_ass_pie <- ggplot() + 
  geom_point(data = beta_block2,
             aes(x = Assembly, y = beta_ENSPIE_block, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = beta_block_assembly_pie$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = beta_block_assembly_pie$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_beta_block_ass_pie


fig_beta_block_pie <- (fig_beta_block_nuts_pie  + fig_beta_block_ass_pie + fig_beta_block_inv_pie)


(fig_alpha_pie / fig_gamma_plot_pie)

(fig_gamma_block_pie / fig_beta_block_pie)



#pres
(fig_alpha / fig_alpha_pie )


(fig_gamma_plot / fig_gamma_plot_pie )

(fig_gamma_block / fig_gamma_block_pie )


(fig_beta_block / fig_beta_block_pie )
