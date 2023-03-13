

library(tidyverse)
library(brms)
library(viridis)
library(patchwork)

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/")

# 14 3D_treat_block.R
TD <- read.csv("Data/Block Treat/prairie.hill.block.treats.TD.csv", header= TRUE)
PD <- read.csv("Data/Block Treat/prairie.hill.block.treats.PD.csv", header= TRUE)
FD <- read.csv("Data/Block Treat/prairie.hill.block.treats.FD.csv", header= TRUE)



# ========================================================================================================== #
#  Taxonomic diversity
head(TD)

TD <- TD %>% unite(Treatment, c("Nutrients", "Assembly", "Invasion"), sep = " " )

div0.TD <- TD %>% filter(Order.q == "q = 0",
              Method == "Observed") 

head(div0.TD)

div0.TD %>% select(treat_id) %>% distinct() %>% arrange()

div2.TD <- TD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
TD_div0 <-  brm(qD ~  Treatment  + ( 1 | block ),
                   data = div0.TD, family = student(), cores = 4, iter=2000, warmup=1000, chains = 4,
               control = list(adapt_delta = 0.99)
                )


save(TD_div0, file = '3D_Model_Fits/TD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div0.Rdata')


summary(TD_div0)

pp_check(TD_div0)

TD_div0_c <- conditional_effects(TD_div0, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

TD_div0_df <-
  as.data.frame(TD_div0_c$`Treatment`)%>% select(-c(qD, block)) 

TD_div0_df

div0.TD <- div0.TD %>% left_join(TD_div0_df)


fig_TD_div0 <- ggplot() + 
  geom_point(data = div0.TD,
             aes(x = reorder(Treatment, `estimate__`), y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div0_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = TD_div0_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 0'
  ) + ylab("Forb Taxonomic Diversity")

fig_TD_div0


#---------------------------
div2.TD <- TD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
TD_div2 <-  brm(qD ~  Treatment  + ( 1 | block ),
                data = div2.TD, family = gaussian(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(TD_div2, file = '3D_Model_Fits/TD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div2.Rdata')


summary(TD_div2)

pp_check(TD_div2)

TD_div2_c <- conditional_effects(TD_div2, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

TD_div2_df <-
  as.data.frame(TD_div2_c$`Treatment`) %>% select(-c(qFD, block)) 

TD_div2_df

div2.TD <- div2.TD %>% left_join(TD_div2_df)


fig_TD_div2 <- ggplot() + 
  geom_point(data = div2.TD,
             aes(x = reorder(Treatment, `estimate__`), y = qD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = TD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 2'
  ) + ylab("Forb Taxonomic Diversity")

fig_TD_div2

(prairie.TD.fig /fig_TD_div0)


# ========================================================================================================== #
#  Phylo diversity
head(PD)

PD <- PD %>% unite(Treatment, c("Nutrients", "Assembly", "Invasion"), sep = " " )

div0.PD <- PD %>% filter(Order.q == "q = 0",
                         Method == "Observed") 

head(div0.PD)


div2.PD <- PD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
PD_div0 <-  brm(qPD ~  Treatment  + ( 1 | block ),
                data = div0.PD, family = student(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(PD_div0, file = '3D_Model_Fits/PD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/PD_block_div0.Rdata')


summary(PD_div0)

pp_check(PD_div0)

PD_div0_c <- conditional_effects(PD_div0, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

PD_div0_df <-
  as.data.frame(PD_div0_c$`Treatment`) %>% select(-c(qPD, block)) 

PD_div0_df

div0.PD <- div0.PD %>% left_join(PD_div0_df)

View(div0.PD)

fig_PD_div0 <- ggplot() + 
  geom_point(data = div0.PD,
             aes(x = reorder(Treatment, `estimate__`), y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div0_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = PD_div0_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 0'
  ) + ylab("Forb Phylogenetic Diversity")

fig_PD_div0


#---------------------------
div2.PD <- PD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
PD_div2 <-  brm(qPD ~  Treatment  + ( 1 | block ),
                data = div2.PD, family = gaussian(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(PD_div2, file = '3D_Model_Fits/PD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/PD_block_div2.Rdata')


summary(PD_div2)

pp_check(PD_div2)

PD_div2_c <- conditional_effects(PD_div2, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

PD_div2_df <-
  as.data.frame(PD_div2_c$`Treatment`) %>% select(-c(qPD, block)) 

PD_div2_df

div2.PD <- div2.PD %>% left_join(PD_div2_df)


fig_PD_div2 <- ggplot() + 
  geom_point(data = div2.PD,
             aes(x = reorder(Treatment, `estimate__`), y = qPD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = PD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 2'
  ) + ylab("Forb Phylogenetic Diversity")

fig_PD_div2

(prairie.PD.fig / fig_PD_div0)

# ========================================================================================================== #
#  Functional diversity
head(FD)

FD <- FD %>% unite(Treatment, c("Nutrients", "Assembly", "Invasion"), sep = " " )

div0.FD <- FD %>% filter(Order.q == "q = 0",
                         Method == "Observed") 

head(div0.FD)

div0.FD %>% select(treat_id) %>% distinct() %>% arrange()

div2.FD <- FD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
FD_div0 <-  brm(qFD ~  Treatment  + ( 1 | block ),
                data = div0.FD, family = student(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(FD_div0, file = '3D_Model_Fits/FD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/FD_block_div0.Rdata')


summary(FD_div0)

pp_check(FD_div0)

FD_div0_c <- conditional_effects(FD_div0, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

FD_div0_df <-
  as.data.frame(FD_div0_c$`Treatment`) %>% select(-c(qFD, block)) 

FD_div0_df

div0.FD <- div0.FD %>% left_join(FD_div0_df)

head(div0.FD)

fig_FD_div0 <- ggplot() + 
  geom_point(data = div0.FD,
             aes(x = reorder(Treatment, `estimate__`), y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div0_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = FD_div0_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 0'
  ) + ylab("Forb Functional Diversity")

fig_FD_div0


#---------------------------
div2.FD <- FD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
FD_div2 <-  brm(qFD ~  Treatment  + ( 1 | block ),
                data = div2.FD, family = gaussian(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(FD_div2, file = '3D_Model_Fits/FD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/FD_block_div2.Rdata')


summary(FD_div2)

pp_check(FD_div2)

FD_div2_c <- conditional_effects(FD_div2, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

FD_div2_df <-
  as.data.frame(FD_div2_c$`Treatment`) %>% select(-c(qFD, block)) 

FD_div2_df

div2.FD <- div2.FD %>% left_join(FD_div2_df)


fig_FD_div2 <- ggplot() + 
  geom_point(data = div2.FD,
             aes(x = reorder(Treatment, `estimate__`), y = qFD, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = FD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T)+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
    title= 'q = 2'
  ) + ylab("Forb Functional Diversity")

fig_FD_div2


( prairie.FD.fig / fig_FD_div0 )
