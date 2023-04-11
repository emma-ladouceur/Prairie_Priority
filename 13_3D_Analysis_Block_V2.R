

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

View(div0.TD)

div0.TD %>% select(treat_id) %>% distinct() %>% arrange()

div2.TD <- TD %>% filter(Order.q == "q = 2",
                         Method == "Observed") 

# trt/block
TD_div0 <-  brm(qD ~  Treatment  + ( Treatment | block ),
                   data = div0.TD, family = student(), cores = 4, iter=2000, warmup=1000, chains = 4,
               control = list(adapt_delta = 0.99)
                )


save(TD_div0, file = '3D_Model_Fits/TD_block_div0.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div0.Rdata')


summary(TD_div0)

pp_check(TD_div0)

TD_div0_c <- conditional_effects(TD_div0, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

TD_div0_df <-
  as.data.frame(TD_div0_c$`Treatment`)%>% select(-c(qD, block)) %>% mutate( Treatment = reorder(Treatment, estimate__) )

TD_div0_df

div0.TD <- div0.TD %>% left_join(TD_div0_df)

head(div0.TD)
fig_TD_div0 <- ggplot() + 
  geom_point(data = div0.TD %>% mutate( Treatment = reorder(Treatment, estimate__) ),
             aes(x = Treatment, y = qD, colour = "#C0C0C0"), 
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div0_df %>% mutate( Treatment = reorder(Treatment, estimate__) ),
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = TD_div0_df  %>% mutate( Treatment = reorder(Treatment, estimate__) ),
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T, option = "plasma")+
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
TD_div2 <-  brm(qD ~  Treatment  + ( Treatment | block ),
                data = div2.TD, family = gaussian(), cores = 4, iter=2000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.99)
)


save(TD_div2, file = '3D_Model_Fits/TD_block_div2.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/TD_block_div2.Rdata')


summary(TD_div2)

pp_check(TD_div2)

TD_div2_c <- conditional_effects(TD_div2, effects = 'Treatment', re_formula = NA, method = 'fitted')  # conditional effects

TD_div2_df <-
  as.data.frame(TD_div2_c$`Treatment`) %>% select(-c(qD, block)) 

TD_div2_df

div2.TD <- div2.TD %>% left_join(TD_div2_df)

div2.TD

fig_TD_div2 <- ggplot() + 
  geom_point(data = div2.TD,
             aes(x = reorder(Treatment, `estimate__`), y = qD, colour = "#C0C0C0"), 
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = TD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = TD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='', title ='q = 2') +
  scale_color_viridis(discrete = T, option = 'plasma')+
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

fig_1<-(prairie.TD.fig /fig_TD_div0/ fig_TD_div2)

fig_1
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
PD_div0 <-  brm(qPD ~  Treatment  + ( Treatment | block ),
                data = div0.PD, family = student(), cores = 4, iter=4000, warmup=1000, chains = 4,
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
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div0_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = PD_div0_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T, option= 'plasma')+
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
head(div2.PD)

# trt/block
PD_div2 <-  brm(qPD ~  Treatment  + ( Treatment | block ),
                data = div2.PD, family = gaussian(), cores = 4, iter=4000, warmup=1000, chains = 4,
                control = list(adapt_delta = 0.9999,
                               max_treedepth = 13 )
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
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = PD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = PD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='', title = 'q = 2') +
  scale_color_viridis(discrete = T, option = 'plasma')+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 11) )+
  labs( 
   # title= 'q = 2'
  ) + ylab("Forb Phylogenetic Diversity")

fig_PD_div2
#12X14 landscape
fig_2 <- (prairie.PD.fig / fig_PD_div0/ fig_PD_div2)
fig_2
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
FD_div0 <-  brm(qFD ~  Treatment  + ( Treatment | block ),
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
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div0_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = FD_div0_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T, option= 'plasma')+
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
FD_div2 <-  brm(qFD ~  Treatment  + ( Treatment | block ),
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
             size = 2, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = FD_div2_df,
             aes(x = Treatment, y = estimate__, colour = Treatment), size = 3) +
  geom_errorbar(data = FD_div2_df,
                aes(x = Treatment, ymin = lower__, ymax = upper__, colour = Treatment),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_viridis(discrete = T, option = 'plasma')+
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

fig_3 <- ( prairie.FD.fig / fig_FD_div0/ fig_FD_div2 )

fig_3
# LANDSCAPE 12 X 14



head(div0.TD)
head(div2.TD)

TD_joint <-  div0.TD %>% select(Treatment, estimate__, lower__, upper__) %>%
  mutate(r_estimate = estimate__, r_lower = lower__, r_upper = upper__) %>%
  select(Treatment, r_estimate, r_lower,  r_upper ) %>% left_join(
     div2.TD %>% 
            select(Treatment, estimate__, lower__, upper__) %>%
            mutate(e_estimate = estimate__, e_lower = lower__, e_upper = upper__) %>%
            select(Treatment, e_estimate, e_lower,  e_upper ) ) %>%
  distinct()

TD_joint

fig_4a <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = TD_joint,
             aes(x = r_estimate, y = e_estimate, colour = Treatment
             ), size = 3) +
  geom_errorbar(data = TD_joint,
                aes(x = r_estimate, ymin = e_lower, ymax = e_upper,  colour = Treatment )) +
  geom_errorbarh(data = TD_joint,
                 aes(y = e_estimate, xmin = r_lower, xmax =  r_upper,  colour = Treatment )) +
  xlim(40,80) +
  ylim(20,50) +
   # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
   # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_viridis(discrete = T, option="plasma")  +
  labs(title= "Taxonomic Diversity")+
  ylab("Average TD q = 2") +
  xlab("Average TD q = 0") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") #+
  #guides(color=guide_legend(title="Treatment", ncol = 3))

# 8.50 X 14
fig_4a


PD_joint <-  div0.PD %>% select(Treatment, estimate__, lower__, upper__) %>%
  mutate(r_estimate = estimate__, r_lower = lower__, r_upper = upper__) %>%
  select(Treatment, r_estimate, r_lower,  r_upper ) %>% left_join(
    div2.PD %>% 
      select(Treatment, estimate__, lower__, upper__) %>%
      mutate(e_estimate = estimate__, e_lower = lower__, e_upper = upper__) %>%
      select(Treatment, e_estimate, e_lower,  e_upper ) ) %>%
  distinct()

PD_joint

fig_4b <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = PD_joint,
             aes(x = r_estimate, y = e_estimate, colour = Treatment
             ), size = 3) +
  geom_errorbar(data = PD_joint,
                aes(x = r_estimate, ymin = e_lower, ymax = e_upper,  colour = Treatment )) +
  geom_errorbarh(data = PD_joint,
                 aes(y = e_estimate, xmin = r_lower, xmax =  r_upper,  colour = Treatment )) +
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_viridis(discrete = T, option="plasma")  +
  labs(title= "Phylogenetic Diversity")+
  ylab("Average PD q = 2") +
  xlab("Average PD q = 0") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title="Treatment", ncol = 3))

# 8.50 X 14
fig_4b


FD_joint <-  div0.FD %>% select(Treatment, estimate__, lower__, upper__) %>%
  mutate(r_estimate = estimate__, r_lower = lower__, r_upper = upper__) %>%
  select(Treatment, r_estimate, r_lower,  r_upper ) %>% left_join(
    div2.FD %>% 
      select(Treatment, estimate__, lower__, upper__) %>%
      mutate(e_estimate = estimate__, e_lower = lower__, e_upper = upper__) %>%
      select(Treatment, e_estimate, e_lower,  e_upper ) ) %>%
  distinct()

FD_joint

fig_4c <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = FD_joint,
             aes(x = r_estimate, y = e_estimate, colour = Treatment
             ), size = 3) +
  geom_errorbar(data = FD_joint,
                aes(x = r_estimate, ymin = e_lower, ymax = e_upper,  colour = Treatment )) +
  geom_errorbarh(data = FD_joint,
                 aes(y = e_estimate, xmin = r_lower, xmax =  r_upper,  colour = Treatment )) +
  # xlim(40,80) +
  # ylim(20,50) +
  # scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_viridis(discrete = T, option="plasma")  +
  labs(title= "Functional Diversity")+
  ylab("Average FD q = 2") +
  xlab("Average FD q = 0") +
  theme_classic(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="none") #+
  #guides(color=guide_legend(title="Treatment", ncol = 3))

# 8.50 X 14
fig_4c

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect for controls
fig_4_legend <- g_legend(fig_4b)

#8.50X14
fig_4 <- (fig_4a + fig_4b + theme(legend.position="none") + fig_4c)/ (fig_4_legend) + plot_layout(heights = c(10,  2))
fig_4
