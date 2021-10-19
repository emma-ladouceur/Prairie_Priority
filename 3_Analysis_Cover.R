






library(tidyverse)
library(brms)
library(ggplot2)
library(stringr)



# 'are grasses more abundant than forbs when they get in first??' 

# Some analysis and plotting

cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/cover_long.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(cover)

cover_lc <- cover %>% filter( species %in% "Lespedeza.cuneata") %>%
  mutate(lc_cover = relative_cover,
         lc_cover = round(lc_cover, 2)) %>%
  select(-c(X,cover_subplot_sum,species, relative_cover, cover))

head(cover_lc)

alpha_c <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(alpha_c)

alphas <- alpha_c %>% left_join(cover_lc) %>%
  mutate(forbs_no_lc = (forbs_cover_sum - lc_cover ))

head(alphas)

alpha_dat <- alphas %>% select(plot, subplot, block, nutrients, invasion, Grass.forbs, treatment,cover_subplot_sum, forbs_no_lc, lc_cover,gram_cover_sum) %>%
  gather(cover_type, cover, forbs_no_lc, lc_cover, gram_cover_sum)  %>%
  mutate(cover = replace(cover, is.na(cover) , 0)) %>% # if cover is.na = 0
  mutate(relative_cover = (cover/ cover_subplot_sum) * 100) %>%
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% select(-c(nutrients, invasion, Grass.forbs)) %>%
  arrange(plot, subplot, block)

head(alpha_dat)

write.csv(alpha_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_group_cover.csv")



alpha_comp <-  brm(relative_cover ~  cover_type + Nutrients * Invasion * Assembly  + (cover_type + Nutrients * Invasion * Assembly  | block/plot),
                   data = alpha_dat, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)



plot(conditional_effects(alpha_comp), ask = FALSE)



alpha_comp_dat <- conditional_effects(alpha_comp, effects = 'treatment:cover_type', re_formula = NA, method = 'fitted')  # conditional effects

head(alpha_comp_dat)



fig_1a <- ggplot() + 
  geom_point(data = alpha_dat,
             aes(x = treatment, y = cover, colour = 	"#C0C0C0"), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_comp_dat$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = alpha_comp_dat$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  #scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs( subtitle= 'a)'
  ) + ylab("Alpha Cover") 


fig_1a


# Some analysis and plotting
gamma_block_c <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_block_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(gamma_block_c)


gamma_block_dat <- gamma_block_c %>% select( block, treatment, gamma_forb_block_sum, gamma_grams_block_rel_sum) %>%
  gather(cover_type, cover, gamma_forb_block_sum, gamma_grams_block_rel_sum)  %>%
  mutate(cover = replace(cover, is.na(cover) , 0)) %>% # if cover is.na = 0
  arrange(block,treatment)

head(gamma_block_dat)

gamma_block_comp <-  brm(cover ~  treatment * cover_type + ( treatment * cover_type ),
                         data = gamma_block_dat, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)

summary(gamma_block_comp)
# or....?
# alpha_comp <-  brm(mvbind(forbs_cover_sum, gram_cover_sum) ~  treatment + ( 1 | p | block),
#                   data = alpha_c, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


plot(conditional_effects(gamma_block_comp), ask = FALSE)

gamma_block_comp_dat <- conditional_effects(gamma_block_comp, effects = 'treatment:cover_type', re_formula = NA, method = 'fitted')  # conditional effects

head(gamma_block_comp_dat)

#alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("old field","never-plowed"))



fig_1a <- ggplot() + 
  geom_point(data = alpha_dat,
             aes(x = interaction(treatment, cover_type), y = cover), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  # geom_point(data = gamma_block_comp_dat$site_status,
  #            aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  # geom_errorbar(data = gamma_block_comp_dat$site_status,
  #               aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
  #               size = 1, width = 0) +
  labs(x = '',
       y='') +
  #scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  labs( subtitle= 'a)'
  ) + ylab("Gamma Cover") 


fig_1a


