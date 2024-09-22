




library(tidyverse)
library(brms)
library(ggplot2)
library(stringr)



# 'are grasses more abundant than forbs when they get in first??' - no
# 'is les cun more abundant under low nutrients?-' no
# do natives do better when les cun gets in late

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
  mutate(cover_type = case_when( cover_type == "forbs_no_lc" ~ "Seeded forbs",
                                 cover_type == "lc_cover" ~ "Lespedeza cuneata",
                                 cover_type ==  "gram_cover_sum" ~ "Graminoids")) %>%
  arrange(plot, subplot, block)

head(alpha_dat)

write.csv(alpha_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_group_cover.csv")

# should include subplot but model runs real slow
alpha_comp <-  brm(relative_cover ~  Invasion + cover_type + Invasion:cover_type   + ( 1  | block/plot),
                   data = alpha_dat, family = student(), cores = 4, iter = 3000, warmup = 1000, chains = 4)


alpha_inv <- conditional_effects(alpha_comp, effects = 'Invasion:cover_type', re_formula = NA, method = 'fitted')  # conditional effects


head(alpha_inv)

piratepal(palette = "all")
piratepal(palette = "pony")


alpha_dat$cover_type <- factor(alpha_dat$cover_type, levels=c("Lespedeza cuneata", "Graminoids", "Seeded forbs"))
  
fig_alpha_cover_inv <- ggplot() + 
  #facet_wrap(.~Invasion) +
  geom_point(data = alpha_dat,
             aes(x = cover_type, y = relative_cover, group = Invasion, fill=Invasion ), colour = "#C0C0C0",
             size = 0.75, alpha = 0.4, 
             position = position_jitterdodge( jitter.width =0.05, jitter.height = 0.45, dodge.width = 0.75, seed = NA)
             ) +
  geom_point(data = alpha_inv$`Invasion:cover_type`,
             aes(x = cover_type, y = estimate__,  group=Invasion, colour = Invasion), size = 3,
             position = position_dodge(width = 0.75)
             ) +
  geom_errorbar(data = alpha_inv$`Invasion:cover_type`,
                aes(x = cover_type, ymin = lower__, ymax = upper__, group = Invasion,colour = Invasion),
                size = 1, width = 0,
                position = position_dodge(width = 0.75)
                ) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c( "#EE0011FF","#0C5BB0FF"))  + 
  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  #coord_flip() +
  labs( subtitle= 'Invasion effects on cover groups'
  ) + ylab("Relative Cover")  #+

fig_alpha_cover_inv

# assembly

alpha_ass_comp <-  brm(relative_cover ~  Assembly + cover_type + Assembly:cover_type   + ( 1  | block/plot),
                   data = alpha_dat, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)

alpha_ass <- conditional_effects(alpha_ass_comp, effects = 'Assembly:cover_type', re_formula = NA, method = 'fitted')  # conditional effects


head(gamma_inv)

piratepal(palette = "all")
piratepal(palette = "pony")


alpha_dat$cover_type <- factor(alpha_dat$cover_type, levels=c("Lespedeza cuneata", "Graminoids", "Seeded forbs"))

fig_alpha_cover_ass <- ggplot() + 
  #facet_wrap(.~Assembly) +
  geom_point(data = alpha_dat,
             aes(x = cover_type, y = relative_cover, group = Assembly, fill=Assembly ), colour = "#C0C0C0",
             size = 0.75, alpha = 0.4, 
             position = position_jitterdodge( jitter.width =0.05, jitter.height = 0.45, dodge.width = 0.75, seed = NA)
  ) +
  geom_point(data = alpha_ass$`Assembly:cover_type`,
             aes(x = cover_type, y = estimate__,  group=Assembly, colour = Assembly), size = 3,
             position = position_dodge(width = 0.75)
  ) +
  geom_errorbar(data = alpha_ass$`Assembly:cover_type`,
                aes(x = cover_type, ymin = lower__, ymax = upper__, group = Assembly,colour = Assembly),
                size = 1, width = 0,
                position = position_dodge(width = 0.75)
  ) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c( "#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "bottom") +
  #coord_flip() +
  labs( subtitle= 'Assembly effects on cover groups'
  ) + ylab("Relative Cover")  #+

fig_alpha_cover_ass



# nutrients

alpha_nuts_comp <-  brm(relative_cover ~  Nutrients + cover_type + Nutrients:cover_type   + ( 1  | block/plot),
                       data = alpha_dat, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)

alpha_nuts <- conditional_effects(alpha_nuts_comp, effects = 'Nutrients:cover_type', re_formula = NA, method = 'fitted')  # conditional effects


head(gamma_inv)

piratepal(palette = "all")
piratepal(palette = "pony")


fig_alpha_cover_nuts <- ggplot() + 
  facet_wrap(.~Nutrients) +
  geom_point(data = alpha_dat,
             aes(x = cover_type, y = relative_cover, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_nuts$`Nutrients:cover_type`,
             aes(x = cover_type, y = estimate__, colour = cover_type), size = 3) +
  geom_errorbar(data = alpha_nuts$`Nutrients:cover_type`,
                aes(x = cover_type, ymin = lower__, ymax = upper__, colour = cover_type),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0", "#A1C720FF", "#16A08CFF", "#972C8DFF" ))  + 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'Nutrients'
  ) + ylab("Relative Cover")  


fig_alpha_cover_nuts







