


pres <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sampled_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/sampled_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



####################################
# Presence Data:
# the species presence data (walk around checklist of every plot)
# again, the same 3 gamma's; plot, block, treat

head(pres)

pres_long <- pres %>% group_by(plot) %>%
  gather(species, pres, graminoid:Lindera.benzoin) %>%
  filter(!pres %in% c("0")) %>% # remove 0 values
  arrange(plot)

head(pres_long)
nrow(pres_long)

# use only sampled dat for blocks and site level data
head(sampled_dat)

sampled_pres <- sampled_dat %>%
  select(plot,treatment, block) %>% distinct() %>%
  left_join(pres_long)

nrow(sampled_pres)
head(sampled_pres)

# extract plot treatment and block info
plot_dat <- cover_long %>% ungroup() %>%
  select( plot, block, treatment, nutrients, invasion, Grass.forbs) %>%
  distinct()

head(plot_dat)

pres_dat <- plot_dat %>% left_join(pres_long) %>% ungroup()

head(pres_dat)




presence_dat <- pres_dat %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment2, Nutrients , Invasion , Assembly , remove = FALSE) 

head(presence_dat)



plot_pres_rich <- presence_dat %>%  filter(!species %in% c("graminoid")) %>% 
  group_by(plot, treatment, block, Nutrients, Assembly, Invasion) %>%
  summarise(pres_gamma_forb_plot_rich = n_distinct(species)) %>%
  left_join(plot_dat)

head(plot_pres_rich)

write.csv(plot_pres_rich, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_plot_pres.csv")


head(sampled_pres)
head(presence_dat)

sampled_pres_dat <- sampled_pres %>% 
  mutate(block = as.character(as.factor(block))) %>%
  left_join(presence_dat) %>% ungroup()

head(sampled_pres_dat)

block_pres_rich <- sampled_pres_dat %>%  filter(!species %in% c("graminoid")) %>% 
  group_by(treatment, block,  Nutrients, Assembly, Invasion) %>%
  summarise(pres_gamma_forb_block_rich = n_distinct(species)) 

head(block_pres_rich)

write.csv(block_pres_rich, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_block_pres.csv")


treat_pres_rich <- sampled_pres_dat %>%  filter(!species %in% c("graminoid")) %>% 
  group_by(treatment, Nutrients, Assembly, Invasion) %>%
  summarise(pres_gamma_forb_treat_rich = n_distinct(species)) 

head(treat_pres_rich)

write.csv(treat_pres_rich, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_treat_pres.csv")



head(plot_pres_rich)

plot_pres_rich_mod <-  brm(pres_gamma_forb_plot_rich ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | block/plot),
                   data = plot_pres_rich, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(plot_pres_rich_mod, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/pres_gamma_plot_rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/pres_gamma_plot_rich.Rdata')




gamma_plot_nut_pres <- conditional_effects(plot_pres_rich_mod, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

gamma_plot_invasion_pres <- conditional_effects(plot_pres_rich_mod, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

gamma_plot_assembly_pres <- conditional_effects(plot_pres_rich_mod, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_gamma_plot_nuts_pres <- ggplot() + 
  geom_point(data = plot_pres_rich,
             aes(x = Nutrients, y = pres_gamma_forb_plot_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_nut_pres$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = gamma_plot_nut_pres$Nutrients,
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
  ) + ylab("Forb species richness")  


fig_gamma_plot_nuts_pres


fig_gamma_plot_inv_pres <- ggplot() + 
  geom_point(data = plot_pres_rich,
             aes(x = Invasion, y = pres_gamma_forb_plot_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_invasion_pres$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = gamma_plot_invasion_pres$Invasion,
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


fig_gamma_plot_inv_pres


fig_gamma_plot_ass_pres <- ggplot() + 
  geom_point(data = plot_pres_rich,
             aes(x = Assembly, y = pres_gamma_forb_plot_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_plot_assembly_pres$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = gamma_plot_assembly_pres$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_gamma_plot_ass_pres


fig_gamma_plot_pres <- (fig_gamma_plot_nuts_pres  + fig_gamma_plot_ass_pres + fig_gamma_plot_inv_pres)



(fig_gamma_plot / fig_gamma_plot_pres)




# block




head(block_pres_rich)

block_pres_rich_mod <-  brm(pres_gamma_forb_block_rich ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | block),
                           data = block_pres_rich, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(block_pres_rich_mod, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/pres_gamma_block_rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/pres_gamma_plot_rich.Rdata')




gamma_block_nut_pres <- conditional_effects(block_pres_rich_mod, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

gamma_block_invasion_pres <- conditional_effects(block_pres_rich_mod, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

gamma_block_assembly_pres <- conditional_effects(block_pres_rich_mod, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects


fig_gamma_block_nuts_pres <- ggplot() + 
  geom_point(data = block_pres_rich,
             aes(x = Nutrients, y = pres_gamma_forb_block_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_nut_pres$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = gamma_block_nut_pres$Nutrients,
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
  ) + ylab("Forb species richness")  


fig_gamma_block_nuts_pres


fig_gamma_block_inv_pres <- ggplot() + 
  geom_point(data = block_pres_rich,
             aes(x = Invasion, y = pres_gamma_forb_block_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_invasion_pres$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = gamma_block_invasion_pres$Invasion,
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


fig_gamma_block_inv_pres


fig_gamma_block_ass_pres <- ggplot() + 
  geom_point(data = block_pres_rich,
             aes(x = Assembly, y = pres_gamma_forb_block_rich, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = gamma_block_assembly_pres$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = gamma_block_assembly_pres$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle( expression(atop(paste(italic(gamma),'-plot-scale'),  paste('plot = (9 subplots = 20.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_gamma_block_ass_pres


fig_gamma_block_pres <- (fig_gamma_block_nuts_pres  + fig_gamma_block_ass_pres + fig_gamma_block_inv_pres)



(fig_gamma_block / fig_gamma_block_pres)











# Combine Composition and Presence Species Lists

head(cover_rel)

head(pres_dat)


comp <- cover_rel %>% select(plot, block, nutrients, invasion, Grass.forbs, treatment, species) %>%
  distinct() %>%
  mutate( source = "composition")


head(comp)

pres <- cover_rel %>% select(plot, block, nutrients, invasion, Grass.forbs, treatment, species) %>%
  distinct() %>%
  mutate( source = "presence")

head(pres)


# species_list_combo <- pres %>% full_join(comp, by = c("plot", "block", "nutrients", "invasion", "Grass.forbs", "treatment", "species")) %>%
#   arrange(plot, block, treatment, species)
# 
# View(species_list_combo)

