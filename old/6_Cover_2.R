




library(tidyverse)
library(brms)
library(ggplot2)
library(stringr)
library(patchwork)
library(gridExtra)
library(grid)



cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/cover_long.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Species_Status_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(cover)
head(sp)

cover$species  <- gsub("[[:punct:]]", " ", cover$species ) 

head(cover)

sp <- sp %>% mutate(species = Species) %>% select(-Species) %>%
  mutate( Seeded = case_when( Seeded == "1" ~ "Seeded Forbs",
                              Seeded == "0" ~ "Not Seeded"),
          Exotic = case_when( Exotic == "1" ~"Exotic",
                     Exotic == "0" ~ "Native"),
          Invasive = case_when( Invasive == "1"~ "Invasive",
                     Invasive == "0" ~ "Not Invasive"),
          Graminoid = case_when( species == "graminoid" ~ "Graminoid"),
          
          ) %>% arrange(species)

head(sp)


cover <- cover %>% left_join(sp) %>%
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment2, Nutrients , Invasion , Assembly , remove = FALSE) 


head(cover)

View(sp)

cover %>% filter(is.na(Seeded))
nrow((cover %>% filter(is.na(Seeded))))


 cover_sums_seeds <- cover %>% group_by(subplot, plot, block, Nutrients,Invasion, Assembly, cover_subplot_sum, Seeded) %>%
   summarise(relative_cover = sum(relative_cover)) %>% filter( Seeded %in% "Seeded Forbs") %>%
   mutate(cover_group = Seeded) %>% select(-Seeded)

 head(cover_sums_seeds)

 
 
 cover_sums_ex <- cover %>% group_by(subplot, plot, block, Nutrients,Invasion, Assembly, cover_subplot_sum, Exotic) %>%
   summarise(relative_cover = sum(relative_cover)) %>% filter( Exotic %in% "Exotic") %>%
   mutate(cover_group = Exotic) %>% select(-Exotic)
 
 head(cover_sums_ex)

 
 
 cover_sums_inv <- cover %>% group_by(subplot, plot, block, Nutrients, Invasion, Assembly, cover_subplot_sum, Invasive) %>%
   summarise(relative_cover = sum(relative_cover)) %>% filter( Invasive %in% "Invasive") %>%
   mutate(cover_group = Invasive) %>% select(-Invasive)
 
 head(cover_sums_inv)
 
 cover_sums_grams <- cover %>% group_by(subplot, plot, block, Nutrients, Invasion, Assembly, cover_subplot_sum, Graminoid) %>%
   summarise(relative_cover = sum(relative_cover)) %>% filter( Graminoid %in% "Graminoid") %>%
   mutate(cover_group = Graminoid) %>% select(-Graminoid)
 
 head(cover_sums_grams)

 
  cover_sums <- cover_sums_grams %>% 
    bind_rows( #cover_sums_inv,
      cover_sums_ex, cover_sums_seeds ) %>%
    arrange(plot, subplot, block, Nutrients, Invasion, Assembly, cover_group, relative_cover) 
 
 
  
  head(cover_sums, n= 20)
 
 alpha_inv_comp <-  brm(relative_cover ~  Invasion + cover_group + Invasion:cover_group   + ( 1  | block/plot),
                    data = cover_sums, family = lognormal(), cores = 4, iter = 2000, warmup = 1000, chains = 4,
                    control = list(adapt_delta = 0.999)
                    )

 setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/")
 
 save(alpha_inv_comp, file = '3D_Model_Fits/alpha_inv_comp.Rdata')
 load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/alpha_inv_comp.Rdata')
 
 summary(alpha_comp)
 
 pp_check(alpha_inv_comp)
 
 alpha_inv <- conditional_effects(alpha_inv_comp, effects = 'Invasion:cover_group', re_formula = NA, method = 'fitted')  # conditional effects
 
 

 fig_alpha_cover_inv <- ggplot() + 
   #facet_wrap(.~Invasion) +
   geom_point(data = cover_sums,
              aes(x = cover_group, y = relative_cover, group = Invasion, fill=Invasion ), colour = "#C0C0C0",
              size = 0.75, alpha = 0.4, 
              position = position_jitterdodge( jitter.width =0.05, jitter.height = 0.45, dodge.width = 0.75, seed = NA)
   ) +
   geom_point(data = alpha_inv$`Invasion:cover_group`,
              aes(x = cover_group, y = estimate__,  group=Invasion, colour = Invasion), size = 3,
              position = position_dodge(width = 0.75)
   ) +
   geom_errorbar(data = alpha_inv$`Invasion:cover_group`,
                 aes(x = cover_group, ymin = lower__, ymax = upper__, group = Invasion,colour = Invasion),
                 size = 1, width = 0,
                 position = position_dodge(width = 0.75)
   ) +
   labs(x = '',
        y='') +
   scale_colour_manual( values = c("#fde725", "#440154") ) +  
   #ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
   theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                #axis.text.x = element_blank(),
                                plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                plot.title=element_text(size=18, hjust=0.5),
                                strip.background = element_blank(),legend.position = "bottom") +
   #coord_flip() +
   labs( subtitle= 'Invasion'
   ) + ylab("Relative Cover")  #+
 
 fig_alpha_cover_inv 
 
 # assembly
 
 alpha_ass_comp <-  brm(relative_cover ~  Assembly + cover_group + Assembly:cover_group   + ( 1  | block/plot),
                        data = cover_sums, family = lognormal(), cores = 4, iter=6000, warmup=3000, chains = 4,
                        control = list(adapt_delta = 0.9999)
                        )
 
 
 save(alpha_ass_comp, file = '3D_Model_Fits/alpha_ass_comp.Rdata')
 load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/alpha_ass_comp.Rdata')
 
 pp_check(alpha_ass_comp)
 
 
 alpha_ass <- conditional_effects(alpha_ass_comp, effects = 'Assembly:cover_group', re_formula = NA, method = 'fitted')  # conditional effects
 
 
 fig_alpha_cover_ass <- ggplot() + 
   #facet_wrap(.~Assembly) +
   geom_point(data = cover_sums,
              aes(x = cover_group, y = relative_cover, group = Assembly, fill=Assembly ), colour = "#C0C0C0",
              size = 0.75, alpha = 0.4, 
              position = position_jitterdodge( jitter.width =0.05, jitter.height = 0.45, dodge.width = 0.75, seed = NA)
   ) +
   geom_point(data = alpha_ass$`Assembly:cover_group`,
              aes(x = cover_group, y = estimate__,  group=Assembly, colour = Assembly), size = 3,
              position = position_dodge(width = 0.75)
   ) +
   geom_errorbar(data = alpha_ass$`Assembly:cover_group`,
                 aes(x = cover_group, ymin = lower__, ymax = upper__, group = Assembly,colour = Assembly),
                 size = 1, width = 0,
                 position = position_dodge(width = 0.75)
   ) +
   labs(x = '',
        y='') +
   scale_colour_manual( values = c( "#443983","#90d743","#21918c") ) +  
   ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
   theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                #axis.text.x = element_blank(),
                                plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                plot.title=element_text(size=18, hjust=0.5),
                                strip.background = element_blank(),legend.position = "bottom") +
   #coord_flip() +
   labs( subtitle= 'Assembly'
   ) + ylab("Relative Cover")  #+
 
 fig_alpha_cover_ass
 
 
 
 
 
 
 # nutrients
 
 alpha_nuts_comp <-  brm(relative_cover ~  Nutrients + cover_group + Nutrients:cover_group   + ( 1  | block/plot),
                         data = cover_sums, family = lognormal(), cores = 4, iter=6000, warmup=3000, chains = 4,
                         control = list(adapt_delta = 0.9999) )
 
 
 save(alpha_nuts_comp, file = '3D_Model_Fits/alpha_nuts_comp.Rdata')
 load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/3D_Model_Fits/alpha_nuts_comp.Rdata')
 
 
 alpha_nuts <- conditional_effects(alpha_nuts_comp, effects = 'Nutrients:cover_group', re_formula = NA, method = 'fitted')  # conditional effects
 
 
 head(gamma_inv)

 
 fig_alpha_cover_nuts <- ggplot() + 
   #facet_wrap(.~Assembly) +
   geom_point(data = cover_sums,
              aes(x = cover_group, y = relative_cover, group = Nutrients, fill=Nutrients ), colour = "#C0C0C0",
              size = 0.75, alpha = 0.4, 
              position = position_jitterdodge( jitter.width =0.05, jitter.height = 0.45, dodge.width = 0.75, seed = NA)
   ) +
   geom_point(data = alpha_nuts$`Nutrients:cover_group`,
              aes(x = cover_group, y = estimate__,  group=Nutrients, colour = Nutrients), size = 3,
              position = position_dodge(width = 0.75)
   ) +
   geom_errorbar(data = alpha_nuts$`Nutrients:cover_group`,
                 aes(x = cover_group, ymin = lower__, ymax = upper__, group = Nutrients,colour = Nutrients),
                 size = 1, width = 0,
                 position = position_dodge(width = 0.75)
   ) +
   labs(x = '',
        y='') +
   scale_colour_manual( values = c("#31688e","#35b779") ) +  
  #ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
   theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                #axis.text.x = element_blank(),
                                plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                plot.title=element_text(size=18, hjust=0.5),
                                strip.background = element_blank(),legend.position = "bottom") +
   #coord_flip() +
   labs( subtitle= 'Nutrients'
   ) + ylab("Relative Cover")  #+
 
 fig_alpha_cover_nuts
 
 
 # LANDCSAPE 8.50 15
 
( fig_alpha_cover_nuts + fig_alpha_cover_ass + fig_alpha_cover_inv  )
 
 g_legend<-function(a.gplot){
   tmp <- ggplot_gtable(ggplot_build(a.gplot))
   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
   legend <- tmp$grobs[[leg]]
   return(legend)} 
 
 
n.legend <- g_legend(fig_alpha_cover_nuts)
 
a.legend <- g_legend(fig_alpha_cover_ass)

i.legend <- g_legend(fig_alpha_cover_inv)

fig_4_legend <- grid.arrange( arrangeGrob(
  n.legend, 
  a.legend,  
  i.legend,
  ncol = 3, nrow = 1,
  heights = c(0.10)
 # layout_matrix = rbind(c(NA), c(NA), c(NA), c(1), c(2), c(3), c(NA), c(NA))
) )

fig_4_legend

almost_fig_4 <-( ( fig_alpha_cover_nuts  +  theme(legend.position= "none") ) + (fig_alpha_cover_ass +  theme(legend.position= "none") )+ (fig_alpha_cover_inv +  theme(legend.position= "none")  ) ) 

fig_4 <- (almost_fig_4/fig_4_legend)  + plot_layout(heights = c(10,2))

# LANDCSAPE 8.50 15
fig_4

