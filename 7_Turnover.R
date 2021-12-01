


library(tidyverse)
library(purrr)
library(betapart)
library(brms)
library(ggplot2)
library(patchwork)


# so then we want to know how nested  one treatment is within another
sampled_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/sampled_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
pres <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# Presence Data:
# the species presence data (walk around checklist of every plot)
# again, the same 3 gamma's; plot, block, treat


####################################
# Presence Data:
# the species presence data (walk around checklist of every plot)
# again, the same 3 gamma's; plot, block, treat

head(pres)

pres_long <- pres %>% group_by(plot) %>%
  gather(species, pres, graminoid:Lindera.benzoin) %>%
  filter(!pres %in% c("0")) %>% # remove 0 values
  arrange(plot)

View(pres_long)
nrow(pres_long)

# use only sampled dat for blocks and site level data
head(sampled_dat)

# combine sampled cover and presence data to make a master 'plot' species list
sampled_pres <- sampled_dat %>% mutate( pres = 1) %>%
  select(plot, block, nutrients, invasion, Grass.forbs, treatment, species, pres) %>% distinct() %>%
  left_join(pres_long)

nrow(sampled_pres)
head(sampled_pres)



presence_dat <- sampled_pres %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment2, Nutrients , Invasion , Assembly , remove = FALSE) 

head(presence_dat)


write.csv(presence_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/pres_and_cover_plot.csv")

site_pres <- presence_dat %>% select(species, pres) %>% distinct() %>%
  mutate( Treatments = "site")

head(site_pres)
nrow(site_pres)

head(sampled_dat)

sub_plots <- sampled_dat %>% mutate( pres = 1) %>% distinct() %>%
  select(block, plot, subplot, nutrients, invasion, Grass.forbs, species, pres) %>%
  mutate( Treatments = 'treatments')

head(sub_plots)

presence_dat <- sub_plots %>% bind_rows(site_pres)

View(presence_dat)

trts.wide <- presence_dat %>% #select(plot, block, invasion, species, pres) %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', species)) %>% 
  group_by(block, plot, subplot, nutrients, invasion, Grass.forbs, Treatments, species2) %>% 
  summarise(pres = n_distinct(pres)) %>%
  spread(species2, pres, fill = 0) %>% ungroup()

head(trts.wide)


# select certain columns from data
plot.info <- presence_dat %>% 
  select(subplot, plot, block, nutrients, invasion, Grass.forbs,Treatments) %>% distinct()

View(plot.info)

site <- trts.wide %>% filter(Treatments == "site") %>% select(-c(block, plot, subplot, nutrients, invasion, Grass.forbs, Treatments)) %>%
  mutate(Treatments = "treatments") %>% left_join(plot.info) %>% select(-Treatments) %>%  mutate(Treatments = "site")

colnames(site)

trts.wide <- trts.wide %>% filter(Treatments == "treatments") %>%
  bind_rows(site)

View(trts.wide)


beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  site.pres = x %>% 
    filter(Treatments == 'site' )
  
  # fix for treatment labels
   trt.plots = x %>% 
    filter(Treatments == 'treatments')
  
  out <- tibble()
  if(nrow(site.pres)>0){
    for(i in 1:nrow(site.pres)){
      beta = beta.pair(bind_rows(site.pres %>% 
                                   slice(i) %>% 
                                   select(-c(Treatments)), 
                                 trt.plots %>% 
                                   select(-c(Treatments) )),
                       index.family = 'jaccard')
      # buid the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         #Treatments = site.pres$Treatments,
                         jtu = as.matrix(beta$beta.jtu)[-1,1],
                         jne = as.matrix(beta$beta.jne)[-1,1],
                         group = i)
      )
    }
  }  
  # escape for no controls
  else{
    out = tibble(
     # Treatments = NA,
      jtu = NA,
      jne = NA,
      group = NA)
  }
  return(out)
}

head(invasion.wide)

wide.trt <- bind_rows(
  left_join(trts.wide, plot.info, 
            by = c('subplot','plot', 'block', 'Treatments') ) %>% 
    group_by(subplot, plot, block) %>% 
    nest_legacy(starts_with('sp_'), Treatments )
)
  
View(wide.trt)

# calculate the beta components
wide.trt <- wide.trt %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


View(wide.trt)

beta.trt.df = wide.trt %>% 
  unnest_legacy(beta) %>%
  unite(col = pw_beta_group,
        c(subplot, plot, block), sep = '_', remove = F) %>% 
  select(-group, -data, -jtu)

View(beta.trt.df)



#write.csv(beta.trt.df,"./Data/beta.trt.df.csv")

head(beta.trt.df)

nesting <- beta.trt.df %>% left_join(plot.info) %>%
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )  

View(nesting)
 
nested.priority <- brm(jne ~  Nutrients * Invasion * Assembly + ( Nutrients * Invasion * Assembly | block/plot),
                      family = zero_inflated_beta(),
                      data = nesting,
                      inits = '0',
                      cores = 4, chains = 4)


 save(nested.priority, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/nested.Rdata')
#load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/nested.Rdata')


summary(nested.priority)

#plot(conditional_effects(alpha_rich), ask = FALSE)
nest_nut <- conditional_effects(nested.priority, effects = 'Nutrients', re_formula = NA, method = 'fitted')  # conditional effects

nest_invasion <- conditional_effects(nested.priority, effects = 'Invasion', re_formula = NA, method = 'fitted')  # conditional effects

nest_assembly <- conditional_effects(nested.priority, effects = 'Assembly', re_formula = NA, method = 'fitted')  # conditional effects

piratepal(palette = "all")
piratepal(palette = "basel")

fig_nest_nuts <- ggplot() + 
  geom_point(data = nesting,
             aes(x = Nutrients, y = jne, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = nest_nut$Nutrients,
             aes(x = Nutrients, y = estimate__, colour = Nutrients), size = 3) +
  geom_errorbar(data = nest_nut$Nutrients,
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
  ) + ylab("jnu")  


fig_nest_nuts


fig_nest_inv <- ggplot() + 
  geom_point(data = nesting,
             aes(x = Invasion, y = jne, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = nest_invasion$Invasion,
             aes(x = Invasion, y = estimate__, colour = Invasion), size = 3) +
  geom_errorbar(data = nest_invasion$Invasion,
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


fig_nest_inv


fig_nest_ass <- ggplot() + 
  geom_point(data = nesting,
             aes(x = Assembly, y = jne, colour = "#C0C0C0"), 
             size = 0.75, alpha = 0.4, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = nest_assembly$Assembly,
             aes(x = Assembly, y = estimate__, colour = Assembly), size = 3) +
  geom_errorbar(data = nest_assembly$Assembly,
                aes(x = Assembly, ymin = lower__, ymax = upper__, colour = Assembly),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#EC579AFF","#5A5895FF", "#15983DFF"))  + 
  ggtitle( expression(atop(paste(italic(alpha),'-scale'),  paste('subplot = (0.25', m^2 , ')' ) )) )+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position = "none") +
  #coord_flip() +
  labs( subtitle= 'b) Assembly'
  ) + ylab("")  


fig_nest_ass


fig_nest <- (fig_nest_nuts  + fig_nest_ass + fig_nest_inv)


fig_nest
  
