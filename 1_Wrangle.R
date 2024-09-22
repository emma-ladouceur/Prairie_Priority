

# Prairie Priority Experiment

# libraries
library(tidyverse)
library(vegan)


# data
cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Subplot_Cover_Prairie_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
pres <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Species_Status_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(cover)
head(pres)
head(sp)



# plot : 10 x 10 m (presence- species walk-around)
# subplot : 0.5m x 0.5m (cover)

# treatments
# nutrients: nutrients applied (1), not applied (0)
# invasion:  les cun -early (2009) or late (2012)
# grass.forbs: grass first  (g), forbs first (f), both together (1 year priority) (b)

cover_long <- cover %>% group_by(plot, subplot, nutrients, invasion, Grass.forbs) %>%
  unite(treatment,nutrients,invasion,Grass.forbs, remove = FALSE) %>%
  gather(species, cover, graminoid:Lindera.benzoin) %>%
  filter(!cover %in% c("0")) %>% # remove 0 values
  mutate(cover = as.numeric(cover)) %>%
  filter(!is.na(cover)) %>% droplevels() %>%
  # create column named block, and group plots into blocks
  # according to supplementary information of Wohland et al. 
  mutate( block = ifelse(plot > 0  & plot < 27, '1',
         ifelse(plot > 26 & plot  < 52,  '2',
                ifelse(plot  > 51 & plot  < 77,  '3',
                       ifelse(plot > 76  & plot < 103, '4','other')))) 
         ) %>%
  arrange(plot, subplot, nutrients, invasion, Grass.forbs, treatment) 


summary(cover_long)
View(cover_long)

# this dataset only recorded forb species, and grouped all grasses into 'graminoid' and collected cover data on all grams
# so we can still get relative cover of all forbs per plot
cover_rel <- cover_long %>% group_by(plot, subplot, nutrients, invasion, Grass.forbs,treatment) %>%
  summarise(cover_subplot_sum = sum(cover) ) %>%
  left_join(cover_long) %>%
  mutate( relative_cover = (cover/cover_subplot_sum) * 100 ) %>%
  arrange(plot, subplot, nutrients, invasion, Grass.forbs,treatment) %>% ungroup()


head(cover_rel)


write.csv(cover_rel, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/cover_long.csv")


# we calculate the alpha of each subplot, nested within plots and treatments
# alpha scale = subplots, which are 0.5 x 0.5 m

alpha_forbs <- cover_rel %>% filter(!species %in% c("graminoid")) %>% 
group_by(plot, subplot,  block, nutrients, invasion, Grass.forbs,treatment) %>%
  summarise(
    forbs_cover_sum = sum(cover),
    alpha_forb_rich = n_distinct(species),
    alpha_forb_ENSPIE = vegan::diversity(relative_cover, index = 'invsimpson')
  ) %>% ungroup()


head(alpha_forbs)


alpha_grams <- cover_rel %>% filter(species %in% c("graminoid")) %>% 
  group_by(plot, subplot, block, nutrients, invasion, Grass.forbs,treatment) %>%
  summarise(
    gram_cover_sum = sum(cover)
  ) %>% ungroup()


head(alpha_grams)

alpha_dat <- cover_rel %>% select(plot, subplot, block,nutrients, invasion, Grass.forbs,treatment, cover_subplot_sum) %>%
  distinct() %>% left_join(alpha_forbs) %>% left_join(alpha_grams)

 head(alpha_dat)
 
 write.csv(alpha_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_composition.csv")
 
 
 head(cover_rel)
 
 # how are treatments nested spatially
 # important for next metrics at different scales
check_er_out <- cover_rel %>% select(treatment, plot, subplot, block) %>% 
   distinct()
   
View(check_er_out)   
   
trt_n_plot <- cover_rel %>% select(plot,subplot,block, treatment) %>%
  distinct(plot, subplot, block,treatment) %>%
  group_by( plot,block,treatment) %>%
  count() %>% # count number of treats
mutate(n_plot = n) %>% select(-n)

head(trt_n_plot )


n_subplots <- trt_n_plot %>% group_by(block, treatment) %>%
summarise(n_block = sum(n_plot)) %>% left_join(trt_n_plot) %>%
  arrange(plot)

head(n_subplots)



# we got 3 'gamma's'
# Composition:
# 1. gamma of treatments nested within each (10x10m) plot = (9 sub plots/plot  = 4.5 x 4.5 m)
# 2. within each block (18 subplots nested within 2 plots = 9 x 9 m)
# 3. treatment/site level - single number? could jack knife or bootstrap or something....




gamma_forbs_plot_prep <- cover_rel %>% left_join(n_subplots) %>%
  filter(!species %in% c("graminoid")) %>% 
  group_by(plot, block, treatment,nutrients, invasion, Grass.forbs,  n_plot, species) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_plot), # divide by the number of plots
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() 

head(gamma_forbs_plot_prep)

gamma_forbs_plot <- gamma_forbs_plot_prep %>% select(plot, block, treatment, species, species_av_p) %>%
  group_by(plot, block, treatment) %>%
  summarise(gamma_forb_plot_rich = n_distinct(species),
            gamma_forb_plot_rel_sum = sum(species_av_p),
            gamma_forb_plot_ENSPIE = vegan::diversity(species_av_p, index = 'invsimpson'))

head(gamma_forbs_plot)

gamma_grams_plot_prep <- cover_rel %>% left_join(n_subplots) %>%
  filter(species %in% c("graminoid")) %>% 
  group_by(plot, block, treatment,nutrients, invasion, Grass.forbs,  n_plot, species) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_plot), # divide by the number of plots
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() 

head(gamma_grams_plot_prep)

gamma_grams_plot <- gamma_grams_plot_prep %>% select(plot, block, treatment, species, species_av_p) %>%
  group_by(plot, block, treatment) %>%
  summarise(
            gamma_grams_plot_rel_sum = sum(species_av_p) )

head(gamma_grams_plot)


gamma_plot_dat <- cover_rel %>% select(plot,  block,nutrients, invasion, Grass.forbs,treatment) %>%
  distinct() %>% left_join(gamma_forbs_plot) %>% left_join(gamma_grams_plot)

head(gamma_plot_dat)

write.csv(gamma_plot_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_plot_composition.csv")

# within each block (18 subplots nested within 2 plots = 9 x 9 m)

# but first we need to sub-sample random plots from treatments with extra plots

plots_with_extra <- n_subplots %>% select(block, treatment, n_block) %>%
  distinct() %>% filter(n_block > 18)

head(plots_with_extra)


head(cover_rel)
head(n_subplots)

# 0_l_b is the treatment with extra
# so we filter by that treatment to just sub sample it
# and then add the sampled data back in
sampled_subplots <- cover_rel %>% left_join(n_subplots) %>% filter(treatment == "0_l_b") %>%
  select(plot,block,treatment, n_plot, n_block) %>% 
  distinct()%>%  group_by(block,treatment) %>%
  sample_n(2) %>% ungroup() # randomly sample two plots from every block


head(sampled_subplots)

samps <- sampled_subplots %>% left_join(cover_rel) %>%
  select(-n_plot, -n_block)

head(samps)

doub_check <- samps %>% select(plot,block,treatment) %>%
  distinct()

doub_check
# nice

sampled_dat <- cover_rel %>% filter(!treatment == "0_l_b") %>%
  bind_rows(samps)

write.csv(sampled_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/sampled_dat.csv")



head(sampled_dat)

# count number of plots again
trt_n_plot <- sampled_dat %>% select(plot,subplot,block, treatment) %>%
  distinct(plot, subplot, block,treatment) %>%
  group_by( plot,block,treatment) %>%
  count() %>% # count number of treats
  mutate(n_plot = n) %>% select(-n)

head(trt_n_plot )

n_block <- trt_n_plot %>% group_by(block, treatment) %>%
  summarise(n_block = sum(n_plot)) %>% left_join(trt_n_plot) %>%
  arrange(plot)

n_subplots <- trt_n_plot %>% group_by(treatment) %>%
  summarise(n_treat = sum(n_plot)) %>% left_join(trt_n_plot) %>%
  left_join(n_block) %>%
  arrange(plot)

head(n_subplots)

n_treat <- n_subplots %>% group_by(treatment, n_plot) %>%
  summarise(n_treat = sum(n_plot)) %>% select(-n_plot)

head(n_treat)

# should be same number of samples (subplots) for every block (18) 

# Now  we calculate gamma metrics within each block (18 subplots nested within 2 plots = 9 x 9 m)

gamma_forbs_block_prep <- sampled_dat %>% left_join(n_subplots) %>%
  filter(!species %in% c("graminoid")) %>% 
  group_by(block, treatment, nutrients, invasion, Grass.forbs,  n_block, species) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_block), # divide by the number of blocks
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() #%>% arrange() 

head(gamma_forbs_block_prep)

gamma_forbs_block <- gamma_forbs_block_prep %>% select(block,  treatment, species, species_av_p) %>%
  group_by(block,  treatment) %>%
  summarise(gamma_forb_block_rich = n_distinct(species),
            gamma_forb_block_sum = sum(species_av_p),
            gamma_forb_block_ENSPIE = vegan::diversity(species_av_p, index = 'invsimpson'))

head(gamma_forbs_block)


gamma_grams_block_prep <- sampled_dat %>% left_join(n_subplots) %>%
  filter(species %in% c("graminoid")) %>% 
  group_by(block, treatment, nutrients, invasion, Grass.forbs,  n_block, species) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_block), # divide by the number of plots
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() 

head(gamma_grams_block_prep)

gamma_grams_block <- gamma_grams_block_prep %>% select(block, treatment, species, species_av_p) %>%
  group_by(block, treatment) %>%
  summarise(
    gamma_grams_block_rel_sum = sum(species_av_p) )

head(gamma_grams_block)

gamma_block_dat <- sampled_dat %>% select(block,nutrients, invasion, Grass.forbs,treatment) %>%
  distinct() %>% left_join(gamma_forbs_block) %>% left_join(gamma_grams_block)

head(gamma_block_dat)

write.csv(gamma_block_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_block_composition.csv")


# treatment/site level - single number? could jack knife or bootstrap or something....

head(n_treat)

gamma_forbs_treat_prep <- sampled_dat %>%  left_join(n_treat) %>%
  filter(!species %in% c("graminoid")) %>% 
  group_by(treatment, nutrients, invasion, Grass.forbs, species, n_treat) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_treat), # divide by the number of blocks
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() #%>% arrange() 

head(gamma_forbs_treat_prep)

gamma_forbs_treat <- gamma_forbs_treat_prep %>% select(treatment, species, species_av_p) %>%
  group_by( treatment) %>%
  summarise(gamma_block_rich = n_distinct(species),
            species_block_sum = sum(species_av_p),
            gamma_block_ENSPIE = vegan::diversity(species_av_p, index = 'invsimpson'))

head(gamma_forbs_treat)


gamma_grams_treat_prep <- sampled_dat %>% left_join(n_treat) %>%
  filter(species %in% c("graminoid")) %>% 
  group_by(treatment, nutrients, invasion, Grass.forbs,  n_treat, species) %>%
  summarise( 
    species_sum = sum(relative_cover))   %>% # sum species rel cover
  mutate(species_av = (species_sum/n_treat), # divide by the number of plots
         species_av_p = (species_av), # make a proportion
         species_av_p = round(species_av_p,2),) %>% # round to 2 decimal places
  select(-c(species_sum, species_av)) %>%
  ungroup() 

head(gamma_grams_treat_prep)

gamma_grams_treat <- gamma_grams_treat_prep %>% select(treatment, species, species_av_p) %>%
  group_by(treatment) %>%
  summarise(
    gamma_grams_block_rel_sum = sum(species_av_p) )

head(gamma_grams_treat)

gamma_treat_dat <- sampled_dat %>% select(nutrients, invasion, Grass.forbs, treatment) %>%
  distinct() %>% left_join(gamma_forbs_treat) %>% left_join(gamma_grams_treat)

head(gamma_treat_dat, n= 12)

write.csv(gamma_treat_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/gamma_treat_composition.csv")



# Beta Diversity

head(alpha_dat)

alpha_mean <- alpha_dat %>% group_by(plot, block, treatment) %>%
  summarise(mean_alpha_rich = mean(alpha_forb_rich),
            mean_alpha_ENSPIE = mean(alpha_forb_ENSPIE))


head(alpha_mean)


beta_prep <- gamma_plot_dat %>% left_join(alpha_mean)

head(beta_prep)

beta_div_plot <- beta_prep %>% 
  mutate( beta_div_plot = (gamma_forb_plot_rich/mean_alpha_rich),
          beta_ENSPIE_plot = (gamma_forb_plot_ENSPIE/mean_alpha_ENSPIE)) %>%
  select(plot, block, treatment, beta_div_plot, beta_ENSPIE_plot)

head(beta_div_plot)


write.csv(beta_div_plot, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/beta_div_plot.csv")

head(alpha_dat)

alpha_mean <- alpha_dat %>% group_by( block, treatment) %>%
  summarise(mean_alpha_rich = mean(alpha_forb_rich),
            mean_alpha_ENSPIE = mean(alpha_forb_ENSPIE))


head(alpha_mean)


beta_prep <- gamma_block_dat %>% left_join(alpha_mean)

head(beta_prep)

beta_div_block <- beta_prep %>% 
  mutate( beta_div_block = (gamma_forb_block_rich/mean_alpha_rich),
          beta_ENSPIE_block = (gamma_forb_block_ENSPIE/mean_alpha_ENSPIE)) %>%
  select( block, treatment,beta_div_block, beta_ENSPIE_block)

head(beta_div_block)


write.csv(beta_div_block, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/beta_div_block.csv")



