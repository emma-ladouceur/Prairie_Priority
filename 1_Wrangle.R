

# Prairie Priority Experiment

# libraries
library(tidyverse)
library(vegan)



# data
cover <- read.csv("~/Dropbox/_Projects/Prairie_Priority/Prairie_Subplot_Cover_Prairie_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
pres <- read.csv("~/Dropbox/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sp <- read.csv("~/Dropbox/_Projects/Prairie_Priority/Prairie_Species_Status_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


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


write.csv(cover_rel, "~/Dropbox/_Projects/Prairie_Priority/cover_long.csv")


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

head(sampled_dat)

write.csv(sampled_dat, "~/Dropbox/_Projects/Prairie_Priority/Data/sampled_dat.csv")

