


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

head(presence_dat)

invasion.wide <- presence_dat %>% #select(plot, block, invasion, species, pres) %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', species)) %>% 
  group_by(block, plot, subplot, nutrients, invasion, Grass.forbs, Treatments, species2) %>% 
  summarise(pres = n_distinct(pres)) %>%
  spread(species2, pres, fill = 0)

head(invasion.wide)


# select certain columns from data
plot.info <- presence_dat %>% 
  select(plot, block, invasion) %>% distinct()

head(plot.info)

beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  early.plots = x %>% 
    filter(invasion == 'e')
  
  # fix for treatment labels
 late.plots = x %>% 
    filter(invasion == 'l' )
  
  out <- tibble()
  if(nrow(early.plots)>0){
    for(i in 1:nrow(early.plots)){
      beta = beta.pair(bind_rows(early.plots %>% 
                                   slice(i) %>% 
                                   select(-c(invasion)), 
                                 early.plots %>% 
                                   select(-c(invasion) )),
                       index.family = 'jaccard')
      # buid the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         invasion = late.plots$invasion,
                         jtu = as.matrix(beta$beta.jtu)[-1,1],
                         jne = as.matrix(beta$beta.jne)[-1,1],
                         group = i)
      )
    }
  }  
  # escape for no controls
  else{
    out = tibble(
      invasion = NA,
      jtu = NA,
      jne = NA,
      group = NA)
  }
  return(out)
}

head(invasion.wide)

wide.invasion <- bind_rows(
  left_join(invasion.wide, plot.info, 
            by = c('plot', 'block', 'invasion') ) %>% 
    group_by(block) %>% 
    nest_legacy(starts_with('sp_'), invasion)
)
  
View(wide.invasion)

# calculate the beta components
wide.invasion <- wide.invasion %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


View(wide.invasion)

beta.invasion.df = wide.invasion %>% 
  unnest_legacy(beta) %>%
  unite(col = pw_beta_group,
        c( block, invasion), sep = '_', remove = F) %>% 
  select(-group)

head(beta.invasion.df)

write.csv(beta.invasion.df,"./Data/beta.invasion.df.csv")



turnover.invade <- brm(jtu ~  invasion +   ( 1  | block),
                         family = zero_one_inflated_beta(),
                         data = beta.invasion.df,
                         inits = '0',
                         cores = 4, chains = 4)



nested.invade <- brm(jne ~  invasion +   ( 1  | block),
                      family = zero_inflated_beta(),
                      data = beta,
                      inits = '0',
                      cores = 4, chains = 4)

  
