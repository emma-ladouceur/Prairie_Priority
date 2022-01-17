
library(ape)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")

# cover species data
sp <- read.csv("pres_and_cover_plot.csv", header= TRUE)
corrected_sp_list <- read.csv("species_corrected_complete.csv", row.names = 1, header= TRUE)


# correct species  names  in cover data
head(corrected_sp_list)

new_sp <- corrected_sp_list %>% mutate(species = old_name,
                             corrected_sp = name) %>%
  select(-c(old_name, name, morphotype))

head(new_sp)

correct_sp <- sp %>% 
  mutate(species = str_replace_all(species, 
                                   pattern = "\\.", replacement = " ")) %>%
  left_join(new_sp) %>%
  mutate(orig_species = species) %>% select(-species) %>%
  mutate_all(na_if,"") %>%
  mutate(species =
           ifelse(!is.na(corrected_sp),
                  corrected_sp,
                  orig_species)) 

head(correct_sp)

# match traits and corrected species list from cover data
traits <- read.csv("imputed_trait_matrix.csv",  header= TRUE)

sp_traits <- traits %>% select(species) %>%
  mutate(traits_sp = species) %>% 
  mutate(species = str_replace_all(species, 
                                   pattern = "\\_", replacement = " "))

head(sp_traits)

nrow(sp_traits)

sp_list <- correct_sp %>% select(species) %>% distinct() %>%
  mutate(cover_sp = species)

head(sp_list)

sp_match <- sp_list %>% full_join(sp_traits)  %>% 
  arrange(species) %>% filter(!is.na(traits_sp),
                              !is.na(cover_sp) ) %>% 
  select(species)

head(sp_match)
nrow(sp_match)
View(sp_match)

#  prep data for analysis
head(correct_sp)

 sp_matrix_prep <- correct_sp %>% 
     unite("treat_id" , Nutrients, Invasion, Assembly, remove= FALSE) %>%
   unite("samp_id",  plot, subplot, block, sep="_", remove = FALSE) 
   # mutate(species = str_replace_all(species, 
   #                               pattern = "\\.", replacement = "_"))

 head(sp_matrix_prep)
 View(sp_matrix_prep)
 
 

 #wrangle data into wide format  species vs plot matrix
 # species sample matrix with relative cover
 sp_matrix <- sp_matrix_prep %>% 
   select(species, pres, samp_id ) %>%
   spread(samp_id, pres) %>%
   mutate_all(funs(replace_na(.,0))) %>% 
  filter(!species == "graminoid") %>% arrange(species)

View(sp_matrix)


 write.csv(sp_matrix, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/inc_sp_matrix.csv", row.names=FALSE)
 
 View(sp_match)
 
 # traits incidence matrix version
 trait_sp <- sp_match %>% left_join(sp_matrix) 
 
nrow(sp_match)
nrow(trait_sp)
View(trait_sp)  

write.csv(trait_sp, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/traits_inc_sp_matrix.csv", row.names=FALSE)

head(traits)
nrow(traits)

traits <- traits %>% mutate(species = str_replace_all(species,
                                                     pattern = "\\_", replacement = " ")) 

traits_fixed<- sp_match %>% 
  left_join(traits)

nrow(traits_fixed)
View(traits_fixed)

write.csv(traits_fixed, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/imputed_trait_matrix_fixed.csv", row.names=FALSE)

# number of samps per plot
 # but make sure  it matches the matrix, or else the treatments get messed up
 #gather and spread mess this up too because of how r treats numbers as factors- so we need to be creative with the order
matrix_id <- read.csv("inc_sp_matrix.csv",  header= FALSE)
 
View(matrix_id)
 
plot_samps <- matrix_id %>% slice_head() %>% 
  gather(col, samp_id) %>% select(samp_id) %>%
  slice(-1) %>% separate(samp_id, c( "plot", "subplot", "block"), remove = FALSE)
  

samp_sum <- plot_samps %>% 
  select(plot, subplot) %>% group_by(plot) %>%
  summarise(n_samps = n_distinct(subplot)) %>%
  spread(plot, n_samps)


View(samp_sum)
ncol(samp_sum)

write.csv(samp_sum, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/plot_samps.csv")

 
 # OK now we are ready
??estimate3D

 # species matrix
 Inci_raw <- read.csv("inc_sp_matrix.csv",  row.names = 1, header= TRUE)
 # nt for incidence samps
 nT <- read.csv('plot_samps.csv', row.names = 1)
 # phylo tree
 tree <- read.tree("phylo.tree.txt")

 head(nT)
 
 

 #coverage only
 # Cmax <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
 #   apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', 2*x[1])) %>% min %>% round(., 4)
 # 
 # Cmin <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
 #   apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', x[1])) %>% min %>% round(., 4)

  
# ========================================================================================================== #
# Figure 3 - Taxonomic diversity
 prairie.out <- iNEXT3D(Inci_raw, diversity = 'TD', q = 0, t = 10, endpoint = 20,datatype = "incidence_freq")
 
 
TD_est <- estimate3D(data = Inci_raw, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', base = 'size',
                     level = 50, nboot = 0, nT = nT)
TD_obs <- obs3D(data = Inci_raw, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', nboot = 0, nT = nT)
TD_asy <- asy3D(data = Inci_raw, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', nboot = 0, nT = nT)

View(TD_est)

out_TD <- rbind(TD_est %>% select(Assemblage, Method, Order.q, qD,) , 
                TD_obs %>% select(Assemblage, Method,  Order.q, qD) ,
                  #mutate(goalSC = 'Observed'), 
                TD_asy %>% select(Assemblage, Method, Order.q, qD) 
                  #mutate(goalSC = 'Asymptotic')
                )
#fig_1_or_3(out_TD, y_label = 'Taxonomic diversity')

View(out_TD)
View(TD_obs)


# now need an incidence matrix, phylo tree and func div species list that matches perfectly for this to work

# ========================================================================================================== #
# Figure 3 - Phylogenetic diversity

PD_est <- estimate3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'size',
                     level = c(Cmin, Cmax), nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)
PD_obs <- obs3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)
PD_asy <- asy3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)


out_PD <- rbind(PD_est %>% select(Assemblage, Order.q, qPD, goalSC), 
                PD_obs %>% select(Assemblage, Order.q, qPD ) %>% mutate(goalSC = 'Observed'), 
                PD_asy %>% select(Assemblage, Order.q, qPD) %>% mutate(goalSC = 'Asymptotic'))



# ========================================================================================================== #
# Figure 3 - Functional diversity
Inci_raw <- read.csv("traits_inc_sp_matrix.csv",  row.names = 1, header= TRUE)
# nt for incidence samps
nT <- read.csv('plot_samps.csv', row.names = 1)
#imputed trait matrix
traits <- read.csv("imputed_trait_matrix_fixed.csv", row.names = 1, header= TRUE)


Cmax <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
  apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', 2*x[1])) %>% min %>% round(., 4)

Cmin <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
  apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', x[1])) %>% min %>% round(., 4)


for (i in 1:ncol(traits)) {
  if (class(traits[,i]) == "character") traits[, i] <- factor(traits[,i], levels = unique(traits[, i]))
}
distM <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()


FD_est <- estimate3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'size',
                     level = c(Cmin, Cmax), nboot = 0, nT = nT, FDdistM = distM)
FD_obs <- obs3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', 
                nboot = 0, nT = nT, FDdistM = distM)
FD_asy <- asy3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, FDdistM = distM)


out_FD <- rbind(FD_est %>% select(Assemblage, Order.q, qFD = qAUC, goalSC), 
                FD_obs %>% select(Assemblage, Order.q, qFD = qAUC) %>% mutate(goalSC = 'Observed'), 
                FD_asy %>% select(Assemblage, Order.q, qFD = qAUC) %>% mutate(goalSC = 'Asymptotic'))




 
 