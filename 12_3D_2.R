
library(ape)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")

# cover and presence combined species data
sp <- read.csv("pres_and_cover_plot.csv", header= TRUE)
# corrected species list
corrected_sp_list <- read.csv("species_corrected_complete.csv", row.names = 1, header= TRUE)

# correct species  names  in cover data
head(corrected_sp_list)

new_sp <- corrected_sp_list %>% mutate(species = old_name,
                                       corrected_sp = name) %>%
  select(-c(old_name, name, morphotype)) %>%
  filter(!corrected_sp == "graminoid sp.")

head(new_sp)
nrow(new_sp)

prairie.prep <- sp %>% 
  # clean out and normalise some riff raff
  mutate(species = str_replace_all(species, 
                                   pattern = "\\.", replacement = "_")) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "\\_", replacement = " ")) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "spp ", replacement = "spp")) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "sp ", replacement = "spp")) %>%
  mutate(species = str_replace_all(species, 
                                   pattern = "spec", replacement = "spp")) %>%
  left_join(new_sp) %>%
  mutate(orig_species = species) %>% select(-species) %>%
  mutate_all(na_if,"") %>%
  mutate(species =
           ifelse(!is.na(corrected_sp),
                  corrected_sp,
                  orig_species)) %>%
  unite("treat_id" , nutrients, invasion, Grass.forbs, remove= FALSE) %>%
  unite("samp_id",  plot, subplot, block, sep="_", remove = FALSE) %>%
  # remove graminoid
  filter(!species == "graminoid") 

head(prairie.prep)
nrow(prairie.prep)

levels(prairie.prep$species)


write.csv(prairie.prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)


# full species list, for taxonomic diversity and phylo diversity
prairie.list <- prairie.prep %>%
  split(.$plot)

prairie.matrix.list <- purrr::map(prairie.list, ~ .x %>% 
                                    select(species, samp_id, pres) %>%
                                    distinct() %>%
                                    spread(key = samp_id, value = pres) %>%
                                    replace(is.na(.), 0) %>%
                                    column_to_rownames(var = "species") )


View(prairie.matrix.list)

# ========================================================================================================== #
#  Taxonomic diversity
# TD_est <- estimate3D(data = prairie.matrix.list, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', base = 'size',
#                       level = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nboot = 0)
# 
# View(TD_est)

TD_out <- iNEXT3D(data = prairie.matrix.list, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), nboot = 0)

View(TD_out)

save(TD_out, file = "TD_out.Rdata")


# ========================================================================================================== #
# Phylogenetic diversity
phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

# full species list, for taxonomic diversity and phylo diversity
phylo.list <- phylo.prep %>%
  split(.$plot)

phylo.matrix.list <- purrr::map(phylo.list, ~ .x %>% 
                                  select(species, samp_id, pres) %>%
                                  distinct() %>%
                                  spread(key = samp_id, value = pres) %>%
                                  replace(is.na(.), 0) %>%
                                  column_to_rownames(var = "species") )


View(phylo.matrix.list)

tree <- read.tree("phylo.tree.txt")

# need sp names to have underscores instead of space because phylo package does this
head(tree)


# PD_est <- estimate3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'size',
#                     # level = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
#                     level = c(20), nboot = 0,  PDtree = tree, PDtype = "PD") 
# 
# View(PD_est)

PD_out <- iNEXT3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                  # OR  # endpoint = 20, knots = 1,
                  nboot = 0,  PDtree = tree, PDtype = "PD") 

View(PD_out)


save(PD_out, file = "PD_out.Rdata")

# ========================================================================================================== #
#  Functional diversity

# match traits and corrected species list from cover data


traits <- read.csv("imputed_trait_matrix.csv",  header= TRUE)

sp_traits <- traits %>% select(species) %>%
  mutate(traits_sp = species) %>% 
  mutate(species = str_replace_all(species, 
                                   pattern = "\\_", replacement = " "))

head(sp_traits)


sp_list <- prairie.prep %>% select(species) %>% distinct() %>%
  mutate(cover_sp = species)

head(sp_list)

sp_match <- sp_list %>% full_join(sp_traits)  %>% 
  arrange(species) %>% filter(!is.na(traits_sp),
                              !is.na(cover_sp) ) %>% 
  select(species)

nrow(sp_match)

# traits incidence matrix version
trait.prep <- sp_match %>% left_join(prairie.prep) %>% arrange(plot,subplot,block)

traits <- traits %>% mutate(species = str_replace_all(species,
                                                      pattern = "\\_", replacement = " ")) 

traits_fixed <- sp_match %>% 
  left_join(traits)

write.csv(traits_fixed, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/imputed_trait_matrix_fixed.csv", row.names=FALSE)


# full species list, for taxonomic diversity
trait.list <- trait.prep %>%
  split(.$plot)

trait.matrix.list <- purrr::map(trait.list, ~ .x %>% 
                                  select(species, samp_id, pres) %>%
                                  distinct() %>%
                                  spread(key = samp_id, value = pres) %>%
                                  replace(is.na(.), 0) %>%
                                  column_to_rownames(var = "species") )


View(trait.matrix.list)

#imputed trait matrix
traits <- read.csv("imputed_trait_matrix_fixed.csv", row.names = 1, header= TRUE)


for (i in 1:ncol(traits)) {
  if (class(traits[,i]) == "character") traits[, i] <- factor(traits[,i], levels = unique(traits[, i]))
}
distM <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()


# FD_est <- estimate3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'size',
#                      #level = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
#                       nboot = 0,  FDdistM = distM)
# 
# View(FD_est)

FD_out <- iNEXT3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                 # size = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                  endpoint = 20, #knots = 1,
                  nboot = 0,  FDdistM = distM)

View(FD_out)

save(FD_out, file = "FD_out.Rdata")
