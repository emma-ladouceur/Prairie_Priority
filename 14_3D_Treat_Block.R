
library(viridis)
library(ape)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)
library(patchwork)

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

#prairie.info <- prairie.prep %>% select(treat_id)

#write.csv(prairie.prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)


prairie.prep.treats <- prairie.prep %>% 
  #gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  #unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  unite(Treatment_Block, treat_id, block, sep="_", remove= F)

head(prairie.prep.treats)


# full species list, for taxonomic diversity and phylo diversity
prairie.list <- prairie.prep.treats  %>%
  split(.$Treatment_Block)

prairie.matrix.list <- purrr::map(prairie.list, ~ .x %>% 
                                    select(species, samp_id, pres) %>%
                                    distinct() %>%
                                    spread(key = samp_id, value = pres) %>%
                                    replace(is.na(.), 0) %>%
                                    column_to_rownames(var = "species") )


View(prairie.matrix.list)

# ========================================================================================================== #
#  Taxonomic diversity


TD_treat_out <- iNEXT3D(data = prairie.matrix.list, diversity = 'TD', q = c(0,1,2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1:600), 
                  #endpoint = 160, knots =1,
                  nboot = 0)

TD_treat_out

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/")
save(TD_treat_out, file = "TD_block_treat_out.Rdata")

load(file = "TD_block_treat_out.Rdata")

#$iNextEst$size_based
prairie.TD.df <- TD_treat_out %>% 
  purrr::pluck("iNextEst", "size_based")

View(prairie.TD.df)

head(prairie.prep)

prairie_info <- prairie.prep.treats %>% select(Treatment_Block, treat_id, block, treat_id, Nutrients, Invasion, Assembly) %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment_Block))


prairie.hill.TD <- prairie.TD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )
  
head(prairie.hill.TD)
View(prairie.hill.TD)

write.csv(prairie.hill.TD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/prairie.hill.block.treats.TD.csv", row.names=FALSE)

# ========================================================================================================== #
# Phylogenetic diversity
setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")
phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

phylo.prep.treats <- phylo.prep %>% 
  # gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  # unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F)%>%
  unite(Treatment_Block, treat_id, block, sep="_", remove= F)

head(phylo.prep.treats)

# full species list, for taxonomic diversity and phylo diversity
phylo.list <- phylo.prep.treats %>%
  split(.$Treatment_Block)

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


PD_treat_out <- iNEXT3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1:600),
                  # OR  # endpoint = 20, knots = 1,
                  nboot = 0,  PDtree = tree, PDtype = "PD") 

PD_treat_out

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/")
save(PD_treat_out, file = "PD_block_treat_out.Rdata")

load(file = "PD_treat_out.Rdata")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )

head(prairie.hill.PD)


write.csv(prairie.hill.PD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/prairie.hill.block.treats.PD.csv", row.names=FALSE)

# ========================================================================================================== #
#  Functional diversity

# match traits and corrected species list from cover data

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/")
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



trait.prep.treats <- trait.prep %>% 
  # gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  # unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  unite(Treatment_Block, treat_id, block, sep="_", remove= F)


head(trait.prep.treats)

# full species list, for taxonomic diversity
trait.list <- trait.prep.treats %>%
  split(.$Treatment_Block)

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



FD_treat_out <- iNEXT3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  #size = c(1:188), 
                  endpoint = 600, #knots = 1,
                  nboot = 0,  FDdistM = distM, FDtype = 'tau_values', 
                  FDtau = NULL)

FD_treat_out
setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/")
save(FD_treat_out, file = "FD_block_treat_out.Rdata")


load(file = "FD_treat_out.Rdata")

#$AUCiNextEst$size_based
prairie.FD.df <- FD_treat_out %>% 
  purrr::pluck("FDiNextEst", "size_based")

View(prairie.FD.df)

prairie.hill.FD <- prairie.FD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.FD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Block Treat/prairie.hill.block.treats.FD.csv", row.names=FALSE)


