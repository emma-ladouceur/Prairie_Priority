
library(tidyverse)
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library(devtools)
library(phytools)
library(V.PhyloMaker)

phylo_prep <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/data/phylo_prep.csv") # Trait matrix with missing values

clean_sps <- phylo_prep %>% select(species, genus, family) %>% distinct() %>% arrange(species) %>%
  mutate(species = str_replace(species, "Lespedeza_juncea_var_sericea", "Lespedeza_juncea")) %>%
  filter(!grepl("_spp",species))

clean_sps
# how to make a phylo tree from existing plant list
# https://vimeo.com/470373338#


# make phylo tree
prairie.phy = phylo.maker(clean_sps)

prairie.tree = prairie.phy$scenario.3

prairie.tree


plotTree(prairie.tree)

write.tree(prairie.tree, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/phylo.tree.txt")

rm(list = ls())


library(tidyverse)
library(viridis)
library(ape)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(patchwork)


# check for updates ?
# install.packages("remotes")
# library(remotes)
# install.packages("rlang")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")
library(iNEXT.3D)
# cite 
citation("iNEXT.3D")


# Phylogenetic diversity
setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")

phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

phylo.prep.treats <- phylo.prep %>% 
  gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  filter(!grepl("_spp",species))

head(phylo.prep.treats)



# full species list, for  phylo diversity
phylo.list <- phylo.prep.treats %>%
  split(.$Treatment)

phylo.list

phylo.matrix.list <- purrr::map(phylo.list, ~ .x %>% 
                                  select(species, samp_id, pres) %>%
                                  distinct() %>%
                                  spread(key = samp_id, value = pres) %>%
                                  replace(is.na(.), 0) %>%
                                  column_to_rownames(var = "species") )


View(phylo.matrix.list)

tree <- read.tree("phylo.tree.txt")

# need sp names to have underscores instead of space because phylo package does this
tree
head(tree)


PD_treat_out <- iNEXT3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                        size = c(1:600),
                        # OR  # endpoint = 20, knots = 1,
                        nboot = 0,  PDtree = tree, PDtype = "PD") 

PD_treat_out

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
save(PD_treat_out, file = "PD_treat_sep_out.Rdata")


load(file = "PD_treat_out.Rdata")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


