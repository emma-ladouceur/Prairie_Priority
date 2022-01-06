

rm(list = ls())


# packages needed
library(tidyverse)
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library(devtools)
library(phytools)
library(V.PhyloMaker)
library(stdnames) 

# read in corrected sp list
new_sps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/species_corrected_complete.csv")

head(new_sps)

# Flag infraspecies
new_sps <- new_sps %>%
  separate(name, into = c("genus", "species", 
                          "type", "infra_name"
                          ),
           remove = FALSE)

head(new_sps, n=30)

# fill in taxonomy- this takes a minute
for(i in 1:nrow(new_sps)) {
  
  print(new_sps$name[i])

  # Get higher taxonomic information
  taxo <- std_names(data.frame(name = new_sps$name[i]), 
                    species_column = "name")$corrected_list
  
  # Compile taxonomy
  new_sps$speciesid[i]  <- 
    as.character(new_sps$speciesid[i])
  new_sps$group[i]      <- taxo$group
  new_sps$order[i]      <- taxo$order
  new_sps$family[i]     <- taxo$family
  new_sps$genus[i]      <- new_sps$genus[i]
  new_sps$species[i]    <- new_sps$species[i]
  new_sps$sub_type[i]   <- new_sps$type[i]
  new_sps$name[i]       <- new_sps$infra_name[i]
  
}

# have a look at output
head(new_sps, n= 30)
View(new_sps)
colnames(new_sps)

# clean the list, put into format for phylo maker
clean_sps <- new_sps %>% 
  select( genus, species, type, infra_name, family,  group, order,   morphotype) %>%
  mutate(type = ifelse(is.na(type), "", type),
         infra_name = ifelse(is.na(infra_name), "", infra_name)) %>%
  unite( "species", genus, species, type, infra_name, remove=FALSE) %>%
  mutate(species = str_replace(species, "__", ""),
         species = str_replace(species, "Capsella_bursa_pastoris_", "Capsella_bursa_pastoris")) %>%
  select(-c(type, infra_name))

head(clean_sps, n = 30)

View(clean_sps)

# how to make a phylo tree from existing plant list
# https://vimeo.com/470373338#

# make phylo tree
prairie.phy = phylo.maker(clean_sps)

prairie.tree = prairie.phy$scenario.3

plotTree(prairie.tree)

write.tree(prairie.tree, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/phylo.tree.txt")



