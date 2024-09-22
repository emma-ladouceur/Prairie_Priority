

# Traits and Phylo!!!
library(tidyverse)
library(stdnames)

# alpha, gamma, beta
# functional diversity and phylogenetic diversity using chao
# div  q = 0 and evenness q = 2 with hill numbers

sampled_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/sampled_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
pres <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

p_sp <- pres %>% group_by(plot) %>%
  gather(species, pres, graminoid:Lindera.benzoin) %>%
  filter(!pres %in% c("0")) %>% # remove 0 values
  ungroup() %>% 
  select(species) %>%
  distinct() %>% arrange()

head(p_sp)

head(sampled_dat)

sp <- sampled_dat %>% select(species) %>%
 bind_rows(p_sp) %>% arrange(species) %>%  distinct(species) 


head(sp)
nrow(sp)

View(sp)

sp_sep <- sp %>%  mutate(species = str_replace(species, "bursa.pastoris", "bursa-pastoris" ),
                         species = str_replace(species, "graminoid", "graminoid.sp." ),) %>% 
  separate(species, c("sp", "gen"), extra = "merge") %>%
  unite(species, "sp", "gen", sep=" ")

View(sp_sep)

morphs <- fix_morphotype(sp_sep$species, pattern = c("sp.","spp.", "spec."), label = "sp.")

is_morphs <- morphs %>%
  filter(morphotype == "yes") %>%
  droplevels()  %>% 
  mutate(old_name = name,
         name = corrected_name) %>%
  select(old_name, name, morphotype)
  
head(is_morphs)

# subset non- morphotypes
not_morphs <-  morphs %>%
  filter(morphotype == "no") %>%
  droplevels() 

head(not_morphs)

sp_out <- std_names(x = not_morphs, 
                 species_column = "corrected_name")


sp_fixed <- sp_out$corrected_list %>%
  arrange(original_name) %>% 
  distinct() %>%
  mutate(old_name = original_name,
         name = tpl_name) %>%
   select( old_name, name) %>% arrange(name) %>%
  mutate(morphotype = "no") %>%
  bind_rows(is_morphs) %>% 
  mutate_all(na_if,"") %>%
  mutate(name =
           ifelse(!is.na(name),
                  name,
                  old_name)) %>%
  arrange(name) 


View(sp_fixed)

write.csv(sp_fixed, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/species_corrected_complete.csv")

sp_fixed <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/species_corrected_complete.csv",header=T,fill=TRUE,na.strings=c(""," ","NA","NA ","na","NULL"))


species <- sp_fixed %>% filter(morphotype == "no") %>%
  select(name) %>% distinct(name) %>% arrange(name)


nrow(species)
head(species)

# write species list submitted to TRY
write.csv(species, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/species_TRY.csv")

species <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/species_TRY.csv",header=T,fill=TRUE,na.strings=c(""," ","NA","NA ","na","NULL"))

nrow(species)
head(species)

#full try species list
try_sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/TRY/TryAccSpecies.txt",header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))

head(try_sp)
nrow(try_sp)

try_sp_list <- try_sp %>% select(AccSpeciesName) %>%
  distinct()

nrow(try_sp_list)

View(try_sp_list)

sp <- try_sp %>% 
  mutate(name = AccSpeciesName,
         id = AccSpeciesID) %>% select(name, id)

 sp_match <- species %>% left_join(sp)
 
 sp_match %>% filter(is.na(id))

sp_fix <- sp_match %>%  mutate(name = str_replace(name, "Capsella bursa-pastoris", "CAPSELLA BURSA-PASTORIS" )) %>% 
  select(-id) %>%
  left_join(sp)
 
 View(sp_fix)


for_try <- sp_match %>% spread(name,id) %>% slice(1)

head(for_try)

write.csv(for_try, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/TRY/species.txt")


# data obtained
try_traits <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/TRY/17830_01122021165643/17830.txt", header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))

colnames(try_traits)

try_sp <- try_traits %>% select(AccSpeciesID, AccSpeciesName) %>%
  distinct() %>% arrange(AccSpeciesName)


nrow(try_sp)

View(try_sp)

#122 species

# do I need to correct the TRY names?

sp_out <- std_names(x = try_sp, 
                    species_column = "AccSpeciesName")


sp_fixed <- sp_out$corrected_list %>%
  arrange(original_name) %>% 
  distinct() %>%
  mutate(old_name = original_name,
         name = tpl_name) %>%
  select( old_name, name) %>% arrange(name)


View(sp_fixed)

# nope
