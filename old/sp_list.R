




library(tidyverse)
library(brms)
library(ggplot2)
library(stringr)
library(patchwork)
library(gridExtra)
library(grid)



cover <- read.csv("~/Dropbox/_Projects/Prairie_Priority/cover_long.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(cover)

setwd("~/Dropbox/_Projects/Prairie_Priority/Data")
corrected_sp_list <- read.csv("species_corrected_complete.csv", row.names = 1, header= TRUE)

head(corrected_sp_list)

trait <- read.csv("~/Dropbox/_Projects/Prairie_Priority/data/trait_matrix.csv") # Trait matrix with missing values

head(trait)



cover$species  <- gsub("[[:punct:]]", " ", cover$species ) 

head(cover)

sp <- sp %>% mutate(species = Species) %>% select(-Species) %>%
  mutate( Seeded = case_when( Seeded == "1" ~ "Seeded Forbs",
                              Seeded == "0" ~ "Not Seeded"),
          Exotic = case_when( Exotic == "1" ~"Exotic",
                              Exotic == "0" ~ "Native"),
          Invasive = case_when( Invasive == "1"~ "Invasive",
                                Invasive == "0" ~ "Not Invasive"),
          Graminoid = case_when( species == "graminoid" ~ "Graminoid"),
          
  ) %>% arrange(species)

head(sp)


cover <- cover %>% left_join(sp) %>%
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(Assemblage, Nutrients , Invasion , Assembly , remove = FALSE) %>%
  filter(Assembly == "Both first")


head(cover)

cover %>% select(Assemblage) %>% distinct()


cover_sum_dat <- cover %>% select(Assemblage, species, cover, Seeded, Exotic, Invasive) %>%
  group_by(Assemblage, species) %>% mutate(cover_sum = sum(cover)) %>% select(-cover) %>% arrange(Assemblage, cover_sum)

head(cover_sum_dat)


