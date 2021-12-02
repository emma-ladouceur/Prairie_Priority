
library(tidyverse)


# use try data

try_traits <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/TRY/17830_01122021165643/17830.txt", header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))

nrow(try_traits)
colnames(try_traits)

View(try_traits)



sp_trait_sum <- try_traits %>% select(AccSpeciesID, AccSpeciesName, TraitName) %>%
  filter(!is.na(TraitName)) %>% distinct() %>% arrange(AccSpeciesName, TraitName)


View(sp_trait_sum)

try_traits$ValueKindName <- as.factor(as.character(try_traits$ValueKindName))
levels(try_traits$ValueKindName)

sp_trait_means <- try_traits %>% select(AccSpeciesID, AccSpeciesName, TraitName, OriglName, OrigValueStr, OrigUnitStr,
                                        ValueKindName, StdValue, UnitName) %>%
  filter(!is.na(TraitName),
         ValueKindName %in% c("Best estimate" , "Mean", "Median", "Single", "Species mean", "Site specific mean")) %>% 
  mutate(OrigValueStr = as.numeric(OrigValueStr)) %>%
  group_by(AccSpeciesID, AccSpeciesName, TraitName, OriglName,  OrigUnitStr,
           ValueKindName, UnitName ) %>%
  summarise( mean_OrigValueStr = mean(OrigValueStr),
             mean_StdValue = mean(StdValue)) %>%
  arrange(AccSpeciesName, TraitName)


View(sp_trait_means)

