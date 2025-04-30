
library(tidyverse)
library(stringr)
library(missMDA)
library(ggfortify)
library(iNEXT.3D)
library(readr)



# use try data

all_traits <- read.csv("~/Dropbox/_Projects/Prairie_Priority/TRY/17830_01122021165643/17830.txt", header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))
lc_traits <- read.csv("~/Dropbox/_Projects/Prairie_Priority/TRY/26968_12052023095110/26968.txt", header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))

View(lc_traits)
nrow(try_traits)
colnames(try_traits)


try_traits <- all_traits %>% bind_rows(lc_traits %>% 
                                           mutate(OrigUncertaintyStr = as.character(OrigUncertaintyStr))) %>%
  

sp_trait_sum <- try_traits %>% bind_rows(lc_traits %>% 
                                           mutate(OrigUncertaintyStr = as.character(OrigUncertaintyStr))
                                           ) %>% 
  select(AccSpeciesID, AccSpeciesName, TraitName, DataName) %>%
  filter(!is.na(TraitName)) %>% distinct() %>% arrange(AccSpeciesName, TraitName, DataName)

head(try_traits)

try_traits$ValueKindName <- as.factor(as.character(try_traits$ValueKindName))
levels(try_traits$ValueKindName)

View(try_traits %>% select(ObservationID, AccSpeciesName, TraitName, ValueKindName) %>% filter(!is.na(TraitName)) %>% distinct())

View(try_traits %>% select(TraitName, ValueKindName) %>% filter(!is.na(TraitName)) %>% distinct() %>% arrange(TraitName, ValueKindName))

try_traits %>% select(DatasetID, TraitName, ValueKindName) %>% filter( TraitName ==  "Seed number per reproducton unit") %>% distinct() 


sp_trait_prep <- try_traits %>% select(AccSpeciesID, AccSpeciesName, TraitName, DataName, OriglName, OrigValueStr, OrigUnitStr,
                                        ValueKindName, StdValue, UnitName) %>%
  filter(!is.na(TraitName),
         ValueKindName %in% c( "Single", "Best estimate"
                               #, "Mean", "Class mean", "Species mean", "Site specific mean"
                               )) %>% 
  arrange(AccSpeciesName, TraitName, DataName) %>% ungroup()




head(sp_trait_prep)
nrow(sp_trait_prep)

trait_sum <- try_traits %>% select(TraitName, #DataName, # OriglName
                                   ) %>%
  filter(!is.na(TraitName)) %>% distinct() %>% arrange(TraitName, #DataName
                                                       #OriglName
                                                       )

head(trait_sum, n = 20)

View(trait_sum)

# group and standardize trait names
std_traits <- sp_trait_prep %>% 
  mutate( Std_Name = case_when(# LA = leaf area
                               TraitName %in% c("Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)" ,
                                                "Leaf area (in case of compound leaves: leaf, petiole excluded)",
                                                "Leaf area (in case of compound leaves: leaf, petiole included)" ,
                                                "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)",
                                                "Leaf area (in case of compound leaves: leaflet, petiole excluded)" ,
                                                "Leaf area (in case of compound leaves: leaflet, petiole included)" ,
                                                "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)" ) ~ "LA",
                               # SLA = specific leaf area
                               TraitName %in% c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" ,
                                               "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included" ,
                                               "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded") ~ "SLA",
                               # LDMC = leaf dry matter content
                               TraitName ==  "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" ~ "LDMC",
                               # LN = leaf nitrogen
                               TraitName ==  "Leaf nitrogen (N) content per leaf dry mass" ~ "LN",
                               # PH = vegetative plant height
                               TraitName ==  "Plant height vegetative" ~ "PH",
                               # PVR = plant vegetative reproduction
                               TraitName ==  "Plant vegetative reproduction: lateral spread" ~ "PVR",
                               # SSBL = soil seedbank longevity
                               TraitName ==  "Seed (seedbank) longevity" ~ "SSBL",
                               # SDM = seed dry mass
                               TraitName ==  "Seed dry mass" ~ "SDM",
                               # SGR = seed germination rate
                               TraitName ==  "Seed germination rate (germination efficiency)" ~ "SGR",
                               # SN = seed number per reproduction unit
                               TraitName ==  "Seed number per reproducton unit" ~ "SN",
                               # SSD = stem specific density
                               TraitName ==  "Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)" ~ "SSD"),
)


std_traits %>% select(AccSpeciesName) %>% distinct()

View(std_traits)
head(std_traits)
colnames(std_traits)

# check names converted properly
std_traits %>% select(TraitName, Std_Name, UnitName,) %>% distinct() %>% arrange(Std_Name, UnitName)
# how many traits total
std_traits %>% select(Std_Name) %>% distinct() %>% arrange() # 11
# check weird cases- eg soil seedbank longevity have two measures
ssbl <- std_traits %>% filter(Std_Name == "SSBL") %>% select(TraitName, UnitName) %>% distinct()
head(ssbl)
#PVR- no available pvr data
pvr <- std_traits %>% filter(Std_Name == "PVR") 
View(pvr)
# so in reality we have 10 traits

#check for lespedeza?
std_traits %>% select(AccSpeciesID, AccSpeciesName) %>% distinct()

# the plant list converted lespedeza cuneata to Lespedeza juncea var. sericea
# we have traits for both, and we will combine both into one name and use Lespedeza cuneata
std_traits %>% filter(AccSpeciesID == "33012") # Lespedeza cuneata
std_traits %>% filter(AccSpeciesID == "237517") # Lespedeza juncea var. sericea

sp_trait_means <- std_traits %>%
  mutate( AccSpeciesName = case_when(
    AccSpeciesName == "Lespedeza juncea var. sericea" ~ "Lespedeza cuneata",
    TRUE ~ AccSpeciesName ) ) %>%
mutate(OrigValueStr = as.numeric(OrigValueStr)) %>%
  filter(!is.na(StdValue) ) %>%
  group_by(AccSpeciesName, Std_Name,
           UnitName ) %>%
  summarise( #mean_OrigValueStr = mean(OrigValueStr),
             mean_StdValue = round( mean(StdValue), 2) ) %>%
  # filter out soil seedbank %
  filter(!Std_Name == 'SSBL' | UnitName == 'year') %>% 
  # and unit that doesnt fit for LDMC
  filter(!Std_Name == 'LDMC' | UnitName == 'g g-1') %>% 
  arrange(AccSpeciesName, Std_Name) %>% ungroup()
        

View(sp_trait_means)
colnames(sp_trait_means)

# check work
sp_trait_means %>% filter(AccSpeciesName == "Lespedeza cuneata")

sp_trait_means %>% select(Std_Name, UnitName) %>% distinct() %>% arrange(Std_Name)

sp_trait_means %>% group_by(Std_Name, UnitName) %>% summarise(count = n ())
  
# how many traits per species ?
 trait_abund <- sp_trait_means %>% select(AccSpeciesName, Std_Name) %>%
  distinct() %>% group_by(AccSpeciesName) %>% 
  summarise(n = n())
  
View(trait_abund)

trait_abund %>% summarise(mean(n))
# about 6.48 traits per sp

write.csv(sp_trait_means, "~/Dropbox/_Projects/Prairie_Priority/Data/trait_means.csv")

# produce species trait matrix
colnames(sp_trait_means)

trait_wide <- sp_trait_means %>% select(-c( UnitName)) %>%
  spread(Std_Name, mean_StdValue) %>% arrange(AccSpeciesName)

colnames(trait_wide)
View(trait_wide)

write.csv(trait_wide, "~/Dropbox//_Projects/Prairie_Priority/Data/trait_matrix.csv")



