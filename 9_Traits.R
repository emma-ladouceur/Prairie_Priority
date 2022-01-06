
library(tidyverse)
library(stringr)
library(missMDA)
library(ggfortify)
library(iNEXT.3D)



# use try data

try_traits <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/TRY/17830_01122021165643/17830.txt", header=T,fill=TRUE,sep="\t",na.strings=c(""," ","NA","NA ","na","NULL"))

nrow(try_traits)
colnames(try_traits)

View(try_traits)



sp_trait_sum <- try_traits %>% select(AccSpeciesID, AccSpeciesName, TraitName, DataName) %>%
  filter(!is.na(TraitName)) %>% distinct() %>% arrange(AccSpeciesName, TraitName, DataName)


View(sp_trait_sum)

try_traits$ValueKindName <- as.factor(as.character(try_traits$ValueKindName))
levels(try_traits$ValueKindName)

sp_trait_prep <- try_traits %>% select(AccSpeciesID, AccSpeciesName, TraitName, DataName, OriglName, OrigValueStr, OrigUnitStr,
                                        ValueKindName, StdValue, UnitName) %>%
  filter(!is.na(TraitName),
         ValueKindName %in% c("Best estimate" , "Mean", "Median", "Single", "Species mean", "Site specific mean")) %>% 
  arrange(AccSpeciesName, TraitName, DataName) %>% ungroup()


head(sp_trait_prep)


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


View(std_traits)
head(std_traits)
colnames(std_traits)

# check names converted properly
std_traits %>% select(TraitName, Std_Name) %>% distinct() %>% arrange(Std_Name)
# how many traits total
std_traits %>% select(Std_Name) %>% distinct() %>% arrange() # 11
# check weird cases- eg soil seedbank longevity have two measures
ssbl <- std_traits %>% filter(Std_Name == "SSBL") 
head(ssbl)
#PVR- no available pvr data
pvr <- std_traits %>% filter(Std_Name == "PVR") 
View(pvr)
# so in reality we have 10 traits


sp_trait_means <- std_traits %>%
mutate(OrigValueStr = as.numeric(OrigValueStr)) %>%
  filter(!is.na(StdValue) ) %>%
  group_by(AccSpeciesID, AccSpeciesName, Std_Name,
           UnitName ) %>%
  summarise( mean_OrigValueStr = mean(OrigValueStr),
             mean_StdValue = mean(StdValue)) %>%
  # filter out soil seedbank %
  filter(!Std_Name == 'SSBL' | UnitName == 'year')
        

View(sp_trait_means)
colnames(sp_trait_means)

# how many traits per species ?
 trait_abund <- sp_trait_means %>% select(AccSpeciesName, Std_Name) %>%
  distinct() %>% group_by(AccSpeciesName) %>% 
  summarise(n = n())
  
View(trait_abund)

trait_abund %>% summarise(mean(n))
# about 6.48 traits per sp

write.csv(sp_trait_means, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/trait_means.csv")

# produce species trait matrix
colnames(sp_trait_means)

trait_wide <- sp_trait_means %>% select(-c(mean_OrigValueStr, UnitName)) %>%
  spread(Std_Name, mean_StdValue)

View(trait_wide)

write.csv(trait_wide, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/trait_matrix.csv")


# method 1: impute traits with a pca
nb_dim  <- missMDA::estim_ncpPCA(trait_wide[, 3:12], ncp.min=3, ncp.max=5, 
                                   scale= T, method.cv="loo", nbsim=9, verbose=FALSE) #E stimate the number of dimensions for the Principal Component Analysis by cross-validation

trait_imp <- missMDA::imputePCA(  # Impute the missing values of the dataset with the Principal Components Analysis model. 
  trait_wide[, 3:12], ncp=nb_dim$ncp) 

trait_imp   <- trait_imp$completeObs
trait_imp   <- as.data.frame(trait_imp)
trait_imp = cbind(trait_imp, trait_wide$AccSpeciesName)
row.names(trait_imp) = trait_imp$`trait_wide$AccSpeciesName`
trait_imp$`trait_wide$AccSpeciesName` = NULL

View(trait_imp)

save(trait_imp, file = "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/pca_imputed_traits.Rdata")


pca_res <- prcomp(trait_imp, scale. = TRUE)
autoplot(pca_res, 
         loadings = TRUE, loadings.label = TRUE,loadings.label.size = 3,
         label = TRUE, label.size = 3
)



# method 2: see 11_Impute

