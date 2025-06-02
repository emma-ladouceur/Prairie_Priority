rm(list = ls())

require(tidyverse)
require(reshape2)
require(missForest)
require(adephylo)
require(phytools)
require(ape)
require(pez)


sessionInfo()

# Reads: trait imputation 
# https://www.sciencedirect.com/science/article/abs/pii/S1574954121001060
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12232

# import trait matrix with missing values
trait <- read.csv("~/Dropbox/_Projects/Prairie_Priority/data/trait_matrix.csv") # Trait matrix with missing values

head(trait)
# _______________________________________________________________

trait_0 <- trait   
trait_0$species <- trait_0$AccSpeciesName   

spp <- trait$AccSpeciesName  
spp <- stringr::str_replace_all(spp," ","_")
spp <- as.data.frame(spp)
spp$species <- spp$spp

trait_0 = cbind(trait_0, spp$species)
trait_0$AccSpeciesName = NULL
trait_0$species = NULL
trait_0$species = trait_0$`spp$species`
trait_0$`spp$species` = NULL

# Load the phylogeny / tree
prairie.tree <- read.tree('~/Dropbox/_Projects/Prairie_Priority/Data/phylo.tree.txt')


phyo_match <- tibble(species = prairie.tree$tip.label, in_tree = 1) %>% left_join(spp, . , by = "species") 

phyo_match

phyo_match <- na.omit(phyo_match)

phyo_match <- phyo_match[, 2]
phyo_match <- as.data.frame(phyo_match)
phyo_match$species <- phyo_match$phyo_match

phyo_match

setdiff( trait_0$species , phyo_match$species) 

trait_1 <- plyr::join(phyo_match, trait_0,  by = "species")   
trait_1$phyo_match = NULL

trait_1 <- tibble::remove_rownames(trait_1)
trait_1 <- tibble::column_to_rownames(trait_1, var="species") 


prox.Ab.all <- adephylo::proxTips(prairie.tree, method = "Abouheif", normalize="none") 
dim(prox.Ab.all) # 104

prox <- prop.table(prox.Ab.all, 1) #standardize by row
prox <- 0.5 * (prox + t(prox)) #make matrix symetric

ME <- adephylo::me.phylo(prox = prox) 
# dim(ME)
# head(ME)

ME <- ME[rownames(trait_1),]

trait.imp <- cbind(trait_1, ME[,1:30]) 

# _______________________________________________________________ missForest
colnames(trait)

dfk <- data.frame(matrix(NA, nrow = 30, ncol = 11)) # 10 traits + k = 11
colnames(dfk) <- c("k", "OOB_LA","OOB_LDMC","OOB_LN","OOB_PH", "OOB_SDM", "OOB_SGR", "OOB_SLA", "OOB_SN", "OOB_SSBL", "OOB_SSD") # 10 traits + k = 11

for (n in 1:30) {
  dfimp <- trait.imp[, 1: (10+n)] 
  o <- missForest(dfimp, maxiter = 25, ntree = 100 , variablewise = TRUE) 
  dfk[n, 1] <- n
  dfk[n,2] <- o$OOBerror[1] # save OOBerror for target traits only. OOB, is out-of-bag imputation error estimate/OBB is a method of measuring the prediction error of random forests
  dfk[n,3] <- o$OOBerror[2]    
  dfk[n,4] <- o$OOBerror[3]
  dfk[n,5] <- o$OOBerror[4]
  dfk[n,6] <- o$OOBerror[5]
  dfk[n,7] <- o$OOBerror[6]
  dfk[n,8] <- o$OOBerror[7]
  dfk[n,9] <- o$OOBerror[8]
  dfk[n,10] <- o$OOBerror[9]
  dfk[n,11] <- o$OOBerror[10]
}

colnames(dfk)

dfk2 <- dfk %>%   # imputation errors per trait
  summarize(min_LA = min(OOB_LA), k_min_LA = k[which.min(OOB_LA)], 
            min_LDMC  = min(OOB_LDMC),  k_min_LDMC  = k[which.min(OOB_LDMC)],     
            min_LN   = min(OOB_LN),   k_min_LN   = k[which.min(OOB_LN)],
            min_PH   = min(OOB_PH),   k_min_PH   = k[which.min(OOB_PH)],
            min_SDM = min(OOB_SDM), k_min_SDM = k[which.min(OOB_SDM)], 
            min_SGR  = min(OOB_SGR),  k_min_SGR  = k[which.min(OOB_SGR)],     
            min_SLA   = min(OOB_SLA),   k_min_SLA   = k[which.min(OOB_SLA)],
            min_SN   = min(OOB_SN),   k_min_SN   = k[which.min(OOB_SN)],
            min_SSBL   = min(OOB_SSBL),   k_min_SSBL   = k[which.min(OOB_SSBL)],
            min_SSD   = min(OOB_SSD),   k_min_SSD   = k[which.min(OOB_SSD)],
            )


head(trait.imp)
head(dfk2)

LA_ideal <-missForest(trait.imp[, 1: (10+dfk2$k_min_LA)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
head(LA_ideal)
LDMC_ideal  <-missForest(trait.imp[, 1: (10+dfk2$k_min_LDMC )] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
LN_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_LN)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
PH_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_PH)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
SDM_ideal <-missForest(trait.imp[, 1: (10+dfk2$k_min_SDM)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE)  
SGR_ideal  <-missForest(trait.imp[, 1: (10+dfk2$k_min_SGR)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
SLA_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_SLA)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
SN_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_SN)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
SSBL_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_SSBL)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
SSD_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_SSD)] , maxiter = 25, ntree = 100 ,   variablewise = TRUE) 


best_LA  <-tibble(LA = LA_ideal$ximp$LA,species=rownames(LA_ideal$ximp))
best_LDMC   <-tibble(LDMC  = LDMC_ideal$ximp$LDMC,  species=rownames (LDMC_ideal$ximp))
best_LN    <-tibble(LN   = LN_ideal$ximp$LN,    species=rownames (LN_ideal$ximp))
best_PH    <-tibble(PH   = PH_ideal$ximp$PH,    species=rownames (PH_ideal$ximp))
best_SDM  <-tibble(SDM = SDM_ideal$ximp$SDM, species=rownames(SDM_ideal$ximp))
best_SGR   <-tibble(SGR  = SGR_ideal$ximp$SGR,  species=rownames (SGR_ideal$ximp))
best_SLA    <-tibble(SLA   = SLA_ideal$ximp$SLA,    species=rownames (SLA_ideal$ximp))
best_SN    <-tibble(SN   = SN_ideal$ximp$SN,    species=rownames (SN_ideal$ximp))
best_SSBL    <-tibble(SSBL   = SSBL_ideal$ximp$SSBL,    species=rownames (SSBL_ideal$ximp))
best_SSD    <-tibble(SSD   = SSD_ideal$ximp$SSD,    species=rownames (SSD_ideal$ximp))


Trait_imputed <- best_LA %>% left_join( best_LDMC, by="species")%>%   # traits imputed using phylogeny and OOBerror
  left_join(best_LN, by="species")%>%
  left_join(best_PH,  by="species")%>%
  left_join(best_SDM,  by="species")%>%
  left_join(best_SGR,  by="species")%>%
  left_join(best_SLA,  by="species")%>%
  left_join(best_SN,  by="species")%>%
  left_join(best_SSBL,  by="species")%>%
  left_join(best_SSD,  by="species")%>%
  select(., species, LA, LDMC, LN, PH, SDM, SGR, SLA, SN, SSBL, SSD )

View(Trait_imputed)

write.csv(Trait_imputed, "~/Dropbox/_Projects/Prairie_Priority/Data/imputed_trait_matrix.csv",  row.names=FALSE)
