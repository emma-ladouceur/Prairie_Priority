
library(ape)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)


setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")

# cover species data
sp <- read.csv("sampled_dat.csv", row.names = 1, header= TRUE)

# wrangle species data to be presence matrix
head(sp)

 sp_matrix_prep <- sp %>% 
   #unite("samp_id", subplot, plot, block, treatment)  %>%
   mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                 nutrients == "1" ~ "Nutrients"),
           Invasion = case_when(invasion == "e" ~ "Early",
                                invasion == "l" ~ "Late"),
           Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                 Grass.forbs == "f" ~ "Forbs first",
                                 Grass.forbs == "b" ~ "Both first")
   )   %>% unite("treat_id" , Nutrients, Invasion, Assembly, remove= FALSE) %>%
   unite("samp_id", treat_id, subplot, plot, block, remove = FALSE) %>%
   mutate(species = str_replace_all(species, 
                                 pattern = "\\.", replacement = "_"))

 head(sp_matrix_prep)
 
 # species sample matrix with relative cover
 sp_matrix <- sp_matrix_prep %>% 
   select(species, relative_cover, samp_id ) %>%
   spread(samp_id, relative_cover) %>%
   mutate_all(funs(replace_na(.,0))) #%>% 
  # filter(!species == "graminoid")

View(sp_matrix)

sp_list <- sp_matrix %>% select(species)

traits <- read.csv("imputed_trait_matrix.csv",  header= TRUE)

sp_traits <- traits %>% select(species)


sp_match <- sp_list %>% full_join(sp_traits)

View(sp_match)


 write.csv(sp_matrix, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/rel_sp_matrix.csv", row.names=FALSE)
 
 
 # change all non-0's to 1 for an incidence based matrix
 

 inc_sp_matrix <- sp_matrix %>% mutate_if(is.numeric, ~1 * (. != 0))


 View(inc_sp_matrix) 
 
 write.csv(inc_sp_matrix, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/inc_sp_matrix.csv",  row.names=FALSE)
 
 
 # number of samps per treat
 head(sp)
 
treats <- sp %>% 
   unite("samp_id", subplot, plot, block, treatment)  %>%
   mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                 nutrients == "1" ~ "Nutrients"),
           Invasion = case_when(invasion == "e" ~ "Early",
                                invasion == "l" ~ "Late"),
           Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                 Grass.forbs == "f" ~ "Forbs first",
                                 Grass.forbs == "b" ~ "Both first")
   )   %>% unite("treat_id" , Nutrients, Invasion, Assembly, remove= FALSE)
 
head(treats)

treat_samps <- treats %>% select(samp_id, treat_id) %>%
  distinct() %>% group_by(treat_id) %>%
  summarise(n_samps = n_distinct(samp_id)) %>%
  spread(treat_id, n_samps)
 
View(treat_samps)

write.csv(treat_samps, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/treat_samps.csv")

 
 # OK now we are ready
 # species matrix
 Inci_raw <- read.csv("inc_sp_matrix.csv",  row.names = 1, header= TRUE)
 # nt for incidence samps
 nT <- read.csv('treat_samps.csv', row.names = 1)
 # phylo tree
 tree <- read.tree("phylo.tree.txt")
 #imputed trait matrix
 traits <- read.csv("imputed_trait_matrix.csv", row.names = 1, header= TRUE)
 
 head(nT)
 View(traits)

 Cmax <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
   apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', 2*x[1])) %>% min %>% round(., 4)
 
 Cmin <- sapply(1:length(nT), function(i) rowSums( Inci_raw[, (sum(nT[1:i]) - sum(nT[i]) + 1) : sum(nT[1:i])] )) %>% rbind(as.integer(nT),.) %>% 
   apply(., 2, function(x) iNEXT.3D:::Coverage(x, 'incidence_freq', x[1])) %>% min %>% round(., 4)

Cmax
Cmin 
  
# ========================================================================================================== #
# Figure 3 - Taxonomic diversity

TD_est <- estimate3D(data = Inci_raw, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'coverage',
                     level = c(Cmin, Cmax), nboot = 0, nT = nT)
TD_obs <- obs3D(data = Inci_raw, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', nboot = 0, nT = nT)
TD_asy <- asy3D(data = Inci_raw, diversity = 'TD', q = c(0, 1, 2), datatype = 'incidence_raw', nboot = 0, nT = nT)


out_TD <- rbind(TD_est %>% select(Assemblage, Order.q, qD, goalSC), 
                TD_obs %>% select(Assemblage, Order.q, qD) %>% mutate(goalSC = 'Observed'), 
                TD_asy %>% select(Assemblage, Order.q, qD) %>% mutate(goalSC = 'Asymptotic'))
fig_1_or_3(out_TD, y_label = 'Taxonomic diversity')

View(out_TD)
View(TD_obs)


# ========================================================================================================== #
# Figure 3 - Phylogenetic diversity

PD_est <- estimate3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'coverage',
                     level = c(Cmin, Cmax), nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)
PD_obs <- obs3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)
PD_asy <- asy3D(data = Inci_raw, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, PDtree = tree, PDreftime = 1)

out_PD <- rbind(PD_est %>% select(Assemblage, Order.q, qPD, goalSC), 
                PD_obs %>% select(Assemblage, Order.q, qPD ) %>% mutate(goalSC = 'Observed'), 
                PD_asy %>% select(Assemblage, Order.q, qPD) %>% mutate(goalSC = 'Asymptotic'))



# ========================================================================================================== #
# Figure 3 - Functional diversity

for (i in 1:ncol(traits)) {
  if (class(traits[,i]) == "character") traits[, i] <- factor(traits[,i], levels = unique(traits[, i]))
}
distM <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()


FD_est <- estimate3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'coverage',
                     level = c(Cmin, Cmax), nboot = 0, nT = nT, FDdistM = distM)
FD_obs <- obs3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', 
                nboot = 0, nT = nT, FDdistM = distM)
FD_asy <- asy3D(data = Inci_raw, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw',
                nboot = 0, nT = nT, FDdistM = distM)

out_FD <- rbind(FD_est %>% select(Assemblage, Order.q, qFD = qAUC, goalSC), 
                FD_obs %>% select(Assemblage, Order.q, qFD = qAUC) %>% mutate(goalSC = 'Observed'), 
                FD_asy %>% select(Assemblage, Order.q, qFD = qAUC) %>% mutate(goalSC = 'Asymptotic'))




 
 