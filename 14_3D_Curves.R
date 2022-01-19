
library(viridis)
library(ape)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)


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


#write.csv(prairie.prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)


# full species list, for taxonomic diversity and phylo diversity
prairie.list <- prairie.prep %>%
  split(.$treat_id)

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
                  size = c(1:188), 
                  #endpoint = 160, knots =1,
                  nboot = 0)

TD_treat_out

save(TD_treat_out, file = "TD_treat_out.Rdata")

load(file = "TD_treat_out")

#$iNextEst$size_based
prairie.TD.df <- TD_treat_out %>% 
  purrr::pluck("iNextEst", "size_based")

View(prairie.TD.df)

head(prairie.prep)

prairie_info <- prairie.prep %>% select(plot,  block, treat_id, Nutrients, Invasion, Assembly) %>%
  distinct() %>% mutate(Assemblage = as.character(treat_id))


prairie.hill.TD <- prairie.TD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )
  
head(prairie.hill.TD)
View(prairie.hill.TD)

write.csv(prairie.hill.TD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.hill.treats.TD.csv", row.names=FALSE)



prairie.TD.SAD <- ggplot(prairie.hill.TD, aes(x = nt, y = qD,  group = Assemblage, color = Assemblage)) +
  facet_wrap(~Order.q)+
  geom_line(aes(), lwd=1, data = prairie.hill.TD) +
  labs(x="Number of sampling units", y = "Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=10))# + 
#labs(subtitle='c) Old field')
#xlim(0,20)+ 

prairie.TD.SAD


df.point <- prairie.hill.TD[which(prairie.hill.TD$Method=="Observed"),]
df.line <- prairie.hill.TD[which(prairie.hill.TD$Method!="Observed"),]
df.line$Method <- factor(df.line$Method, 
                         c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.TD.fig <- ggplot(prairie.hill.TD, aes(x = nt, y = qD,   color = Assemblage)) +
  facet_wrap(~Order.q)+
  geom_point(aes(), shape = 1, size=2, data = df.point) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.line) +
  # geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
  #                 fill = site, colour=NULL), alpha=0.2) +
  # facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  #geom_line(aes(), lwd=1, data=ccr.all.df.avg) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Taxonomic Diversity",title="") +
  scale_color_viridis(discrete = T)  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  labs(title='a) Taxonomic Diversity', color="Treatment")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15))


prairie.TD.fig


# ========================================================================================================== #
# Phylogenetic diversity
phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

# full species list, for taxonomic diversity and phylo diversity
phylo.list <- phylo.prep %>%
  split(.$treat_id)

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
                  size = c(1:188),
                  # OR  # endpoint = 20, knots = 1,
                  nboot = 0,  PDtree = tree, PDtype = "PD") 

PD_treat_out


save(PD_out, file = "PD_treat_out.Rdata")

load(file = "PD_treat_out")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.PD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.hill.treats.PD.csv", row.names=FALSE)






# ========================================================================================================== #
#  Functional diversity

# match traits and corrected species list from cover data


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


# full species list, for taxonomic diversity
trait.list <- trait.prep %>%
  split(.$plot)

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


# FD_est <- estimate3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', base = 'size',
#                      #level = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
#                       nboot = 0,  FDdistM = distM)
# 
# View(FD_est)

FD_out <- iNEXT3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  # size = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                  endpoint = 20, #knots = 1,
                  nboot = 0,  FDdistM = distM)

FD_out

save(FD_out, file = "FD_out.Rdata")


load(file = "FD_out")

#$AUCiNextEst$size_based
prairie.FD.df <- FD_out %>% 
  purrr::pluck("AUCiNextEst", "size_based")

View(prairie.FD.df)

prairie.hill.FD <- prairie.FD.df %>% left_join(prairie_info)


write.csv(prairie.hill.FD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.hill.FD.csv", row.names=FALSE)

