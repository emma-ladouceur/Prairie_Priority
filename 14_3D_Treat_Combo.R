
library(viridis)
library(ape)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(iNEXT.3D)
library(patchwork)

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Combo")

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

#prairie.info <- prairie.prep %>% select(treat_id)

#write.csv(prairie.prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)


# full species list, for taxonomic diversity and phylo diversity
prairie.list <- prairie.prep  %>%
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
                  size = c(1:600), 
                  #endpoint = 160, knots =1,
                  nboot = 0)

TD_treat_out

save(TD_treat_out, file = "TD_treat_out.Rdata")

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Combo")
load(file = "TD_treat_out.Rdata")

#$iNextEst$size_based
prairie.TD.df <- TD_treat_out %>% 
  purrr::pluck("iNextEst", "size_based")

View(prairie.TD.df)

head(prairie.prep)

prairie_info <- prairie.prep %>% select(treat_id, Nutrients, Invasion, Assembly) %>%
  distinct() %>% mutate(Assemblage = as.character(treat_id))


prairie.hill.TD <- prairie.TD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )
  
head(prairie.hill.TD)
View(prairie.hill.TD)

write.csv(prairie.hill.TD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Combo/prairie.hill.treats.TD.csv", row.names=FALSE)

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Combo")
prairie.hill.TD <- read.csv("prairie.hill.treats.TD.csv",  header= TRUE)


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


prairie.hill.TD <- prairie.hill.TD %>% filter(!Order.q == "q = 1") %>% 
  unite(Treatment , Nutrients, Invasion, Assembly, sep = " ", remove= F) 

head(prairie.hill.TD)
# 
# prairie.hill.TD.mean.nuts <- prairie.hill.TD %>% group_by(nt, Method, Order.q, Nutrients) %>%
#   summarise(mean.qD = mean(qD)) %>% mutate(Treatment = Nutrients) %>% select(-Nutrients)
# 
# prairie.hill.TD.mean.inv <- prairie.hill.TD %>% group_by(nt, Method, Order.q, Invasion) %>%
#   summarise(mean.qD = mean(qD)) %>% mutate(Treatment = Invasion) %>% select(-Invasion)
# 
# prairie.hill.TD.mean.ass <- prairie.hill.TD %>% group_by(nt, Method, Order.q, Assembly) %>%
#   summarise(mean.qD = mean(qD)) %>% mutate(Treatment = Assembly) %>% select(-Assembly)
# 
# prairie.hill.TD.mean <- prairie.hill.TD.mean.nuts %>% bind_rows(prairie.hill.TD.mean.inv) %>%
#   bind_rows(prairie.hill.TD.mean.ass)
# 
# head(prairie.hill.TD.mean)

df.point <- prairie.hill.TD[which(prairie.hill.TD$Method=="Observed"),]
df.line <- prairie.hill.TD[which(prairie.hill.TD$Method!="Observed"),]
df.line$Method <- factor(df.line$Method, 
                         c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.TD.fig <- ggplot(prairie.hill.TD, aes(x = nt, y = qD,   color = Assemblage)) +
  facet_wrap(~Order.q)+
  geom_point(aes(), shape = 1, size=2, data = df.point) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.line) +
  labs(x="Number of sampling units", y="Taxonomic Diversity",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='a) Taxonomic Diversity', color="Treatment", x= "")+
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


save(PD_treat_out, file = "PD_treat_out.Rdata")

load(file = "PD_treat_out.Rdata")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.PD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.hill.treats.PD.csv", row.names=FALSE)

prairie.hill.PD <- read.csv("prairie.hill.treats.PD.csv",  header= TRUE)

head(prairie.hill.PD)

prairie.hill.PD <- prairie.hill.PD %>% filter(!Order.q == "q = 1") %>% 
  unite(Treatment , Nutrients, Invasion, Assembly, sep = " ", remove= F)

df.point <- prairie.hill.PD[which(prairie.hill.PD$Method=="Observed"),]
df.line <- prairie.hill.PD[which(prairie.hill.PD$Method!="Observed"),]
df.line$Method <- factor(df.line$Method, 
                         c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.PD.fig <- ggplot(prairie.hill.PD, aes(x = nt, y = qPD,   color = Treatment)) +
  facet_wrap(~Order.q)+
  geom_point(aes(), shape = 1, size=2, data = df.point) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.line) +
  labs(x="Number of sampling units", y="Phylogenetic Diversity",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='b) Phylogenetic Diversity', color = "Treatment", x = "")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "right") +
  guides(col = guide_legend(ncol = 1))


prairie.PD.fig





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
  split(.$treat_id)

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



FD_treat_out <- iNEXT3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  #size = c(1:188), 
                  endpoint = 188, #knots = 1,
                  nboot = 0,  FDdistM = distM, FDtype = 'tau_values', 
                  FDtau = NULL)

FD_treat_out

save(FD_treat_out, file = "FD_treat_out.Rdata")


load(file = "FD_treat_out.Rdata")

#$AUCiNextEst$size_based
prairie.FD.df <- FD_treat_out %>% 
  purrr::pluck("FDiNextEst", "size_based")

View(prairie.FD.df)

prairie.hill.FD <- prairie.FD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.FD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.hill.treats.FD.csv", row.names=FALSE)


prairie.hill.FD <- read.csv("prairie.hill.treats.FD.csv",  header= TRUE)


head(prairie.hill.FD)

is.numeric(prairie.hill.FD$Assemblage)

prairie.hill.FD <- prairie.hill.FD %>% filter(!Order.q == "q = 1") %>% 
  unite(Treatment , Nutrients, Invasion, Assembly, sep = " ", remove= F)

df.point <- prairie.hill.FD[which(prairie.hill.FD$Method=="Observed"),]
df.line <- prairie.hill.FD[which(prairie.hill.FD$Method!="Observed"),]
df.line$Method <- factor(df.line$Method, 
                         c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.FD.fig <- ggplot(prairie.hill.FD, aes(x = nt, y = qFD,   color = Treatment)) +
  facet_wrap(~Order.q)+
  geom_point(aes(), shape = 1, size=2, data = df.point) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.line) +
  labs(x="Number of sampling units", y="Functional Diversity",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='c) Functional Trait Diversity', color="Treatment")+
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "none") +
  guides(col = guide_legend(ncol = 15))


prairie.FD.fig

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

prairie.hill.FD$Method <- factor(prairie.hill.FD$Method  , levels=c("Rarefaction", "Extrapolation"))

# line types / method
line.leg <- ggplot() +
  geom_line(data=prairie.hill.FD %>% filter(Method == c("Rarefaction", "Extrapolation")), aes(x=nt, y=qFD,  linetype = Method), lwd=1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="A")  + 
  scale_color_manual(values =  c("#5DC863FF"))  + 
  labs(title='Species accumulation across scales', color="Reference Habitat", linetype="Inference Method")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal", legend.position = "bottom") 

line.legend <- g_legend(line.leg)

# olf field colors
trt.leg <- ggplot() +
  geom_line(data = prairie.hill.FD, aes(x = nt, y = qFD,   color = Assemblage), lwd = 1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='Multi-scale',subtitle="Average Accumulation for Year Since Abandonment", color="Restoration Treatments")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 6))

trt.legend <- g_legend(trt.leg)


 prairie.div <- (prairie.TD.fig / prairie.PD.fig/ prairie.FD.fig  + plot_layout(heights = c(10,10,10))) 

 prairie.div
