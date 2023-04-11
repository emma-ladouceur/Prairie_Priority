
library(tidyverse)
library(viridis)
library(ape)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(patchwork)

# check for updates ?
# install.packages("remotes")
# library(remotes)
# install.packages("rlang")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")
library(iNEXT.3D)
# cite 
citation("iNEXT.3D")

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
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
head(sp)
summary(sp)

sp %>% select(subplot) %>% distinct()

prairie.prep <- sp %>% 
  # filter(!subplot == 10 ) %>%
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
   mutate(orig_species = species,
          subplot = as.character(subplot),
          plot = as.character(plot),
          block = as.character(block),
          ) %>% select(-species) %>%
   #mutate_all(na_if,"") %>%
  mutate(across( where(is.character), ~ na_if(.x, "0") ),
        # across( where(is.numeric), ~ na_if(.x, 0) ),
         ) %>%
  mutate(species =
           ifelse(!is.na(corrected_sp),
                  corrected_sp,
                  orig_species)) %>%
  unite("treat_id" , nutrients, invasion, Grass.forbs, remove= FALSE) %>%
  unite("samp_id",  plot, subplot, block, sep="_", remove = FALSE) %>%
  # remove graminoid
  filter(!species == "graminoid") %>%
    mutate(species = str_replace(species, "Lespedeza juncea var. sericea", "Lespedeza juncea"))

head(prairie.prep)
nrow(prairie.prep)

prairie.prep %>% select(subplot) %>% distinct()

levels(prairie.prep$species)

#prairie.info <- prairie.prep %>% select(treat_id)

#write.csv(prairie.prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)

prairie.prep.treats <- prairie.prep %>% 
  gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F)

head(prairie.prep.treats)


prairie_info <- prairie.prep.treats %>% select(Treatment, Treatment_cat, Treatment_type) %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment))


write.csv(prairie.prep.treats, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/prairie.prep.treats.csv", row.names=FALSE)

# full species list, for taxonomic diversity and phylo diversity
prairie.list <- prairie.prep.treats  %>%
  split(.$Treatment)

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

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
save(TD_treat_out, file = "TD_treat_sep_out.Rdata")

load(file = "TD_treat_sep_out.Rdata")

#$iNextEst$size_based
prairie.TD.df <- TD_treat_out %>% 
  purrr::pluck("iNextEst", "size_based")

View(prairie.TD.df)

head(prairie.prep)


prairie.hill.TD <- prairie.TD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )
  
head(prairie.hill.TD)
View(prairie.hill.TD)

write.csv(prairie.hill.TD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.TD.csv", row.names=FALSE)

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
prairie.hill.TD <- read.csv("prairie.hill.treats.sep.TD.csv",  header= TRUE)

head(prairie.hill.TD)

prairie.hill.TD %>% filter(Method == "Observed")

prairie.hill.TD$Treatment_cat <- factor(prairie.hill.TD$Treatment_cat, levels = c("Nutrients",  "Assembly", "Invasion"))

prairie.hill.TD$Treatment_type<- as.factor(prairie.hill.TD$Treatment_type)
levels(prairie.hill.TD$Treatment_type)
prairie.hill.TD$Treatment_type <- factor(prairie.hill.TD$Treatment_type, 
                                         levels = c("Control", "Nutrients", 
                                                    "Both first", "Forbs first", "Grass first",
                                                    "Early", "Late"))

prairie.hill.TD0 <- prairie.hill.TD %>% filter(Order.q == "q = 0") 
prairie.hill.TD2 <- prairie.hill.TD %>% filter(Order.q == "q = 2") 

View(prairie.hill.TD0)

df.pointTD0 <- prairie.hill.TD0[which(prairie.hill.TD0$Method=="Observed"),]
df.lineTD0 <- prairie.hill.TD0[which(prairie.hill.TD0$Method!="Observed"),]
df.lineTD0$Method <- factor(df.lineTD0$Method, 
                         c("Rarefaction", "Extrapolation"))

df.pointTD2 <- prairie.hill.TD2[which(prairie.hill.TD2$Method=="Observed"),]
df.lineTD2 <- prairie.hill.TD2[which(prairie.hill.TD2$Method!="Observed"),]
df.lineTD2$Method <- factor(df.lineTD2$Method, 
                          c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.TD.fig <- ggplot(prairie.hill.TD0, aes(x = nt, y = qD,   color = Treatment_type)) +
  facet_grid(~Treatment_cat)+
  geom_point(aes(), shape = 1, size=2, data = df.pointTD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineTD0) +
  geom_point(aes(), shape = 1, size=2, data = df.pointTD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineTD2) +
  labs(x="Number of sampling units", y="Taxonomic Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_colour_manual( values = c("#31688e","#35b779", "#443983","#90d743","#21918c","#fde725", "#440154") ) +  
  labs(title='Taxonomic Diversity', color="Treatment", x= "")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "bottom") +
  guides(col = guide_legend(ncol = 7)) +
  annotate(
    "text", label = "q = 0",
    x = 550, y = 120, size = 5, colour = "black"
  ) +   annotate(
    "text", label = "q = 2",
    x = 550, y = 29, size = 5, colour = "black"
  ) 


prairie.TD.fig 





# ========================================================================================================== #
# Phylogenetic diversity
setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")

phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

phylo.prep$species[phylo.prep$species == "Lespedeza_juncea"] = "Lespedeza_juncea_var_sericea"

phylo.prep.treats <- phylo.prep %>% 
  #filter(!subplot == 10 ) %>%
  gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  filter(!grepl("_spp",species))

head(phylo.prep.treats)


# full species list, for taxonomic diversity and phylo diversity
phylo.list <- phylo.prep.treats %>%
  split(.$Treatment)

phylo.list

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
#variety is messing up the tree with an error. change the species name. do the same above to match,
tree$tip.label[tree$tip.label == "Lespedeza_juncea"] = "Lespedeza_juncea_var_sericea"

PD_treat_out <- iNEXT3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 1, 2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1:600),
                  # OR  # endpoint = 20, knots = 1,
                  nboot = 0,  PDtree = tree, PDtype = "meanPD") 

 PD_treat_out

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
save(PD_treat_out, file = "PD_treat_sep_out.Rdata")


load(file = "PD_treat_sep_out.Rdata")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.PD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.PD.csv", row.names=FALSE)

prairie.hill.PD <- read.csv("prairie.hill.treats.sep.PD.csv",  header= TRUE)

head(prairie.hill.PD)

prairie.hill.PD$Treatment_cat <- factor(prairie.hill.PD$Treatment_cat, levels = c("Nutrients", "Assembly",  "Invasion"))

prairie.hill.PD$Treatment_type<- as.factor(prairie.hill.PD$Treatment_type)
levels(prairie.hill.PD$Treatment_type)
prairie.hill.PD$Treatment_type <- factor(prairie.hill.PD$Treatment_type, 
                                         levels = c("Control", "Nutrients", 
                                                    "Both first", "Forbs first", "Grass first", 
                                                    "Early", "Late"))


prairie.hill.PD0 <- prairie.hill.PD %>% filter(Order.q == "q = 0") 
prairie.hill.PD2 <- prairie.hill.PD %>% filter(Order.q == "q = 2") 

df.pointPD0 <- prairie.hill.PD0[which(prairie.hill.PD0$Method=="Observed"),]
df.linePD0 <- prairie.hill.PD0[which(prairie.hill.PD0$Method!="Observed"),]
df.linePD0$Method <- factor(df.linePD0$Method, 
                          c("Rarefaction", "Extrapolation"))

df.pointPD2 <- prairie.hill.PD2[which(prairie.hill.PD2$Method=="Observed"),]
df.linePD2 <- prairie.hill.PD2[which(prairie.hill.PD2$Method!="Observed"),]
df.linePD2$Method <- factor(df.linePD2$Method, 
                          c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.PD.fig <- ggplot(prairie.hill.PD0, aes(x = nt, y = qPD,   color = Treatment_type)) +
  facet_wrap(~Treatment_cat)+
  geom_point(aes(), shape = 1, size=2, data = df.pointPD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.linePD0) +
  geom_point(aes(), shape = 1, size=2, data = df.pointPD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.linePD2) +
  labs(x="Number of sampling units", y="Phylogenetic Diversity",title="") +
  scale_colour_manual( values = c("#31688e","#35b779", "#443983","#90d743","#21918c","#fde725", "#440154") ) +  
  labs(title='Phylogenetic Diversity', color = "Treatment", x = "")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 7)) +
  annotate(
    "text", label = "q = 0",
    x = 550, y = 17, size = 6, colour = "black"
  ) +   annotate(
    "text", label = "q = 2",
    x = 550, y = 8, size = 6, colour = "black"
  )


prairie.PD.fig


# ========================================================================================================== #
#  Functional diversity

# match traits and corrected species list from cover data

setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data")
traits <- read.csv("imputed_trait_matrix.csv",  header= TRUE)

head(traits)

sp_traits <- traits %>% select(species) %>%
  mutate(traits_sp = species) %>% 
  mutate(species = str_replace_all(species, 
                                   pattern = "\\_", replacement = " "))

View(sp_traits)


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

View(traits_fixed)

write.csv(traits_fixed, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/imputed_trait_matrix_fixed.csv", row.names=FALSE)

head(trait.prep)

trait.prep.treats <- trait.prep %>% 
  #filter(!subplot == 10 ) %>%
  gather(Treatment_cat, Treatment_type, "Nutrients":"Assembly") %>%
  unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F)

head(trait.prep.treats)

# full species list, for taxonomic diversity
trait.list <- trait.prep.treats %>%
  split(.$Treatment)

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
                  endpoint = 600, #knots = 1,
                  nboot = 0,  FDdistM = distM, FDtype = 'tau_values', 
                  FDtau = NULL)

FD_treat_out
setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/")
save(FD_treat_out, file = "FD_treat_sep_out.Rdata")


load(file = "FD_treat_sep_out.Rdata")

#$AUCiNextEst$size_based
prairie.FD.df <- FD_treat_out %>% 
  purrr::pluck("FDiNextEst", "size_based")

View(prairie.FD.df)

prairie.hill.FD <- prairie.FD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.FD, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.FD.csv", row.names=FALSE)


prairie.hill.FD <- read.csv("prairie.hill.treats.sep.FD.csv",  header= TRUE)


head(prairie.hill.FD)

is.numeric(prairie.hill.FD$Assemblage)

prairie.hill.FD$Treatment_cat <- factor(prairie.hill.FD$Treatment_cat, levels = c("Nutrients",  "Assembly", "Invasion"))

prairie.hill.FD$Treatment_type<- as.factor(prairie.hill.FD$Treatment_type)
levels(prairie.hill.FD$Treatment_type)
prairie.hill.FD$Treatment_type <- factor(prairie.hill.FD$Treatment_type, 
                                         levels = c("Control", "Nutrients", 
                                                    "Both first", "Forbs first", "Grass first",
                                                    "Early", "Late"))


prairie.hill.FD0 <- prairie.hill.FD %>% filter(Order.q == "q = 0") 
prairie.hill.FD2 <- prairie.hill.FD %>% filter(Order.q == "q = 2") 

df.pointFD0 <- prairie.hill.FD0[which(prairie.hill.FD0$Method=="Observed"),]
df.lineFD0 <- prairie.hill.FD0[which(prairie.hill.FD0$Method!="Observed"),]
df.lineFD0$Method <- factor(df.lineFD0$Method, 
                          c("Rarefaction", "Extrapolation"))

df.pointFD2 <- prairie.hill.FD2[which(prairie.hill.FD2$Method=="Observed"),]
df.lineFD2 <- prairie.hill.FD2[which(prairie.hill.FD2$Method!="Observed"),]
df.lineFD2$Method <- factor(df.lineFD2$Method, 
                          c("Rarefaction", "Extrapolation"))

# make an iNext style plot
prairie.FD.fig <- ggplot(prairie.hill.FD0, aes(x = nt, y = qFD,   color = Treatment_type)) +
  facet_wrap(~Treatment_cat)+
  geom_point(aes(), shape = 1, size=2, data = df.pointFD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineFD0) +
  geom_point(aes(), shape = 1, size=2, data = df.pointFD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineFD2) +
  labs(x="Number of sampling units", y="Functional Diversity",title="") +
  scale_colour_manual( values = c("#31688e","#35b779", "#443983","#90d743","#21918c","#fde725", "#440154") ) +  
  labs(title='Functional Trait Diversity', color="Treatment")+
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15)) +
  annotate(
    "text", label = "q = 0",
    x = 550, y = 22, size = 6, colour = "black"
  ) +   annotate(
    "text", label = "q = 2",
    x = 550, y = 15, size = 6, colour = "black"
  )


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
