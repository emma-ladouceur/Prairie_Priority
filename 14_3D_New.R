
library(tidyverse)
library(viridis)
library(ape)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(patchwork)
library(MetBrewer)

# check for updates ?
# install.packages("remotes")
# library(remotes)
# install.packages("rlang")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")
library(iNEXT.3D)
# cite 
citation("iNEXT.3D")




#met.brewer("Greek", 3))

setwd("~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/")
setwd("~/Dropbox/_Projects/Prairie_Priority/Data")
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
  filter(!species == "graminoid",
         Assembly == "Both first") %>%
  select(-Assembly) %>%
    mutate(species = str_replace(species, "Lespedeza juncea var. sericea", "Lespedeza juncea"))

head(prairie.prep)
nrow(prairie.prep)

prairie.prep %>% select(subplot) %>% distinct()

levels(prairie.prep$species)

#prairie.info <- prairie.prep %>% select(treat_id)

#write.csv(prairie.prep, "~/Dropbox/_Projects/Prairie_Priority/Data/3D_prep.csv", row.names=FALSE)

prairie.prep.treats <- prairie.prep %>% 
  #gather(Treatment_cat, Treatment_type, "Nutrients":"Invasion") %>%
  unite(Treatment, Nutrients, Invasion, sep="_", remove= F)

head(prairie.prep.treats)


prairie_info <- prairie.prep.treats %>% select(Treatment) %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment))


write.csv(prairie.prep.treats, "~/Dropbox/_Projects/Prairie_Priority/Data/prairie.prep.treats.csv", row.names=FALSE)

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
??iNEXT3D

TD_treat_out <- iNEXT3D(data = prairie.matrix.list, diversity = 'TD', q = c(0,2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1:200), 
                  #endpoint = 160, knots =1,
                  nboot = 50)

TD_treat_out

setwd("~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/")
save(TD_treat_out, file = "TD_treat_sep_out.Rdata")

load(file = "TD_treat_sep_out.Rdata")

#$iNextEst$size_based
prairie.TD.df <- TD_treat_out %>% 
  purrr::pluck("iNextEst", "size_based")

head(prairie.TD.df)

head(prairie.prep)


prairie.hill.TD <- prairie.TD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                               #Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )
  
head(prairie.hill.TD)
View(prairie.hill.TD)

write.csv(prairie.hill.TD, "~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.TD.csv", row.names=FALSE)

setwd("~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/")
prairie.hill.TD <- read.csv("prairie.hill.treats.sep.TD.csv",  header= TRUE)

head(prairie.hill.TD)

prairie.hill.TD %>% filter(Method == "Observed")

prairie.hill.TD$Assemblage <- factor(prairie.hill.TD$Assemblage, levels = c("Nutrients_Late", "Control_Late", "Nutrients_Early",  "Control_Early"))

#prairie.hill.TD$Treatment_type<- as.factor(prairie.hill.TD$Treatment_type)
# levels(prairie.hill.TD$Treatment_type)
# prairie.hill.TD$Treatment_type <- factor(prairie.hill.TD$Treatment_type, 
#                                          levels = c("Control", "Nutrients", 
#                                                     #"Both first", "Forbs first", "Grass first",
#                                                     "Early", "Late"))

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


MetBrewer::colorblind_palettes
display_all()
display_all(colorblind_only = T)

# make an iNext style plot
prairie.TD.fig0 <- ggplot(prairie.hill.TD0, aes(x = nt, y = qD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointTD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineTD0) +
  labs(x="Number of sampling units", y="Taxonomic Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  +
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='Taxonomic Diversity', subtitle = 'q = 0')+
  xlim(0,160)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none", plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.TD.fig0


prairie.TD.fig2 <- ggplot(prairie.hill.TD0, aes(x = nt, y = qD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointTD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineTD2) +
  labs(x="Number of sampling units", y="Taxonomic Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='', subtitle = 'q = 2')+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none",
                            axis.title.y = element_blank(), plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.TD.fig <- (prairie.TD.fig0 +  prairie.TD.fig2)


prairie.TD.fig


# ========================================================================================================== #
# Phylogenetic diversity
setwd("~/Dropbox//_Projects/Prairie_Priority/Data")

phylo.prep <- read.csv("phylo_prep.csv")

head(phylo.prep)

phylo.prep$species[phylo.prep$species == "Lespedeza_juncea"] = "Lespedeza_juncea_var_sericea"

phylo.prep.treats <- phylo.prep %>% 
  #filter(!subplot == 10 ) %>%
  filter(Assembly == "Both first") %>%
  select(-Assembly) %>%
  #gather(Treatment_cat, Treatment_type, "Nutrients":"Invasion") %>%
 # unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  unite(Treatment, Nutrients, Invasion, sep="_", remove= F) %>%
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

PD_treat_out <- iNEXT3D(data = phylo.matrix.list, diversity = 'PD', q = c(0, 2), datatype = 'incidence_raw', #base = 'size',
                  size = c(1:200),
                  # OR  # endpoint = 20, knots = 1,
                  nboot = 50,  PDtree = tree, PDtype = "meanPD") 

 PD_treat_out

setwd("~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/")
save(PD_treat_out, file = "PD_treat_sep_out.Rdata")


load(file = "PD_treat_sep_out.Rdata")

#$PDiNextEst$size_based
prairie.PD.df <- PD_treat_out %>% 
  purrr::pluck("PDiNextEst", "size_based")

View(prairie.PD.df)

prairie.hill.PD <- prairie.PD.df %>% left_join(prairie_info)%>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                              # Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.PD, "~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.PD.csv", row.names=FALSE)

prairie.hill.PD <- read.csv("prairie.hill.treats.sep.PD.csv",  header= TRUE)

head(prairie.hill.PD)

prairie.hill.PD$Assemblage <- factor(prairie.hill.PD$Assemblage, levels = c("Nutrients_Late", "Control_Late", "Nutrients_Early",  "Control_Early"))

# prairie.hill.PD$Treatment_type<- as.factor(prairie.hill.PD$Treatment_type)
# levels(prairie.hill.PD$Treatment_type)
# prairie.hill.PD$Treatment_type <- factor(prairie.hill.PD$Treatment_type, 
#                                          levels = c("Control", "Nutrients", 
#                                                     "Both first", "Forbs first", "Grass first", 
#                                                     "Early", "Late"))


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

prairie.PD.fig0 <- ggplot(prairie.hill.PD0, aes(x = nt, y = qPD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointPD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.linePD0) +
  labs(x="Number of sampling units", y="Phylogenetic Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='Phylogenetic Diversity', #subtitle = 'q = 0'
       )+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none", plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.PD.fig2 <- ggplot(prairie.hill.PD0, aes(x = nt, y = qPD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointPD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.linePD2) +
  labs(x="Number of sampling units", y="Phylogenetic Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='', #subtitle = 'q = 2'
       )+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none",
                            axis.title.y = element_blank(), plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.PD.fig <- (prairie.PD.fig0 +  prairie.PD.fig2)


prairie.PD.fig




# ========================================================================================================== #
#  Functional diversity

# match traits and corrected species list from cover data

setwd("~/Dropbox/_Projects/Prairie_Priority/Data")
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

write.csv(traits_fixed, "~/Dropbox/_Projects/Prairie_Priority/Data/imputed_trait_matrix_fixed.csv", row.names=FALSE)

head(trait.prep)

trait.prep.treats <- trait.prep %>% 
  #filter(!subplot == 10 ) %>%
  # filter(Assembly == "Both first") %>%
  # select(-Assembly) %>%
  #gather(Treatment_cat, Treatment_type, "Nutrients":"Invasion") %>%
  # unite(Treatment, Treatment_cat, Treatment_type, sep="_", remove= F) %>%
  unite(Treatment, Nutrients, Invasion, sep="_", remove= F) 

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



FD_treat_out <- iNEXT3D(data = trait.matrix.list, diversity = 'FD', q = c(0, 2), datatype = 'incidence_raw', #base = 'size',
                  #size = c(1:188), 
                  endpoint = 200, #knots = 1,
                  nboot = 50,  FDdistM = distM, FDtype = 'tau_values', 
                  FDtau = NULL)

FD_treat_out
setwd("~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/")
save(FD_treat_out, file = "FD_treat_sep_out.Rdata")


load(file = "FD_treat_sep_out.Rdata")

#$AUCiNextEst$size_based
prairie.FD.df <- FD_treat_out %>% 
  purrr::pluck("FDiNextEst", "size_based")

View(prairie.FD.df)

prairie.hill.FD <- prairie.FD.df %>% left_join(prairie_info) %>%
  mutate( Order.q  = case_when(Order.q  == "0" ~ "q = 0",
                              # Order.q == "1" ~ "q = 1",
                               Order.q == "2" ~ "q = 2") )


write.csv(prairie.hill.FD, "~/Dropbox/_Projects/Prairie_Priority/Data/Treat Sep/prairie.hill.treats.sep.FD.csv", row.names=FALSE)


prairie.hill.FD <- read.csv("prairie.hill.treats.sep.FD.csv",  header= TRUE)


head(prairie.hill.FD)

is.numeric(prairie.hill.FD$Assemblage)

prairie.hill.FD$Assemblage <- factor(prairie.hill.FD$Assemblage, levels = c("Nutrients_Late", "Control_Late", "Nutrients_Early",  "Control_Early"))

# prairie.hill.FD$Treatment_type<- as.factor(prairie.hill.FD$Treatment_type)
# levels(prairie.hill.FD$Treatment_type)
# prairie.hill.FD$Treatment_type <- factor(prairie.hill.FD$Treatment_type, 
#                                          levels = c("Control", "Nutrients", 
#                                                     "Both first", "Forbs first", "Grass first",
#                                                     "Early", "Late"))


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


prairie.FD.fig0 <- ggplot(prairie.hill.FD0, aes(x = nt, y = qFD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointFD0) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineFD0) +
  labs(x="Number of sampling units", y="Functional Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='Functional Diversity', #subtitle = 'q = 0'
       )+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none", plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.FD.fig2 <- ggplot(prairie.hill.FD0, aes(x = nt, y = qFD,   color = Assemblage)) +
  geom_point(aes(), shape = 1, size=2, data = df.pointFD2) +
  geom_line(aes(linetype = Method), lwd=1.5, data = df.lineFD2) +
  labs(x="Number of sampling units", y="Functional Diversity",title="") +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_color_manual(values=met.brewer("Hokusai3", 4))+
  # scale_colour_manual( values = c("#31688e","#35b779",
  #                                 #"#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154") ) +  
  labs(title='', #subtitle = 'q = 2'
       )+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal", legend.position = "none",
                            axis.title.y = element_blank(), plot.subtitle = element_text(hjust = 0.5) ) +
  guides(col = guide_legend(ncol = 7)) 

prairie.FD.fig <- (prairie.FD.fig0 +  prairie.FD.fig2)


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

# 
trt.leg <- ggplot() +
  geom_line(data = prairie.hill.FD, aes(x = nt, y = qFD,   color = Assemblage), lwd = 1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="C")  +
  scale_color_manual(values=met.brewer("Hokusai3", 4),
                     labels=c("Nutrients late invasion", "Control late invasion",
                              "Nutrients early invasion", "Control early invasion"))+
  # scale_colour_manual( values = c("#31688e","#35b779",# "#443983","#90d743","#21918c",
  #                                 "#fde725", "#440154"),
  #                      labels=c("Nutrients late invasion", "Control late invasion",
  #                               "Nutrients early invasion", "Control early invasion")) +  
  labs( color="Treatments")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 2))

trt.legend <- g_legend(trt.leg)


 prairie.div <- (prairie.TD.fig  / prairie.PD.fig / ( prairie.FD.fig + theme(legend.position="none") ) / (line.legend) / (trt.legend)   + plot_layout(heights = c(10,10,10,2,2))) 

 prairie.div
