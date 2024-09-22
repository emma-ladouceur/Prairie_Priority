


pres <- read.csv("~/Dropbox/_Projects/Prairie_Priority/Prairie_Plot_Presence_Absence_100621.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sampled_dat <- read.csv("~/Dropbox/_Projects/Prairie_Priority/Data/sampled_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



####################################
# Presence Data:
# the species presence data (walk around checklist of every plot)


# Here we combine that data with cover data and convert the whole list to incidence data for hill numbers

head(pres)

pres_long <- pres %>% group_by(plot) %>%
  gather(species, pres, graminoid:Lindera.benzoin) %>%
  filter(!pres %in% c("0")) %>% # remove 0 values
  arrange(plot)

head(pres_long)
nrow(pres_long)

# use only sampled dat for blocks and site level data
head(sampled_dat)
nrow(sampled_dat)

sampled_pres <- sampled_dat %>%
  select(subplot,plot, block, nutrients, invasion, Grass.forbs, species, cover) %>% 
  mutate(pres = 1)

nrow(sampled_pres)
head(sampled_pres)

View(sampled_pres)

# extract plot treatment and block info
plot_dat <- sampled_dat %>% ungroup() %>%
  select( plot, block, nutrients, invasion, Grass.forbs) %>%
  distinct()

head(plot_dat)

pres_dat <- plot_dat %>% left_join(pres_long) %>% ungroup() %>%
  mutate(subplot = 10)

head(pres_dat)

full_pres <- sampled_pres %>% bind_rows(pres_dat) %>% 
  arrange(plot, subplot, block, species)

nrow(full_pres)

presence_dat <- full_pres %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
   ) #   %>% unite(treatment2, Nutrients , Invasion , Assembly , remove = FALSE)  %>%
  # distinct()

head(presence_dat)
nrow(presence_dat)

View(presence_dat)

write.csv(presence_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/pres_and_cover_plot.csv", row.names=FALSE)

