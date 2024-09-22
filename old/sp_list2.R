



sp <- read.csv("pres_and_cover_plot.csv", header= TRUE)
# corrected species list
corrected_sp_list <- read.csv("species_corrected_complete.csv", row.names = 1, header= TRUE)


head(sp)

# correct species  names  in cover data
head(corrected_sp_list)
View(corrected_sp_list)

new_sp <- corrected_sp_list %>% mutate(species = old_name,
                                       corrected_sp = name) %>%
  select(-c(old_name, name, morphotype)) %>%
  filter(!corrected_sp == "graminoid sp.") %>%
  mutate( corrected_sp = case_when(
    corrected_sp == "Lespedeza juncea var. sericea" ~ "Lespedeza cuneata",
    TRUE ~ corrected_sp ) ) 

head(new_sp)
