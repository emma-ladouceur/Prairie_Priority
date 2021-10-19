
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Prairie_Priority'
alpha_dat <- read.csv(paste0(path, '/alpha_group_cover.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



alpha_comp <-  brm(relative_cover ~  cover_type + Nutrients * Invasion * Assembly  + (cover_type + Nutrients * Invasion * Assembly  | block/plot),
                   data = alpha_dat, family = student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(alpha_comp,
     file=Sys.getenv('OFILE'))



