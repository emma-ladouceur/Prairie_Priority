


library(tidyverse)
library(lme4)
librry(lmerTest)
library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1


library(ggplot2)
library(stringr)
library(yarrr)
library(patchwork)




# alpha
alpha_subplot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Data/alpha_composition.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(alpha_subplot)

treats <- alpha_subplot %>% select(treatment, nutrients, invasion, Grass.forbs) %>%
  distinct()

alpha_subplot$treatment <- as.factor(alpha_subplot$treatment)
levels(alpha_subplot$treatment)

alpha_subplot2 <- alpha_subplot %>% # relabel treatments to make logical sense
  mutate( Nutrients = case_when(nutrients == "0" ~ "Control",
                                nutrients == "1" ~ "Nutrients"),
          Invasion = case_when(invasion == "e" ~ "Early",
                               invasion == "l" ~ "Late"),
          Assembly = case_when( Grass.forbs == "g" ~ "Grass first",
                                Grass.forbs == "f" ~ "Forbs first",
                                Grass.forbs == "b" ~ "Both first")
  )   %>% unite(treatment, Nutrients , Invasion , Assembly , remove = FALSE) 

View(alpha_subplot2)
nrow(alpha_subplot2)


alpha_rich_lme <-  lmer(alpha_forb_rich ~  Nutrients * Invasion * Assembly  + ( 1  | block/plot),
                   data = alpha_subplot2)



summary(alpha_rich_lme)

# options
# https://stackoverflow.com/questions/11072544/how-to-get-coefficients-and-their-confidence-intervals-in-mixed-effects-models

fixef(alpha_rich_lme)

coef(summary(as(alpha_rich_lme,"merModLmerTest")))
     
#likelihood profile
confint(alpha_rich_lme)



# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

emm1 = emmeans(alpha_rich_lme, specs = pairwise ~ Nutrients:Invasion:Assembly)

emm1$emmeans

emm1$contrasts

emm1$contrasts %>%
  confint()


save(alpha_rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/alpha_rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Prairie_Priority/Model_Fits/alpha_rich.Rdata')




