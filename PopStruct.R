###### URBA POPULATION STRUCTURE ANALYSES CODE - MEDYCI ######
# project (created first also in GH), and linked via:
# if doubts see: https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/
#library(usethis)
#library(gitcreds)
#create_github_token() # to create a token and link project and github
#gitcreds_set()
#use_github()

####### CODE STARTS #######
library(tidyverse)
s.index<-read.delim2("s_index_2018_2023_CBMS_uBMS.txt", sep = ";") # include CBMS sites meeting criteria (check comment on github) + ubms
head(s.index)
s.index$TOTAL_NM <- as.numeric(s.index$TOTAL_NM)
s.index$SINDEX <- as.numeric(s.index$SINDEX)

# calculate pop change: log(Nt+1/Nt) == log(gam_index[i+1]/gam_index[i]).
# the code writes the value of pop change in the first column (equivalent to (t+1))
s.index <- data.frame(s.index %>%
                          group_by(SITE_ID, SPECIES) %>%
                          mutate(
                            n_change = log(lead(SINDEX)/SINDEX) # !! check those -INF for study species in case o can be change to 0.00001 (ex. in piepra in P Robert)
                          ))

# filter P. rapae to test for models
piepra <- s.index %>% filter(SPECIES == "Pieris rapae")
summary(piepra) # double check with DGC that s-index is correct. Values are very high compared to those from the EXTINCT data...
piepra %>% group_by(M_YEAR, SCHEME) %>%  summarise(COUNT = sum(SINDEX)) #seems ok

# model pop change with only Density dependence for piepra by site (simpel first)
library(plyr)
mdl.f<-function(piepra){summary(lmer((n_change) ~ SINDEX + (1|M_YEAR),data=piepra))$coefficients[,1:2]} # few data for adding year as random + Inf and NA might need to be removed 

mdl.est <-dlply(piepra, "SITE_ID", mdl.f)
time_estimates2[1:10]
time_estimates_invSine <- ldply (time_estimates2, data.frame)

mdl.s<-function(butterflydens3){summary(lmer(IHST(gam_index) ~ Timeseries*rangeposition + (1|site),data=butterflydens3))$coefficients[,1:5]} 



log10(gamdens+0.0001) ~ (rangeposition) +(1|species) +(1|site) ,REML = T ,data=butterflydatadens)