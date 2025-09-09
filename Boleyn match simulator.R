rm(list=ls())
library(readxl)
options(scipen=999)
#library(combinat)
library(tidyverse)
#read in the data
bol_file <-"G:/My Drive/Real Tennis/Boleyn/Boleyn v1.2.xlsm"
mod5_param <- as.double(read_excel(bol_file,
                                   sheet="IO- Stata",range="F15",col_names = FALSE))

players32<-read_excel(bol_file,
                      sheet="C- Tournament",range="C58:D74") %>% 
  pivot_longer(cols=everything(),names_to="name") %>% 
  select(value) %>% 
  distinct() %>% 
  rename(p1=value)

player_combo<-combn(players32$p1,2) %>% 
  t() %>% 
  as_tibble()
combo_reverse<-data.frame(V1=player_combo$V2,V2=player_combo$V1)
player_combo<-bind_rows(player_combo,combo_reverse) %>% 
  unique()
rm(combo_reverse)

player_ranks<-read_excel(bol_file,
                         sheet="I- Rankings",range="D6:H100")

#apply handicaps to each combination
player_combo <- left_join(player_combo,player_ranks,by=c("V1"="Player")) 
player_combo<-select(player_combo,c(V1,V2,`AmendedRTO`)) %>% 
  rename(p1_hcap=`AmendedRTO`)
player_combo <- left_join(player_combo,player_ranks,by=c("V2"="Player")) 
player_combo<-select(player_combo,c(V1,V2,p1_hcap,`AmendedRTO`)) %>% 
  rename(p2_hcap=`AmendedRTO`) %>% 
  mutate(hcapdiff=p2_hcap-p1_hcap) 
player_combo <- mutate(player_combo,
                       pr_winpoint = pnorm(hcapdiff*mod5_param),
                       pr_winmatch5=NA)

#run through every combination and estimate the best of 5 match probability
for (z in 1:nrow(player_combo)) {

#how to win a game
# pr_winpoint <- player_combo$pr_winpoint[z]
# transitions<-matrix(c(0.0),nrow=5,ncol=5,byrow=TRUE)
# transitions[1:4,5]<-1
# for(j in 4:1){
#   for (i in 4:1){
#     transitions[i,j]<-pr_winpoint*transitions[i,j+1]+(1-pr_winpoint)*transitions[i+1,j]
#   }
# }
#probability of winning a game is then transitions[1,1]
pr_wingame=player_combo$pr_winpoint[z]

matrix_set<-matrix(c(0),nrow=7,ncol=7,byrow=TRUE)
matrix_set[1:6,7]<-1
for(j in 6:1){
  for (i in 6:1){
    matrix_set[i,j]<-pr_wingame*matrix_set[i,j+1]+(1-pr_wingame)*matrix_set[i+1,j]
  }
}
#probability of winning a set is then matrix_set[1,1]
pr_winset=matrix_set[1,1]
player_combo$pr_winset[z]=as.double(pr_winset)

bestof<-5
matrix_match<-matrix(c(0),nrow=(bestof+3)/2,ncol=(bestof+3)/2,byrow=TRUE)
matrix_match[1:(bestof+1)/2,(bestof+3)/2]<-1
for(j in ((bestof+1)/2):1){
  for (i in ((bestof+1)/2):1){
    matrix_match[i,j]<-pr_winset*matrix_match[i,j+1]+(1-pr_winset)*matrix_match[i+1,j]
  }
}
#probability of winning a set is then matrix_set[1,1]
pr_winmatch=matrix_match[1,1]
player_combo$pr_winmatch5[z]=as.double(pr_winmatch)
}
