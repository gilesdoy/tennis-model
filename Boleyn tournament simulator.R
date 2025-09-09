rm(list=ls())
library(readxl)
library(tidyverse)
# dfgrid <- read_excel("G:/My Drive/Real Tennis/Boleyn/Boleyn v1.2.xlsm", 
#                      sheet = "C- TournGrid", range = "a1:q17")
# dflong=pivot_longer(dfgrid,!p1,names_to="p2",values_to="p1win")
source("G:/My Drive/Real Tennis/Boleyn/Boleyn match simulator.R")
dflong<-player_combo %>% 
  rename(p1=V1,p2=V2,p1win=pr_winmatch5)
matches=read_excel("G:/My Drive/Real Tennis/Boleyn/Boleyn v1.2.xlsm", 
                   sheet = "C- Tournament", range = "c58:f89")
fracdec <- read_excel("G:/My Drive/Real Tennis/Boleyn/Boleyn v1.2.xlsm",
                      sheet="I- Decimal to Fractional",range="B2:C110")
record <-data.frame()
# record=data.frame(win=NA,final1=NA,final2=NA,semi1=NA,semi2=NA,semi3=NA,semi4=NA,
                  # q1=NA,q2=NA,q3=NA,q4=NA,q5=NA,q6=NA,q7=NA,q8=NA)

totalsimulations=10000
starttime=Sys.time()

for (i in 1:totalsimulations) {
  
  # Round of 32 ----
  for (j in 1:16) {
    if (matches$`Completed?`[j]==0) {
      dflong_filter=dflong %>% 
        filter(p1==matches$p1[j] & p2==matches$p2[j])
      
      if(runif(1)<dflong_filter$p1win[1]) {
        matches$Winner[j]=matches$p1[j]
      } else {
        matches$Winner[j]=matches$p2[j]
      }
      
    } #end of if matches completed=0
  } #end of for j
  matches$p1[17]=matches$Winner[1]
  matches$p2[17]=matches$Winner[2]
  matches$p1[18]=matches$Winner[3]
  matches$p2[18]=matches$Winner[4]
  matches$p1[19]=matches$Winner[5]
  matches$p2[19]=matches$Winner[6]
  matches$p1[20]=matches$Winner[7]
  matches$p2[20]=matches$Winner[8]
  matches$p1[21]=matches$Winner[9]
  matches$p2[21]=matches$Winner[10]
  matches$p1[22]=matches$Winner[11]
  matches$p2[22]=matches$Winner[12]
  matches$p1[23]=matches$Winner[13]
  matches$p2[23]=matches$Winner[14]
  matches$p1[24]=matches$Winner[15]
  matches$p2[24]=matches$Winner[16]
  
  
  # Round of 16 ----
for (j in 17:24) {
    if (matches$`Completed?`[j]==0) {
dflong_filter=dflong %>% 
  filter(p1==matches$p1[j] & p2==matches$p2[j])

if(runif(1)<dflong_filter$p1win[1]) {
  matches$Winner[j]=matches$p1[j]
} else {
  matches$Winner[j]=matches$p2[j]
}

} #end of if matches completed=0
} #end of for j

matches$p1[25]=matches$Winner[17]
matches$p2[25]=matches$Winner[18]
matches$p1[26]=matches$Winner[19]
matches$p2[26]=matches$Winner[20]
matches$p1[27]=matches$Winner[21]
matches$p2[27]=matches$Winner[22]
matches$p1[28]=matches$Winner[23]
matches$p2[28]=matches$Winner[24]

# Quarter Finals ----
for (j in 25:28) {
  if (matches$`Completed?`[j]==0) {
    dflong_filter=dflong %>% 
      filter(p1==matches$p1[j] & p2==matches$p2[j])
    
    if(runif(1)<dflong_filter$p1win[1]) {
      matches$Winner[j]=matches$p1[j]
    } else {
      matches$Winner[j]=matches$p2[j]
    }
    
  } #end of if matches completed=0
} #end of for j

#fill the semi finals
matches$p1[29]=matches$Winner[25]
matches$p2[29]=matches$Winner[26]
matches$p1[30]=matches$Winner[27]
matches$p2[30]=matches$Winner[28]

#Semi finals ----
for (j in 29:30) {
  if (matches$`Completed?`[j]==0) {
    dflong_filter=dflong %>% 
      filter(p1==matches$p1[j] & p2==matches$p2[j])
    
    if(runif(1)<dflong_filter$p1win[1]) {
      matches$Winner[j]=matches$p1[j]
    } else {
      matches$Winner[j]=matches$p2[j]
    }
    
  } #end of if matches completed=0
} #end of for j

matches$p1[31]=matches$Winner[29]
matches$p2[31]=matches$Winner[30]

#The final ----

  if (matches$`Completed?`[31]==0) {
    dflong_filter=dflong %>% 
      filter(p1==matches$p1[31] & p2==matches$p2[31])
    
    if(runif(1)<dflong_filter$p1win[1]) {
      matches$Winner[31]=matches$p1[31]
    } else {
      matches$Winner[31]=matches$p2[31]
    }
    
  } #end of if matches completed=0

# minirecord=data.frame(win=NA,final1=NA,final2=NA,semi1=NA,semi2=NA,semi3=NA,semi4=NA,
#                       q1=NA,q2=NA,q3=NA,q4=NA,q5=NA,q6=NA,q7=NA,q8=NA)
# minirecord$win=matches$Winner[31]
# minirecord$final1=matches$Winner[30]
# minirecord$final2=matches$Winner[29]
# minirecord$semi1=matches$Winner[28]
# minirecord$semi2=matches$Winner[27]
# minirecord$semi3=matches$Winner[26]
# minirecord$semi4=matches$Winner[25]
# minirecord$q1=matches$Winner[17]
# minirecord$q2=matches$Winner[18]
# minirecord$q3=matches$Winner[19]
# minirecord$q4=matches$Winner[20]
# minirecord$q5=matches$Winner[21]
# minirecord$q6=matches$Winner[22]
# minirecord$q7=matches$Winner[23]
# minirecord$q8=matches$Winner[24]
minirecord=tibble(win=NA,final1=NA,final2=NA,semi1=NA,semi2=NA,semi3=NA,semi4=NA)
minirecord=tibble(win=matches$Winner[31],
                  final=list(final=matches$Winner[29:30]),
                  semi=list(semi=matches$Winner[25:28]),
                  quarter=list(quarter=matches$Winner[17:24]))

record=bind_rows(record,minirecord)
if (i/totalsimulations*100 ==round(i/totalsimulations*100,-1)) {
  finishtime=(((Sys.time()-starttime)/i)*totalsimulations)+starttime
  print(paste(i,"iterations done. Finish:",finishtime,sep=" "))
}

  } #end of for i
table(record$win)/totalsimulations

# record <- record %>% filter(!is.na(win)) %>% 
#   mutate(id=row_number()) %>% 
#   select(q1,q2,q3,q4,q5,q6,q7,q8,semi1,semi2,semi3,semi4,final1,final2,win,id)
# longrecord <- pivot_longer(record,cols=!"id",names_to="position",values_to="player")
# 
# winner <- longrecord %>% 
#   filter(str_detect(position,"^win")) %>% 
#   group_by(player) %>% 
#   summarise(c_winner=n()/totalsimulations)
# semi <- longrecord %>% 
#   filter(str_detect(position,"^semi")) %>% 
#   group_by(player) %>% 
#   summarise(c_semi=n()/totalsimulations)
# finalist <- longrecord %>% 
#   filter(str_detect(position,"^final")) %>% 
#   group_by(player) %>% 
#   summarise(c_finalist=n()/totalsimulations)
# quarters <- longrecord %>% 
#   filter(str_detect(position,"^q")) %>% 
#   group_by(player) %>% 
#   summarise(c_quarters=n()/totalsimulations)
# 
# overall <- full_join(winner,finalist,by="player") %>% 
#   full_join(., semi,by="player") %>% 
#   full_join(., quarters,by="player") 
# 
#write_csv(overall,"G:/My Drive/Real Tennis/Boleyn/Boleyn tournament simulator overall.csv")


print("Done!")
probwin <- record %>% 
  select(win) %>% 
  rename(player=win) %>% 
  group_by(player) %>% 
  summarise(winner=n()/nrow(record)) %>% 
  pivot_longer(cols=winner,names_to="stage",values_to="probability")
probfinal <- record %>% 
  select(final) %>%
  rename(player=final) %>% 
  unnest(player) %>% 
  group_by(player) %>% 
  summarise(finalist=n()/nrow(record)) %>% 
  pivot_longer(cols=finalist,names_to="stage",values_to="probability")
probsemi <- record %>% 
  select(semi) %>%
  rename(player=semi) %>% 
  unnest(player) %>% 
  group_by(player) %>% 
  summarise(semifinalist=n()/nrow(record)) %>% 
  pivot_longer(cols=semifinalist,names_to="stage",values_to="probability")
simresultssummary <- tibble(bind_rows(probwin,probfinal,probsemi) )
simwider <- pivot_wider(simresultssummary,names_from="stage",values_from="probability")
simwider[is.na(simwider)]<-0
write_csv(simwider,"G:/My Drive/Real Tennis/Boleyn/Boleyn tournament simulator sim summary.csv")

fracdec <- read_excel("G:/My Drive/Real Tennis/Boleyn/Boleyn v1.2.xlsm",
                      sheet="I- Decimal to Fractional", range="B2:C110")
simresultssummary<-simresultssummary %>% 
  ungroup() %>% 
  group_by(stage) %>% 
  arrange(stage,-probability) %>% 
  ungroup() %>% 
  mutate(prob_amend=pmax(probability+0.025,probability*1.05),
         decodds=1/prob_amend,
         fracodds=NA)

for (z in 1:nrow(simresultssummary)) {
  simresultssummary$fracodds[z] <- fracdec$FRACTION[nrow(filter(fracdec,DECIMAL<=simresultssummary$decodds[z]))]
}

record2 <- record %>%
  unnest_wider(col="final",names_sep="") %>%
  unnest_wider(col="semi",names_sep="") %>%
  unnest_wider(col="quarter",names_sep="") %>% 
  select(starts_with("quarter"),starts_with("semi"),starts_with("final"),win)
write_csv(record2,"G:/My Drive/Real Tennis/Boleyn/Boleyn tournament simulator sim record.csv")

#Text for tournament winner and finalists
# text_win <-filter(simresultssummary,stage=="winner") %>% 
#   select(player,fracodds) %>% 
#   mutate(t_win=paste0(player," - ",fracodds)) %>% 
#   select(t_win) %>% 
#   as.list(.)
# text_final <-filter(simresultssummary,stage=="finalist") %>% 
#   select(player,fracodds) %>% 
#   mutate(t_finalist=paste0(player," - ",fracodds)) %>% 
#   select(t_finalist) %>% 
#   as.list(.)
# text_semi <-filter(simresultssummary,stage=="semifinalist") %>% 
#   select(player,fracodds) %>% 
#   mutate(t_semi=paste0(player," - ",fracodds)) %>% 
#   select(t_semi) %>% 
#   as.list(.)
# 
# "_Winner_"
# text_win
# "_Finalist_"
# text_final
# "_Semi Finalist_"
# text_semi
