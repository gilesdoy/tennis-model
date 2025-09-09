install.packages("tidyverse")
library(tidyverse)
draw<-tibble(id=1:128,p1=NA,p2=NA,winner=200:327,complete=0)
draw <- read_csv("draw_short.csv")
prob_matrix <- read_csv("probability_matrix_mens.csv")

totalsimulations=10000
starttime=Sys.time()

for (i in 1:totalsimulations) {
  
  # Round of 128 ----
  for (j in 1:64) {
    if (draw$complete[j]==0){
      paste("Not done")
    }
  }
  
  r128_vec <- draw$winner[1:64]
  mat <- matrix(r128_vec, ncol = 2, byrow = TRUE)
  df2 <- data.frame(mat) 
  draw[65:96, c("p1", "p2")] <- df2  
  
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
