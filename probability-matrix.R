# This file takes the draw and generates a long-list of match probabilities for each possible pair of players

# Import libraries
library(tidyverse)

draw <- read_csv("draw.csv")
rankings <- read_csv("2025-08-20 ATP Rankings.csv") %>%
  select(name, pts)

# Sample list of 100 names
##names_list <- paste("Name", 1:128)
##names_df <- data.frame(nm=names_list,rnk=runif(128))

# Create all combinations of two names (including reversed pairs)
#df <- expand.grid(Name1 = names_list, Name2 = names_list, stringsAsFactors = FALSE)
#df <- expand.grid(Name1 = names_df$nm, Name2 = names_df$nm, stringsAsFactors = FALSE)
playermatrix <- expand.grid(p1 = draw$nameorder, p2=draw$nameorder, stringsAsFactors = FALSE)

# Remove rows where Name1 == Name2 (no self-pairs)
##df <- df[df$Name1 != df$Name2, ]
playermatrix <- playermatrix[playermatrix$p1 != playermatrix$p2, ]

# Remove duplicate permutations (e.g., A-B and B-A) (except actually, we want to keep them in)
##df_unique <- df[!duplicated(t(apply(df, 1, sort))), ]
##df <- playermatrix[!duplicated(t(apply(playermatrix, 1, sort))), ]
df <- playermatrix

# join the ranking data into it
df <- left_join(df,rankings,by=c("p1"="name")) %>%
	rename(p1rank=pts) %>%
	left_join(.,rankings,by=c("p2"="name")) %>%
	rename(p2rank=pts)
df <- mutate(df,p1win=p1rank/(p1rank+p2rank))

write_csv(df,"probability_matrix_mens.csv")
