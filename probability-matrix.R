# Sample list of 100 names
names_list <- paste("Name", 1:128)

# Create all combinations of two names (including reversed pairs)
df <- expand.grid(Name1 = names_list, Name2 = names_list, stringsAsFactors = FALSE)

# Remove rows where Name1 == Name2 (no self-pairs)
df <- df[df$Name1 != df$Name2, ]

# Remove duplicate permutations (e.g., A-B and B-A)
df_unique <- df[!duplicated(t(apply(df, 1, sort))), ]

