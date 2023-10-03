###
# PRocessing for Litter Invert Data
###

library(dplyr)
library(tidyr)

inverts <- read.csv('./data/raw-litter-invert-data.csv')

## Add 0's for all litter inverst
all_sites <- unique(inverts$Tag)
all_spp <- unique(inverts$Spp)

full_combinations <- expand.grid(Spp = all_spp, Tag = all_sites)

result_df <- full_combinations |> 
  left_join(inverts, by = c("Spp", "Tag"))


for(Tag in unique(result_df$Tag)) {
  idx = which(result_df$Tag == Tag)
  result_df[idx, ]$Sampling_tree <- unique(result_df[idx, ]$Sampling_tree[which(!is.na(result_df[idx, ]$Sampling_tree))])
  result_df[idx, ]$Region <- unique(result_df[idx, ]$Region[which(!is.na(result_df[idx, ]$Region))])
  result_df[idx,]$Num[which(is.na(result_df[idx,]$Num))] <- 0
}

result_df <- result_df |> 
  group_by(Spp, Tag, Sampling_tree, Region) |> 
  summarize(Num = sum(Num))

write.csv(result_df, './data/inverts.csv',
          row.names = F)
