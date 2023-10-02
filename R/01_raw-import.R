###
# Processing the litter data
###

raw_litter <- read.csv('./data/raw-litter-invert-data.csv')

# Fill in 0's for non-observations

all_taxa <- unique(raw_litter$Spp)

# add 0's for non-observed
raw_litter <- raw_litter |> 
  ad