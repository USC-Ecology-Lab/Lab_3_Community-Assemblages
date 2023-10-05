##
# Processing tree data into multiple formats ##########
##

# note to future instructor - I did this on a midly concussed brain may be off

library(dplyr)

tree_raw <- read.csv('./data/tree_raw.csv')

tree_raw$Subregion <- tolower(tree_raw$Subregion)
tree_raw$Tree_id <- tolower(tree_raw$Tree_id)
tree_raw$Tree_cat <- tolower(tree_raw$Tree_cat)

write.csv(tree_raw, './data/tree_raw.csv', row.names = F)

# calculate Relative abundance value ##########

tree_raw$area <- pi * (tree_raw$DBH/2)^2 # calculate area


transect_bins <- cut(tree_raw$Transect_Loc,
                             breaks = c(0,5,10,15,20,25,30,35))

lower_lim <- as.numeric(gsub('\\((.*)\\,.*','\\1',transect_bins))

tree_raw$bin <- lower_lim

riv_calc <- function(area) {
  riv <- area / sum(area) 
  return(riv)
}

# |- By 5m streches & species ------------------

tree_sp_riv_5m <- tree_raw |> 
  group_by(transect_id, bin, Tree_id) |>
  summarize(total_area = sum(area)) |> 
  summarize(riv = riv_calc(total_area),
            tree_id = Tree_id)

temp_df <- data.frame(
  transect_id = rep(rep(1:3, each = length(seq(0,35,5))), each = length(unique(tree_sp_riv_5m$tree_id))),
  bin = rep(rep(seq(0,35,5), times = 3), each = length(unique(tree_sp_riv_5m$tree_id)))
)
temp_df$tree_id <- rep(unique(tree_sp_riv_5m$tree_id), times = length(seq(0,35,5)) * 3)

tree_sp_riv_5m <- temp_df |> 
  left_join(tree_sp_riv_5m)

tree_sp_riv_5m$riv[is.na(tree_sp_riv_5m$riv)] <- 0

write.csv(tree_sp_riv_5m, './data/tree_sp_riv_5m.csv',
          row.names = F)

# |- By 5m stretches & groups ------------------
tree_cat_riv_5m <- tree_raw |> 
  group_by(transect_id, bin, Tree_cat) |>
  summarize(total_area = sum(area)) |> 
  summarize(riv = riv_calc(total_area),
            Tree_cat = Tree_cat)

temp_df <- data.frame(
  transect_id = rep(rep(1:3, each = length(seq(0,35,5))), each = length(unique(tree_cat_riv_5m$Tree_cat))),
  bin = rep(rep(seq(0,35,5), times = 3), each = length(unique(tree_cat_riv_5m$Tree_cat)))
)
temp_df$Tree_cat <- rep(unique(tree_cat_riv_5m$Tree_cat), times = length(seq(0,35,5)) * 3)

tree_cat_riv_5m <- temp_df |> 
  left_join(tree_cat_riv_5m)

tree_cat_riv_5m$riv[is.na(tree_cat_riv_5m$riv)] <- 0



write.csv(tree_cat_riv_5m, './data/tree_cat_5m.csv',
          row.names = F)

# |- By id & subregion -------------

tree_sp_riv_subregion <- tree_raw |> 
  group_by(transect_id, Subregion, Tree_id) |>
  summarize(total_area = sum(area)) |> 
  summarize(riv = riv_calc(total_area),
            tree_id = Tree_id)


temp_df <- data.frame(
  transect_id = rep(rep(1:3, each = length(unique(tree_sp_riv_subregion$Subregion))),
                    each = length(unique(tree_sp_riv_subregion$tree_id))),
  Subregion = rep(rep(unique(tree_sp_riv_subregion$Subregion), times = 3), 
            each = length(unique(tree_sp_riv_subregion$tree_id)))
)
temp_df$tree_id <- rep(unique(tree_sp_riv_subregion$tree_id), times = 9)

tree_sp_riv_subregion <- temp_df |> 
  left_join(tree_sp_riv_subregion)

tree_sp_riv_subregion$riv[is.na(tree_sp_riv_subregion$riv)] <- 0


write.csv(tree_sp_riv_subregion, './data/tree_sp_subregion.csv',
          row.names = F)

# |- By category & subreion ------------
tree_cat_riv_subregion <- tree_raw |> 
  group_by(transect_id, Subregion, Tree_cat) |>
  summarize(total_area = sum(area)) |> 
  summarize(riv = riv_calc(total_area),
            Tree_cat = Tree_cat)


temp_df <- data.frame(
  transect_id = rep(rep(1:3, each = length(unique(tree_cat_riv_subregion$Subregion))),
                    each = length(unique(tree_cat_riv_subregion$Tree_cat))),
  Subregion = rep(rep(unique(tree_cat_riv_subregion$Subregion), times = 3), 
                  each = length(unique(tree_cat_riv_subregion$Tree_cat)))
)
temp_df$Tree_cat <- rep(unique(tree_cat_riv_subregion$Tree_cat), times = 9)

tree_cat_riv_subregion <- temp_df |> 
  left_join(tree_cat_riv_subregion)

tree_cat_riv_subregion$riv[is.na(tree_cat_riv_subregion$riv)] <- 0


write.csv(tree_cat_riv_subregion, './data/tree_cat_subregion.csv',
          row.names = F)
