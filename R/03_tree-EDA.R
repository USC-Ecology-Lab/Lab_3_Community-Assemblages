###
# Analyzing the tree data ###
###
library(dplyr)
library(ggplot2)

# load in the data 
tree_sp_riv_5m <- read.csv('./data/tree_sp_riv_5m.csv')
tree_cat_riv_5m <- read.csv('./data/tree_cat_5m.csv')
tree_cat_riv_subregion <- read.csv('./data/tree_cat_subregion.csv')
tree_raw <- read.csv('./data/tree_raw.csv')


###
# individual regressions of RIV by tree species #########
###

# Loop analysis:
tree_sp <- list()
for(tree in unique(tree_sp_riv_5m$tree_id)) {
  tree_sp[[tree]] <- tree_sp_riv_5m |> 
    filter(tree_id == tree)
}

tree_5m_reg_mod <- list()
for(tree in names(tree_sp)) {
  tree_5m_reg_mod[[tree]] <- lm(riv ~ bin, data = tree_sp[[tree]])
  print(tree)
  print(summary(tree_5m_reg_mod[[tree]]))
}

for(tree in names(tree_sp)) {
  windows()
  try({
    plot(riv ~ bin, tree_sp[[tree]],
         main = tree)
    abline(tree_5m_reg_mod[[tree]])
  })
}


###
# For a categories #########
###

# Loop analysis:
tree_cat <- list()
for(cat in unique(tree_cat_riv_5m$Tree_cat)) {
  tree_cat[[cat]] <- tree_cat_riv_5m |> 
    filter(Tree_cat == cat)
}

tree_cat_reg_mod <- list()
for(cat in names(tree_cat)) {
  tree_cat_reg_mod[[cat]] <- lm(riv ~ bin, data = tree_cat[[cat]])
  print(cat)
  print(summary(tree_cat_reg_mod[[cat]]))
}

for(cat in names(tree_cat)) {
  windows()
  try({
    plot(riv ~ bin, tree_cat[[cat]],
         main = cat)
    abline(tree_cat_reg_mod[[cat]])
  })
}

# 
# # a single way
# # The issue here is we hardly have enough trees for a decent dataset:
# Loblolly_riv <- tree_sp_riv_5m |> 
#   filter(tree_id == 'Loblolly')
# 
# loblolly_lm <- lm(riv ~ bin, data = Loblolly_riv)
# summary(loblolly_lm)
# 
# plot(riv~bin, data = Loblolly_riv)
# abline(loblolly_lm)

# this is really bad because we just don't have enough data

# is there any case for a regression?
# DBH with transect_loc?

dbh_mod <- lm(DBH ~ Transect_Loc, data = tree_raw)
summary(dbh_mod)

plot(DBH ~ Transect_Loc, data = tree_raw)
abline(dbh_mod)

ggplot(tree_raw,
       aes(x = Transect_Loc, y = DBH,
           color = Tree_id)) +
  geom_point() +
  geom_smooth(method = 'lm')+
  theme_bw()

# linear models don't seem to work:
# let's try an ANOVA:

# 
# 
# hardwood_subregion <- tree_cat_riv_subregion |> 
#   filter(Tree_cat == 'Hardwood')
# 
# aov(riv ~ Subregion, data = hardwood_subregion) |> 
#   summary()
# 
# pine_subregion <- tree_cat_riv_subregion |> 
#   filter(Tree_cat == 'Pine')
# 
# aov(riv ~ Subregion, data = pine_subregion) |> 
#   summary()
# 
# oak_sub <- tree_cat_riv_subregion |> 
#   filter(Tree_cat == 'oak')
# 
# aov(riv ~ Subregion, data = oak_sub) |> 
#   summary()
# 
# # Two-way anova on the whole system:
# aov(riv ~ Subregion + Tree_cat + Subregion*Tree_cat, data = tree_cat_riv_subregion) |> 
#   summary()


###
# Tree ANOVA ############
###

tree_cat_reg <- list()

for(cat in unique(tree_cat_riv_subregion$Tree_cat)) {
  tree_cat_reg[[cat]] <- tree_cat_riv_subregion |> 
    filter(Tree_cat == cat)
}
  
for(cat in names(tree_cat_reg)) {
  print(cat)
  kruskal.test(riv ~ Subregion, data = tree_cat_reg[[cat]]) |> 
    print()
}


ggplot(tree_cat_reg[["hardwood"]]) +
  geom_bar(aes(x = Subregion, y = riv),
           stat = 'summary')

### What about just the abundance of trees?

tree_abund <- tree_raw |> 
  group_by(Subregion, transect_id, Tree_cat) |> 
  summarize(num_trees = length(Tree_cat))


hardwood_abund <- tree_abund[which(tree_abund$Tree_cat == 'hardwood'),]
kruskal.test(hardwood_abund$num_trees ~ hardwood_abund$Subregion)

# Does the distribution of trees abundance vary by region?

for(reg in unique(tree_raw$Subregion)) {
  
}
chisq.test(x = )