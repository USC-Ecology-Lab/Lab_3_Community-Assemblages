###
# Analyzing the tree data ###
###


# load in the data 


# individual regressions of RIV by tree species:

# a single way
# The issue here is we hardly have enough trees for a decent dataset:
Loblolly_riv <- tree_sp_riv_5m |> 
  filter(tree_id == 'Loblolly')

loblolly_lm <- lm(riv ~ bin, data = Loblolly_riv)
summary(loblolly_lm)

plot(riv~bin, data = Loblolly_riv)
abline(loblolly_lm)

# this is really bad because we just don't have enough data

# What if we did it by category and looked at our most abundant category?
hardwood_riv <- tree_cat_riv_5m |> 
  filter(Tree_cat == "Hardwood")

hardwood_lm <- lm(riv ~ bin, data = hardwood_riv)
summary(hardwood_lm)
plot(riv ~ bin, data = hardwood_riv)
abline(hardwood_lm)

# is there any case for a regression?
# DBH with transect_loc?

dbh_mod <- lm(DBH ~ Transect_Loc, data = tree_raw)
summary(dbh_mod)

plot(DBH ~ Transect_Loc, data = tree_raw)
abline(dbh_mod)



# linear models don't seem to work:
# let's try an ANOVA:
hardwood_subregion <- tree_cat_riv_subregion |> 
  filter(Tree_cat == 'Hardwood')

aov(riv ~ Subregion + Tree_cat + Subregion*Tree_cat, data = tree_cat_riv_subregion) |> 
  summary()
