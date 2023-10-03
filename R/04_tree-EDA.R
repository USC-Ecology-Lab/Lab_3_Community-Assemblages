###
# Analyzing the Invert Data
###


invert <- read.csv('./data/inverts.csv')
library(dplyr)

###
# Kruskall wallace comparing the regions
####

acari_abund <- invert |> 
  filter(Spp == 'Acari')

kruskal.test(Num ~ Region, data = acari_abund)

hymenoptera_abund <- invert |> 
  filter(Spp == 'Hymenoptera')

kruskal.test(Num ~ Region, data = hymenoptera_abund)
kruskal.test(Num ~ Sampling_tree, data = hymenoptera_abund)
aov(Num ~ Region + Sampling_tree, data = hymenoptera_abund) |> 
  summary()

###
# nmds
###

library(vegan)
library(tidyr)
library(ggplot2)
invert_comm <- invert |> 
  pivot_wider(names_from = Spp,
              values_from = Num)
set.seed(1000)
invert_nmds=metaMDS(invert_comm[,4:ncol(invert_comm)], # Our community-by-species matrix
                     k=2) # The number of reduced dimensions


plot_nmds <- cbind(invert_comm[,1:3], invert_nmds$points)

ggplot() +
  geom_point(data = plot_nmds,
             aes(x = MDS1, y = MDS2, color = Sampling_tree,
                 shape = Region),
             size = 5)+
  geom_label(data = as.data.frame(invert_nmds$species), 
             aes(x = MDS1, y = MDS2, label = rownames(as.data.frame(invert_nmds$species))))+
  theme_minimal()
