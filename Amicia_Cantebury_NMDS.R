# Author: Amicia Canterbury
# Date: 20 July 2021
# Quantitative Ecology
# Topic 11: nMDS Questions 

# Questions---------------------------------------------------------------------

library(tidyverse)
library(vegan)
# Question 1: Load the data - MITE data in {vegan}:-----------------------------

# Species data: 
data(mite) # Load the dataset
head(mite) # Views the first 6 rows and columns on the dataset
dim(mite) # Looks at the dimensions of the data (70 rows & 35 columns)

# Environmental data:
data("mite.env") # Load the dataset
head(mite.env) # View the first 6 rows and columns of the dataset.
glimpse(mite.env) # Gives a summary of what to expect in the dataset.
dim(mite.env) # Looks at the dimensions of the data (70 rows & 5 columns)

#-------------------------------------------------------------------------------
# Principle Coordinate Analysis: 

# Work out the dissimilarity matrix - Bray-Curtis:

spe_bray <- vegdist(mite)

spe_bray # To view the dissimilarity matrix

# Work out the PCoA:

# spe_pcoa <- cmdscale(spe_bray, k = nrow(spe) - 1, eig = TRUE)
spe_pcoa <- capscale(spe_bray ~ 1)
spe_pcoa

summary(spe_pcoa)

spe_pcoa <- capscale(mite ~ 1, distance = "bray")
spe_pcoa

# This method shows the species scores 

# To see what is inside the results....
str(spe_pcoa)

# The percentage inertia explained by the first three axes is therefore:

round(sum(spe_pcoa$CA$eig[1:3]) / sum(spe_pcoa$CA$eig) * 100, 2)
# 54.77% 

# Ordination diagrams: 

plot(spe_pcoa, scaling = 1, main = "PCoA Oribatid mites - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA Oribatid mites - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(spe_pcoa ~ RARD, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "RARD"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(spe_pcoa ~ Miniglmn, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Miniglmn"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(spe_pcoa ~ Protopl, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Protopl"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(spe_pcoa ~ Trimalc2, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)

spe_pcoa_env <- envfit(spe_pcoa, mite.env, scaling = 2)
plot(spe_pcoa_env, p.max = 0.05, col = "red")

# Discussion: 

# Principle Component Analysis (PCoA) is similar to that of PCA as it preserves 
# Euclidian distances between the objects. It uses a dissimilarity matrix as an input.
# The PCoA does not use the raw data as PCA and CA would, which may affect the results 
# because one cannot see the species scores clearly to see which Species has the greatest 
# effect on the variation in the environment. The environmental variables found 
# within the dune data set is all clustered together around the origin. This alludes 
# to the origin being the area that may have the most variation. All the sites are also 
# clustered around this area, meaning the species are co-exisiting with all the different
# environmental factors. 

# The species with the highest species scores are represented in one diagram. They include 
# RARD, Miniglm, Protopl and Trimalc2. RARD has circular contours with larger bubbles in 
# the middle. It is similar for all the plots. Due to the environmental variables all 
# being clustered in the middle, it is unclear to see the data clearly. 


#-------------------------------------------------------------------------------
# Principle Component Analysis (PCA):

# Remove columns with qualitative variables: 
mite_env <- dplyr::select(mite.env, -Substrate, -Shrub, -Topo)
head(mite_env) # View the first 6 rows 

# Work out the PCA: 

env_pca <- rda(mite_env, scale = TRUE)
env_pca

summary(env_pca) # Views more detailed summary 

# Inertia: 
sum(env_pca$CA$eig)

# Plots: 

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Plots using cleanplot(): 

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")

cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)

# Discussion: 

# PCA is a method of unconstrained ordination that focuses on the eigen-decomposition 
# of a distance or association matrix. The total inertia that is found in the PCA 
# is comprised of mainly the first two eigenvalues as they are the ones that carry 
# the most weight, whereas the others are minimal. Due to the low values that are presented
# by the species scores, one can allude to the fact that the environmental factors, 
# water content and substrate density will have an effect on the variation that is calculated
# when using the PCA method. Both of the environmental factors are in opposite directions 
# to one another, meaning that the factors will not affect the next. 
# When looking at PCA biplot 1 - Scaling 1, both environmental factors are within the 
# circle of equilibrium , meaning that the variables will not affect the next when looking
# at the variation across the sites. 
# Sites including 67, 44 and 32 will experience higher levels of water concentration within 
# the substrate. Sites that include 8, 9, 2 and 5 will experience higher levels of substrate density.
# When sites are clustered together, one can assume that the environment is of a stable nature. 
# The species that live on these sites can co-exist together, which in turn may cause
# low variation amongst the species found in sites that cluster around the origin. 
# A positive correlation is present here within the PCA. 


#-------------------------------------------------------------------------------

# Question 2: Load the data - DUNE data in {vegan}:-----------------------------

# Species data: 
data("dune") # Load the dataset.
head(dune) # Views the first 6 rows and columns of the dataset.
dim(dune) # Looks at the dimensions of the data (20 rows & 30 columns)

# Environmental data: 
data("dune.env") # Load the dataset
head(dune.env) # View the first 6 rows and columns of the dataset
glimpse(dune.env) # Gives a summary of what to expect in the dataset.
dim(dune.env) # Looks at the dimensions of the data ( 20 rows & 5 columns)

#-------------------------------------------------------------------------------

# Non-metric Dimensional Scaling (nMDS):

dune_nmds <- metaMDS(dune, distance = "bray")

nmds <- metaMDS(dune, metric = "gower")

nmds

summary(nmds)

# Plots:

par(mfrow = c(2, 2))
stressplot(dune_nmds, main = "Shepard plot")
ordiplot(dune_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(dune_nmds$stress, 2)))
gof = goodness(dune_nmds)
plot(dune_nmds, type = "t", main = "Goodness of fit")
points(dune_nmds, display = "sites", cex = gof * 200)

pl <- ordiplot(dune_nmds,type = "none", main = "nMDS Oribatid mite abundance")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

# Ordination diagrams: 

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(dune_nmds ~ Callcusp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds  ~Comapalu , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Comapalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Eleopalu , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Eleopalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Ranuflam , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Ranuflam"))
abline(h = 0, v = 0, lty = 3)

# Add the environmental variables: 

(spe_nmds_env <- envfit(dune_nmds, dune.env)) 
plot(spe_nmds_env, p.max = 0.05, col = "red")

# Discussion: 

# Non-metric dimensional scaling is similar in the sense of PCoA as it produces an
# ordination of objects from any distance or dissimilarity matrix. 
# Exact distances is not kept within this method as it tries to represent the ordering 
# relationships among objects in a small and specific number of axes. 
# Due to the nMDS using small and specific number of axes, stress becomes one of the 
# ways in which one can determine whether the data is reliable or not. 
# In the case of the nMDS using the dune data, the stress 0.12. Species 
# or sites are clustered together within the second plot. The stress also represents the 
# scatter within the dissimilarity matrix. 
# The goodness-of-fit of the ordination is measured as the $R^{2}$ of either a 
# linear or a non-linear regression of the nMDS distances on the original ones.
# Within in this dataset, the bubbles are overlapping and large within the goodness 
# of fit plot. This relates to the fact that the sites are all clustering together near 
# the orgin. 
#-------------------------------------------------------------------------------
# Correspondence Analysis (CA): 

# We use the {vegan} package to use the cca() function for the CA and for the 
# Constrained correspondence analysis (CCA):

spe_ca <- cca(dune) # Finds the CA
spe_ca # Views the data in the console 

# Do the PCA with the new data matrix:

spe_ca <- cca(dune)
spe_ca

summary(spe_ca)

# Calculate the total inertia manually: 

round(sum(spe_ca$CA$eig), 5)

# 2.11526

# The inertia for the first axis (CA1) is:

round(spe_ca$CA$eig[1], 5)

# CA1 = 0.53601

# The inertia of CA1 and CA2 is:

round(sum(spe_ca$CA$eig[1:2]), 5)

# 0.93615

# The fraction of the variance explained by CA1 and CA2 is:

round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2) # result in %

# 44.26%

# Ordination diagrams: 

plot(spe_ca, scaling = 1, main = "PCA Dune abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "PCA Dune abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(spe_ca ~ Callcusp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(spe_ca ~ Comapalu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Comapalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(spe_ca ~ Eleopalu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Eleopalu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(spe_ca ~ Ranuflam, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Ranuflam"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv(dune.env)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a 
# different colour

# Discussion: 
