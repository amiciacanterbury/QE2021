# # Author: Amicia Canterbury
# Date: 16 July 2021
# Quantitative Ecology
# Topic 10: Principle Coordinate Analysis (PCoA) - Practice

# Notes-------------------------------------------------------------------------
# - Instead of using raw data (like CA/PCA), PCoA uses a (dis)similarity 
#   matrix as an input.
# - If dissimilarities are Euclidean distances, then PCoA = PCA
# - PCoA is more useful because (dis)similarity matrices can be calculated from
#   quantitative, semi-quantitative, qualitative and mixed variables. 
# - PCoA takes a set of dissimilarities and returns it as a set of points, so that 
#   when the points are plotted in 2D or 3D space that the distance between the 
#   points are approximately equal to the dissimilarities. 
# TRIES TO REPRESENT SPECIES DISSIMILARITIES AS EUCLIDIAN DISTANCES! 

# EXAMPLE ON GITHUB-------------------------------------------------------------

# Load packages: 

library(tidyverse)
library(vegan) # Used for the ordinations 

# Load the species data of the DOUBS river data: 

spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Filter the data (columns)
spe <- dplyr::slice(spe, -8)  # Filter the data (rows)

# Work out the dissimilarity matrix - Bray-Curtis:

spe_bray <- vegdist(spe)

spe_bray # To view the dissimilarity matrix 

# Work out the PCoA:

# Can use capscale(), cmdscale() or pcoa() 

?capscale

# spe_pcoa <- cmdscale(spe_bray, k = nrow(spe) - 1, eig = TRUE)
spe_pcoa <- capscale(spe_bray ~ 1)
spe_pcoa

summary(spe_pcoa)

# Species scores are not available because the data is in the form of a dissimilarity
# matrix and not as raw data.

spe_pcoa <- capscale(spe ~ 1, distance = "bray")
spe_pcoa

summary(spe_pcoa)

# This method shows the species scores 

# To see what is inside the results....
str(spe_pcoa)

# The percentage inertia explained by the first three axes is therefore:

round(sum(spe_pcoa$CA$eig[1:3]) / sum(spe_pcoa$CA$eig) * 100, 2)

# 77.98%

# Ordination diagrams-----------------------------------------------------------

# See page 140 -145 for the interpretation of PCoA ordination diagrams

plot(spe_pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")

# Scaling of PCoA is the same as the scaling in CA

# Building plots from scratch:

pl1 <- ordiplot(spe_pcoa, type = "none", scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(spe_pcoa, type = "none", scaling = 2, main = "PCoA fish abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_pcoa ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)
env <- dplyr::slice(env, -8)

(spe_pcoa_env <- envfit(spe_pcoa, env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red") 
