# Author: Amicia Canterbury
# Date: 20 July 2021
# Quantitative Ecology
# Topic 11: nMDS 

# Notes-------------------------------------------------------------------------

# - Ranked-based indirect gradient analysis, that uses a distance or dissimilarity
#   as an input. 
# - nMDS strives to represent the pairwise dissimilarities between sites in 
#   ordination space .
# - It does not use the distances or the dissimilarities directly (INDIRECT ANALYSIS)
# - Results in a loss of insight into the magnitude of difference between site pairs,
#   but we benefit from technique being more robust and less influenced by deviations
#   from idealised data distributions.
# - nMDS is the non-metric equivalent to PCoA. 

#-----------------------WORKING THROUGH GITHUB EXAMPLE--------------------------

# Load necessary packages: 

library(tidyverse)
library(vegan)

# Load the species data of the Doubs River data:

spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Filter out the first column
spe <- dplyr::slice(spe, -8)  # Remove row 8 because = 0

spe_nmds <- metaMDS(spe, distance = "bray") # Using the Bray-Curtis dissimilarity index
spe_nmds

# summary(spe_nmds ) - NOT USEFUL LIKE WITH PCA, PCoA AND CA

# See Numerical Ecology in R (pp. 145 to 149)  - Interpretation of ordination diagrams

# Ordination diagrams: 

# New concepts are introduced: Stress, Shepard plots and goodness of fit.
# - Stress: shows the scatter of observed dissimilarities against an expected monotone
#   regression.
# - Shepard plots: plots the ordination distances against the original dissimilarities,
#   and adds monotone or linear fit line to highlight the relationship.
# - stressplot(): also produces two fit statistics. 
# - The goodness of fit: the ordination is measured as the R^2  of either linear 
#   or non-linear regression of the nMDS distances on the original ones.

par(mfrow = c(2, 2))
stressplot(spe_nmds, main = "Shepard plot")
ordiplot(spe_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))
gof = goodness(spe_nmds)
plot(spe_nmds, type = "t", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit

# ordination plots from scratch: 

# In console: par(mfrow = c(1,1))

pl <- ordiplot(spe_nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

# Using ordisurf to make ordination diagrams: 

spe_nmds

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_nmds ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)
env <- dplyr::slice(env, -8)

(spe_nmds_env <- envfit(spe_nmds, env)) 
plot(spe_nmds_env, col = "grey40")
plot(spe_nmds_env, p.max = 0.05, col = "red")