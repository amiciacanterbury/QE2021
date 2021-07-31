# Author: Amicia Canterbury
# Date: 26 July 2021
# Quantitative Ecology
# Doubs River Data Integrative Assignment

# Unconstrianed Ordination:
# Principle Component Analysis:-------------------------------------------------

# Load packages:

library(tidyverse)
library(vegan) # Contains all ordination methods

# Load data: - Using Doubs River Data (env)
env <-  read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")

env <- dplyr::select(env, -1) # Removes the first column (nrs)
head(env)# Looks at the top half of the data

# Do the PCA:
env_pca <- rda(env, scale = TRUE)

env_pca

summary(env_pca)

# Graphical representation:

par(mar = c(2, 2, 0.9, 0.5) + .1, mfrow = c(2, 2))
biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# More detailed graphical representation: (using cleanplot())

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
par(mar = c(2, 2, 0.9, 0.5) + .1, mfrow = c(2, 2))
cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)


# Correspondence Analysis:------------------------------------------------------

# Load the data - Using Doubs data but sp. data:

spe <-  read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Remove first column
head(spe, 8) # To look at the top 8 rows instead of 6

# Do the CA: 
spe_ca <- cca(spe) # Finds the CA
spe_ca # Views the data in the console 

# Remove the rows = 0
apply(spe, 1, sum)
spe <- spe[rowSums(spe) > 0, ]
head(spe, 8)

spe_ca <- cca(spe)
spe_ca

summary(spe_ca)

# Ordination diagrams:
par(mar = c(2, 2, 0.9, 0.5) + .1, mfrow = c(2, 2))
plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, p.max = 0.05, col = "red")

# nMDS:-------------------------------------------------------------------------

# Load the species data of the Doubs River data:

spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Filter out the first column
spe <- dplyr::slice(spe, -8)  # Remove row 8 because = 0

spe_nmds <- metaMDS(spe, distance = "bray") # Using the Bray-Curtis dissimilarity index
spe_nmds

# Ordination diagrams: 

par(mfrow = c(2, 2))
stressplot(spe_nmds, main = "Shepard plot")
ordiplot(spe_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))
gof = goodness(spe_nmds)
plot(spe_nmds, type = "t", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = gof * 200)

pl <- ordiplot(spe_nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

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
plot(spe_nmds_env, p.max = 0.05, col = "red")

# Principle Coordinate Analysis (PCoA):-----------------------------------------

# Load packages: 

library(tidyverse)
library(vegan) # Used for the ordinations 

# Load the species data of the DOUBS river data: 

spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # Filter the data (columns)
spe <- dplyr::slice(spe, -8)  # Filter the data (rows)

# Work out the dissimilarity matrix - Bray-Curtis:

spe_bray <- vegdist(spe)

spe_bray

# Work out PcoA:

spe_pcoa <- capscale(spe_bray ~ 1) # Does not show sp. scores
spe_pcoa


spe_pcoa <- capscale(spe ~ 1, distance = "bray") # shows sp. scores 
spe_pcoa

summary(spe_pcoa)

# Ordination diagrams: 
plot(spe_pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")

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

# Clustering Analysis:----------------------------------------------------------
# Setting up the analysis environment:
library(tidyverse) 
library(GGally)
library(cluster)
library(dendextend)
library(ggcorrplot)
library(factoextra)
library(gridExtra)
library(vegan)
library(FunCluster)

# Load the sp. data for the RDA:

spp <- read.csv(url("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")) %>%
 select(-1) %>% # first column is not needed
  slice(-8) # this row is empty
head(spp)
dim(spp)
glimpse(spp)

# Set up the data matrix: 
spp_bray <- vegdist(spp, diag = TRUE) #calculate the bray-Curtis dissimilarity for abundance data.
spp_bray

# Load in the env. data:

env <- read.csv(url("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")) %>%
  select(-X) %>%
  slice(-8) # must remove the same row that was removed in the species data.
dim(env)
head(env)

E1 <- decostand(env, method = "standardize") # standardize the variables as they have differing units of measurement.

# Do the db-RDA

rda_full <- capscale(spp_bray~., E1, distance = "bray")
summary(rda_full)

anova(rda_full, parallel = 4)

# the fit is significant.
round(rda_full_R2 <- RsquareAdj(rda_full)$adj.r.squared, 2)


# The variation explained by the full set environmental variables
round(sum(rda_full$CCA$eig) / rda_full$tot.chi * 100, 2)

vif.cca(rda_full) # check co-linearity of all env variables.

E2 <- E1 %>% select(-dfs)
rda_sel1 <- capscale(spp_bray~., E2, distance = "bray")
vif.cca(rda_sel1)

E3 <- E2 %>% select(-amm)
rda_sel2 <- capscale(spp_bray ~., E3, distance = "bray")
vif.cca(rda_sel2)

E4 <- E3 %>% select(-pho)
rda_sel3 <- capscale(spp_bray~., E4, distance = "bray")
vif.cca(rda_sel3)

rda_final <- rda_sel3

# Do the clustering: 
  corr <- round(cor(env), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)

# The number of clusters:
?fviz_nbclust
fviz_nbclust(E4, FUN = kmeans, method = c("silhouette", "wss", "gap_stat"))

E4_euc <- vegdist(E4, method = "euclidean")

E4_kmeans <- kmeans(E4, centers = 4)

par(mfrow = c(1, 1))
fviz_cluster(E4_kmeans, data = E4,
             geom = "text",
             ellipse.type = "confidence",)
