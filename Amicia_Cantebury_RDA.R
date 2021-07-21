# # Author: Amicia Canterbury
# Date: 20 July 2021
# Quantitative Ecology
# Topic 12: RDA - Practice 

# Notes-------------------------------------------------------------------------
# - Going to be using a distance-based redundancy analysis (db-RDA) found in 
#   {vegan} capscale(). 
# - "Distance-based redundancy analysis (dbRDA) is an ordination method similar
#   to Redundancy Analysis (rda), but it allows non-Euclidean dissimilarity indices, 
#   such as Manhattan or Bray-Curtis distance."

# Going to use the seaweed data from Smit et al., 2017

# GITHUB example----------------------------------------------------------------

# Load necessary data packages:
library(tidyverse) # To filter etc 
library(betapart) # Used to work out the nested-resultant and turnover 
library(vegan) # Used for the ordinations & environmental changes 
library(gridExtra) # To draw tables & arrange grid-based plots on page 
library(grid) # Helps with the appearance & arrangement of the graphs 
library(gridBase) # Combining grid and base graphics output.
library(tidyr) # To help tidy the datasets

# Load the data - Using species data first:

spp <- read.csv("Quantitative_Ecology-main/exercises/diversity/SeaweedsSpp.csv")
spp <- dplyr::select(spp, -1) # Filter out the first column 
dim(spp) # Checks the dimensions of the dataset 

# 58 rows, 847 columns

# First worked out Sorenson dissimilarity then work out the nestedness-resultant 
# and species turnover using betacore() and betapart.pair() in {betapart}. 

Y.core <- betapart.core(spp) 
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

# Load the environmental data:

load("Quantitative_Ecology-main/exercises/diversity/SeaweedEnv.RData") # Use this function if data is on laptop 
dim(env) # Looks at the dimensions of the dataset 

# 58 rows and 18 columns

# Only selecting certain thermal variables:

E1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                    augRange, augSD, annMean, annRange, annSD)
# Calculate the z-scores: 
E1 <- decostand(E1, method = "standardize")

# Load in the bioregion data: 

bioreg <- read.csv("Quantitative_Ecology-main/exercises/diversity/bioregions.csv")

head(bioreg)

# BMP: Benguela Marine Province (1-17)
# B-ATZ: Benguela-Agulhas Transition Zone (18-22)
# AMP: Agulhas Marine Province (19-43/44)
# ECTZ: East Coast Transition Zone (44/45-48)

# Load the geographic coordinates for the coastal sections:
  
sites <- read.csv("Quantitative_Ecology-main/exercises/diversity/sites.csv")
sites <- sites[, c(2, 1)] # Places data in 2 columns, 1 row
head(sites) # Views first top half of data 
dim(sites) # Views the dimensions of data

# 58 rows, 2 columns

# Doing the RDA:

# fit the full model:
rda_full <- capscale(Y1 ~., E1)
rda_full

# summary(rda_full)

# Is the fit significant? I run a permutation test to check:
anova(rda_full, parallel = 4) # ... yes!

#  Since the fit is significant (the environmental variables capture the variation 
#  seen in the species data), I compute the adjusted R^2:

rda_full_R2 <- RsquareAdj(rda_full)$adj.r.squared
round(rda_full_R2, 2)

# 0.9

# What is the variation explained by the full set environmental variables?
round(sum(rda_full$CCA$eig) / rda_full$tot.chi * 100, 2)

# 91.23%

vif.cca(rda_full)

# Drop annMean:
E2 <- dplyr::select(E1, -annMean)
rda_sel1 <- capscale(Y1 ~., E2)
vif.cca(rda_sel1)

# Drop febMean:
E3 <- dplyr::select(E2, -febMean)
rda_sel2 <- capscale(Y1 ~., E3)
vif.cca(rda_sel2)

rda_final <- rda_sel2
  
# is the fit significant?
anova(rda_final, parallel = 4) # ... yes!

anova(rda_final, by = "axis", parallel = 4) # ... yes!

# Extract the significant variables in $E3$ that are influential in the final 
# model as influencers of seaweed community differences amongst coastal sections:

(rda_final_axis_test <- anova(rda_final, by = "terms", parallel = 4))

# The significant variables are:

rda_final_ax <- which(rda_final_axis_test[, 4] < 0.05)
rda_final_sign_ax <- colnames(E3[,rda_final_ax])
rda_final_sign_ax

# The adjusted $R^{2}$ for the constraints:
round(rda_final_R2 <- RsquareAdj(rda_final)$adj.r.squared, 2)

# Variance explained by reduced (final) model:
round(sum(rda_final$CCA$eig) / rda_final$tot.chi * 100, 2)

# The biplot scores for constraining variables:
scores(rda_final, display = "bp", choices = c(1:2))

# Ordination diagrams: ---------------------------------------------------------
# use scaling = 1 or scaling = 2 for site and species scaling, respectively
rda_final_scrs <- scores(rda_final, display = c("sp", "wa", "lc", "bp"))
# see ?plot.cca for insight into the use of lc vs wa scores
# below I splot the wa (site) scores rather than lc (constraints) scores
site_scores <- data.frame(rda_final_scrs$site) # the wa scores
site_scores$bioreg <- bioreg$bolton
site_scores$section <- seq(1:58)
colnames(site_scores) <- c("x", "y", "Bioregion", "Section")

biplot_scores <- data.frame(rda_final_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_final_sign_ax,]

ggplot(data = site_scores, aes(x, y, colour = Bioregion)) +
  geom_point(size = 5.0, shape = 24, fill = "white") +
  geom_text(aes(label = Section), size = 3.0, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "black") +
  geom_segment(data = biplot_scores_sign,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "lightseagreen", alpha = 1, size = 0.7) +
  xlab("CAP1") + ylab("CAP2") +
  ggtitle(expression(paste("Significant thermal variables and ", beta[sim]))) +
  theme_grey() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.8)

# Dealing with factors----------------------------------------------------------
E4 <- E3
# append the bioregs after the thermal vars
E4$bioreg <- bioreg$bolton
head(E4)
rda_cat <- capscale(Y1 ~., E4)
plot(rda_cat)

# The default plot works okay and shows all necessary info, but the various pieces 
# (site, species, and centroid scores) are not clearly discernable. 
# Plot the class (factor) centroids in ggplot():

# also extractthe factor centroids for the bioregions
rda_cat_scrs <- scores(rda_cat, display = c("sp", "wa", "lc", "bp", "cn"))
site_scores <- data.frame(rda_cat_scrs$site) # the wa scores
site_scores$bioreg <- bioreg$bolton
site_scores$section <- seq(1:58)

biplot_scores <- data.frame(rda_cat_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_final_sign_ax,]

bioreg_centroids <- data.frame(rda_cat_scrs$centroids)
bioreg_centroids$labels <- rownames(bioreg_centroids)

ggplot(data = site_scores, aes(CAP1, CAP2, colour = bioreg)) +
  geom_point(size = 5.0, shape = 24, fill = "white") +
  geom_text(aes(label = section), size = 3.0, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "black") +
  geom_segment(data = biplot_scores_sign,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "lightseagreen", alpha = 1, size = 0.7) +
  geom_label(data = bioreg_centroids,
             aes(x = CAP1, y = CAP2,
                 label = labels), size = 4.0,
             col = "black", fill = "yellow") +
  xlab("CAP1") + ylab("CAP2") +
  ggtitle(expression(paste("Significant thermal variables and ", beta[sim]))) +
  theme_grey() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.8)
