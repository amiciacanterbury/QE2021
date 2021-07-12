# Author: Amicia Canterbury
# Date: 8 July 2021
# Quantitative Ecology
# Topic 8: Principle Component analysis (PCA)

# Load packages:

library(tidyverse)
library(vegan) # Contains all ordination methods

# Load data: - Using Doubs River Data (env)
env <-  read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
  
env <- dplyr::select(env, -1) # Removes the first column (nrs)
head(env)# Looks at the top half of the data

#----------------------------DO THE PCA-----------------------------------------

env_pca <- rda(env, scale = TRUE) # PCA automatically standardizes the data 
                                  # when scale = TRUE
env_pca

# Inertia = variable.
# In PCA, use correlation matrix.
# Inertia = sum of diagonal values of the matrix (number of variables).
# Unconstrained = not influenced by environmental variables.
# PCA is ALWAYS unconstrained.
# Eigenvalues for unconstrained axes = relative importance of the resultant 
# reduced axes. Used for the proportion of of total inertia. 
# Eigenvalue 1 = PC1 = the highest variation value.
# Remaining PC# explains the largest proportion of the remaining variance. 
# Axes = orthogonal = ranked decreasing order of importance.
# Sum of eigenvalues = total inertia.

# How to extract first eigenvalue: 
round(env_pca$CA$eig[1], 3) # Round() function rounds decimal to 3 
# 5.969


# Total inertia: 
sum(env_pca$CA$eig)
# 11

# So the proportion of variation explained by the first PC is:
round(env_pca$CA$eig[1] / sum(env_pca$CA$eig) * 100, 1) # result in %
# 54.3 %

# New method of showing the data:
summary(env_pca)

# Notes:
# - Sp. scores = loadings that show the strength of the contribution of the old
#   env. variables to the new variables(PC#s). 
# - The larger/more positive and smaller/more negative values, the more 
#   contribution albeit in both directions. 
# - Sp. scores are represented by arrows on ordination diagrams.
# - Longer vectors have more influence (also known as stronger drivers) on the
#   environmental (& possibly species) differences between sites, and their 
#   direction indicates along the PC axes they influence the greatest. 

# - Site scores = scaled & rotated coordinates of the objects.
# - Used to plot the position of the sites in 2D or 3D space.
# - Site spread further apart from others in this space & differ much i.t.o
#   env. variables. 
# - How much they spread apart depends on the major env. gradients indicated
#   by species scores. 

# - Scaling 1 and Scaling 2 (depending on what one calls on using the rda()), 
#   are useful when interpreting species(Scaling 1) or variables (Scaling 2).
# - Scaling 1 = the distances between points plotted on the ordination diagram
#   will retain their Euclidian distances, allowing for better interpretation
#   of how sites relate to one another. 
# - Scaling 2 = preserves more accurately the angles between variables with the 
#   consequence that in the biplot smaller angles between variables will reflect
#   stronger correlations. 

#-------------------------GRAPHICAL REPRENSATION------------------------------

# Called ordination diagrams.
# Use the biplot() function.

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Now we create biplots using the cleanplot.pca() function that comes with 
# Numerical Ecology in R book:

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")

cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)

# We can plot the underlying environmental gradients using the ordisurf() 
# function in vegan. 
# We plot the response surfaces for altitude and biological oxygen demand:

biplot(env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)

#------------------------------QUESTIONS: --------------------------------------

# PART A:

# Question 1: Why can a PCA, or any ordination for that matter, not explain all 
# of the variation in a dataset? In other words, why is it best to only use the 
# first few Principal Components for insight into the drivers of variability? 
# What is 'explained' by the remaining PC axes?

# Ordination extracts only the main trends in the form of continuous axes. 
# Any form of ordination cannot explain variation of the dataset because it is 
# unimodal. Every single detail that can be seen in the raw dataset cannot
# be represented as it can only show 3 orthogonal planes. Due to the other 
# planes not being represented correctly, the value of the planes are much less
# making them less important. It is impossible to visualise multiple dimensions
# at the same time. 'Low dimensional space' will represent the important and 
# interpretable environmental gradients. 

# PART B: 

# Question 1:

# (A) Alpine plant communities in Aravo, France: 

library(readr)
aravo_env <- read_csv("Quantitative_Ecology-main/aravo_env.csv") %>% 
  select(-X1, -ZoogD) # Descriptive variables must be taken out

dim(aravo_env) # Read dimensions
# 75 rows & 5 columns
head(aravo_env)

aravo_pca <- rda(aravo_env, scale = TRUE)
aravo_pca

summary(aravo_pca)

cleanplot.pca(aravo_pca, scaling = 1)
cleanplot.pca(aravo_pca, scaling = 2)

# (B) Bird communities along elevation gradient in Yushan Mountain, Taiwan:

bird_data<- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt"

bird_env <- read.delim(url(bird_data)) %>%
  select(-`Veg.`, -Station, -Veg_ext) # remove the descriptive variables

head(bird_env)
dim(bird_env) # Dimensions of the data 
# 50 rows, 18 columns

bird_pca <- rda(bird_env, scale = TRUE)
bird_pca

summary(bird_pca)

cleanplot.pca(bird_pca, scaling = 1)
cleanplot.pca(bird_pca, scaling = 2)


# Question 2: 
# (A) Explain the ordination diagram with particular reference to the major patterns 
# shown:

# ALPINE DATA: 

# Aspect =  Relative south aspect (opposite of the sine of aspect with flat coded 0)
# Slope  = Slope inclination (°)
# Form 	 = Microtopographic landform index [1 (convexity); 2 (convex slope); 3 (right slope); 4 (concave slope); 5 (concavity)]
# Snow 	 = Mean snowmelt date (Julian day) averaged over 1997-1999
# PhysD  = Physical disturbance, i.e., percentage of unvegetated soil due to physical processes

# The highest values in scaling biplot 1 were present in the sites found in the top
# of the plot. These variables were slope, aspect and snow. The group of sites
# do not include sites 15, 46 and 74. The sites mentioned above has a high value 
# for PhysD and lowest in the form. This group includes sites 66 and 72. In the bottom
# part of the biplot, there were high values for snow and slope. 

# Scaling biplot 2 shows that slope and snow has a strong negative correlation. 
# Similar evidence shows that form and PhysD. Aspect has the shortest arrow which
# shows that this variable has the least variation when looking at the ordination.


# (B) Provide a mechanistic explanation for the existence of the patterns seen with 
# respect to elevation/altitude: 

# Correlations are present between variables but no patterns can be observed.

# (A) Explain the ordination diagram with particular reference to the major patterns 
# shown:

# BIRD DATA:

# The proportion of variation is 40.47%. In the scaling biplot 1, there is a 
# gradient that runs from left to right. Sites 1-10, 12-16 and 22-23 make up the
# first group of sites. This group of sites has the lowest values for the following
# variables: 
# - EXP: Exposure: 0 - valley, 2 - lower slope, 4 - middle slope, 6 - upper slope, 8 - ridge, 10 - peak
# - SLP: Slope
# - ELE: Elevation [m a.s.l.]
# - CP: Conifer percentage
# The highest values of the variables are as follows:
# - TSD: Tree species diversity 
# - TD:  Tree density
# - T2c: Secondary tree cover
# - ASP: Aspect (norhteness): 1-8; 1 - southwest (warmest), 8 - northeast (coldest)
# - HC: Herb cover
#
# Another group is comprised of the sites 14, 17 - 21 and 24 - 31. This group 
# represents the highest values for TFV, CH, T1C, FHD, SDDB and TBA. The lowest 
# values that were found in this group were for SC and GC.
# In general, there was a change in the dominated species of the landscape.
# Initially the area was tree dominated but then went to being shrub dominated as 
# there was an increase in the elevation of the area.

# Scaling biplot 2 shows the correlations between variables. Variables with strong
# correlations include: TD, T2C, TSD, ASP and HC. This group of variables shows the 
# strong positive correlations. The strongly-negative correlations are made up of 
# the variables EXP, CP, SLP and ELE. 
# (B) Bird communities along elevation gradient in Yushan Mountain, Taiwan: 

# (B) Provide a mechanistic explanation for the existence of the patterns seen with 
# respect to elevation/altitude: 

# Due to the change in the environment from being dominantly trees to being
# a shrub-dominated area in short distances over the landscape may be due to the 
# presence of an elevation gradient. As elevation changes in an area, other variables
# may be affected. Variables such as temperature and species composition may be 
# changed over the shorter distances. As the temperature drops, there will be 
# a big change in the species composition as there will be less variation present.

# (C) If there are significant positive or negative correlations between the 
# environmental variables, provide mechanistic reasons for how they came about.

# The first group of variables has a negative correlation with the second group 
# variables mentioned in A. As the elevation increases, there will be a steeper 
# incline in the area. As the slope increases, the density of tree species diversity
# will decreases due to the colder environment present higher up. 


