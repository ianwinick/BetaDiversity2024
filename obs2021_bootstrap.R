library(tidyverse)
library(vegan)
library(FD)

U_2021 <- read_csv("U_2021_bootstrap.csv")
L_2021 <- read_csv("L_2021_bootstrap.csv")
H_2021 <- read_csv("H_2021_bootstrap.csv")

traits <- read_csv("traitMatrix.csv") %>%
  column_to_rownames(var="spp")

################################################################################
# Unburned #####################################################################
################################################################################
obsGamma = NULL
obsAlpha = NULL
obsBeta = NULL
Q_obsGamma <- NULL
Q_obsAlpha <- NULL
Q_obsBeta <- NULL

for(i in 1:999){
  
  data <- select(filter(U_2021, boot==i), -boot)
  data <- data[, colSums(data != 0) > 0]
  data <- data[rowSums(data != 0) > 0,]
  data <- wisconsin(data)
  
  traitMatrix <- traits %>%
    filter(rownames(.) %in% colnames(data))
  
  obsGamma[i] = diversity(colMeans(data), "shannon")
  obsAlpha[i] = mean(diversity(data, "shannon"))
  obsBeta[i] = mean(vegdist(data))
  Q_obsGamma[i] = dbFD(traitMatrix, colMeans(data), scale.RaoQ=T, message=F)$RaoQ
  Q_obsAlpha[i] = mean(dbFD(traitMatrix, data, scale.RaoQ=T, message=F)$RaoQ)
  Q_obsBeta[i] = Q_obsGamma[i]-Q_obsAlpha[i]
}

U_2021_obs <- data.frame("boot"=1:999, "year"=2021, "severity"="U", obsGamma, obsAlpha, obsBeta, Q_obsGamma, Q_obsAlpha, Q_obsBeta)

################################################################################
# Low Severity #################################################################
################################################################################
obsGamma = NULL
obsAlpha = NULL
obsBeta = NULL
Q_obsGamma <- NULL
Q_obsAlpha <- NULL
Q_obsBeta <- NULL

for(i in 1:999){
  
  data <- select(filter(L_2021, boot==i), -boot)
  data <- data[, colSums(data != 0) > 0]
  data <- data[rowSums(data != 0) > 0,]
  data <- wisconsin(data)
  
  traitMatrix <- traits %>%
    filter(rownames(.) %in% colnames(data))
  
  obsGamma[i] = diversity(colMeans(data), "shannon")
  obsAlpha[i] = mean(diversity(data, "shannon"))
  obsBeta[i] = mean(vegdist(data))
  Q_obsGamma[i] = dbFD(traitMatrix, colMeans(data), scale.RaoQ=T, message=F)$RaoQ
  Q_obsAlpha[i] = mean(dbFD(traitMatrix, data, scale.RaoQ=T, message=F)$RaoQ)
  Q_obsBeta[i] = Q_obsGamma[i]-Q_obsAlpha[i]
}

L_2021_obs <- data.frame("boot"=1:999, "year"=2021, "severity"="L", obsGamma, obsAlpha, obsBeta, Q_obsGamma, Q_obsAlpha, Q_obsBeta)

################################################################################
# High Severity ################################################################
################################################################################
obsGamma = NULL
obsAlpha = NULL
obsBeta = NULL
Q_obsGamma <- NULL
Q_obsAlpha <- NULL
Q_obsBeta <- NULL

for(i in 1:999){
  
  data <- select(filter(H_2021, boot==i), -boot)
  data <- data[, colSums(data != 0) > 0]
  data <- data[rowSums(data != 0) > 0,]
  data <- wisconsin(data)
  
  traitMatrix <- traits %>%
    filter(rownames(.) %in% colnames(data))
  
  obsGamma[i] = diversity(colMeans(data), "shannon")
  obsAlpha[i] = mean(diversity(data, "shannon"))
  obsBeta[i] = mean(vegdist(data))
  Q_obsGamma[i] = dbFD(traitMatrix, colMeans(data), scale.RaoQ=T, message=F)$RaoQ
  Q_obsAlpha[i] = mean(dbFD(traitMatrix, data, scale.RaoQ=T, message=F)$RaoQ)
  Q_obsBeta[i] = Q_obsGamma[i]-Q_obsAlpha[i]
}

H_2021_obs <- data.frame("boot"=1:999, "year"=2021, "severity"="H", obsGamma, obsAlpha, obsBeta, Q_obsGamma, Q_obsAlpha, Q_obsBeta)

################################################################################
# All together now #############################################################
################################################################################

obs2021_boot <- rbind(U_2021_obs, L_2021_obs, H_2021_obs)

write.csv(obs2021_boot, "obs2021_boot.csv")
