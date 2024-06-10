library(tidyverse)
library(vegan)
library(FD)

################################################################################
## FUNCTION ONE -- NULL BETA DIVERSITY GENERATOR ###############################
################################################################################

null_beta_scaled <- function(x, traits){
  
  #Generate species list
  Gamma <- as.vector(unique(x$spp))
  
  #Start building data frame with plot numbers.
  nullModel_ <- data.frame(
    "Plot" = unique(x$plot))
  
  #Randomly sample species richness values from uniform distribution
  MaxObsRich <- x %>%
    filter(cover>0) %>%
    group_by(plot) %>%
    summarise(spp = n()) %>%
    slice_max(spp) %>%
    select(spp) %>%
    unique() %>%
    unlist()
  
  Richness = sample(1:MaxObsRich, length(nullModel_$Plot), replace=T)
  
  nullModel_ <- nullModel_ %>%
    cbind(Richness)
  
  #Add column for species and randomly sample from species list according to plot richness. The repeat loop ensures all species in species pool are included, such that gamma diversity remains constant.
  repeat{
    nullModel. <- nullModel_ %>%
      mutate(Species = NA) %>%
      group_by(Plot) %>%
      mutate(across(Species, ~list(sample(Gamma, size=Richness, replace=F)))) %>%
      unnest(Species) %>%
      ungroup()
    
    if(length(unique(nullModel.$Species)) == length(Gamma)){break} 
    else (rm(nullModel.))
  }
  
  #Create vector of random species covers from beta distribution and bind to data frame. The repeat loop ensures that no plot has a total absolute cover greater than the maximum observed plot-level cover.
  
  Cover <- as.vector(runif(n=length(nullModel.$Species), 0.2, 100))
  
  nullModel <- nullModel. %>%
    cbind("Cover"=Cover)
  
  #Convert data frame to matrix
  nullModel <- nullModel %>%
    pivot_wider(names_from=Species, values_from=Cover, values_fill=0)
  
  nullMatrix <- nullModel %>% 
    select(-Richness) %>%
    column_to_rownames(var="Plot")
  
  nullMatrix <- nullMatrix[,order(colnames(nullMatrix))]
  
  #Relativize using Wisconsin double standardization
  nullMatrix <- wisconsin(nullMatrix)
  
  #Trim trait table to match community matrix
  traitMatrix <- traits %>%
    filter(rownames(.) %in% colnames(nullMatrix))
  
  #Compute beta diversity
  gammaRao <- dbFD(traitMatrix, colMeans(nullMatrix), scale.RaoQ=T, message=F)$RaoQ
  alphaRao <- mean(dbFD(traitMatrix, nullMatrix, scale.RaoQ=T, message=F)$RaoQ)
  betaFUN <- gammaRao-alphaRao
  
  betaTAX <- mean(vegdist(nullMatrix, method="bray"))
  
  beta <- list("tax"=betaTAX, "fun"=betaFUN)
  
  return(beta)
  
}

################################################################################
# FUNCTION TWO -- ITERATE ######################################################
################################################################################

null_beta_rep <- function(x, traits, Nsim=5){
  
  tax=NULL
  fun=NULL
  
  for(i in 1:Nsim){
    y <- null_beta_scaled(x, traits)
    tax[i]=y$tax
    fun[i]=y$fun
  }
  nullBeta <- cbind(tax,fun)
  return(nullBeta)
}

################################################################################
################################ END NULL MODEL ################################
################################################################################