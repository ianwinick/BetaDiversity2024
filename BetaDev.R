library(tidyverse)
library(vegan)
library(FD)

U_2020 <- read_csv("U_2020_bootstrap.csv")
L_2020 <- read_csv("L_2020_bootstrap.csv")
H_2020 <- read_csv("H_2020_bootstrap.csv")
traits <- read_csv("traitMatrix.csv") %>%
  column_to_rownames(var="spp")

for(i in 1:999){
  data <- U_2020 %>%
    filter(boot==i)
  data <- data[, colSums(data != 0) > 0]
  data <- data %>%
    select(-boot) %>%
    rownames_to_column(var="plot") %>%
    pivot_longer(cols=-plot, names_to="spp", values_to="cover")
  
  null <- null_beta_rep(data, traits, Nsim=999)
  
  null <- null %>%
    mutate(year=2020, severity="U")
  
  write.csv(null, str_c("Bootstrapped Nulls/U2020_", i, ".csv"))
}

for(i in 1:999){
  data <- L_2020 %>%
    filter(boot==i)
  data <- data[, colSums(data != 0) > 0]
  data <- data %>%
    select(-boot) %>%
    rownames_to_column(var="plot") %>%
    pivot_longer(cols=-plot, names_to="spp", values_to="cover")
  
  null <- null_beta_rep(data, traits, Nsim=999)
  
  write.csv(null, str_c("Bootstrapped Nulls/2020/L2020_", i, ".csv"), row.names=FALSE)
}

for(i in 1:999){
  data <- H_2020 %>%
    filter(boot==i)
  data <- data[, colSums(data != 0) > 0]
  data <- data %>%
    select(-boot) %>%
    rownames_to_column(var="plot") %>%
    pivot_longer(cols=-plot, names_to="spp", values_to="cover")
  
  null <- null_beta_rep(data, traits, Nsim=999)
  
  write.csv(null, str_c("Bootstrapped Nulls/2020/H2020_", i, ".csv"))
}
