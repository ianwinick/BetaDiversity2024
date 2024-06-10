library(tidyverse)
library(vegan)

data2021 <- read_csv("CoverSep2021.csv") %>% 
  mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
  select(-treatment)

# Function
boots <- function(x){
  matrix=NULL
  for(i in 1:nrow(x)){
    plot <- x %>%
      filter(row.names(.) %in% sample(row.names(.), size=1)) %>%
      mutate(plot=i)
    matrix <- rbind(matrix, plot) 
  }
  return(matrix)
}

#Unburned
U_2021 <- data2021 %>%
  filter(severity=="U") %>%
  select(-c(plot, severity))
U_2021 <- U_2021[rowSums(U_2021 != 0) > 0,]

U_2021_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(U_2021)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  U_2021_bootstrap <- rbind(U_2021_bootstrap, matrix)
}  

write.csv(U_2021_bootstrap, "U_2021_bootstrap.csv", row.names=FALSE)

# Low Severity
L_2021 <- data2021 %>%
  filter(severity=="L") %>%
  select(-c(plot, severity))
L_2021 <- L_2021[rowSums(L_2021 != 0) > 0,]

L_2021_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(L_2021)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  L_2021_bootstrap <- rbind(L_2021_bootstrap, matrix)
}  

write.csv(L_2021_bootstrap, "L_2021_bootstrap.csv", row.names=FALSE)

# High Severity
H_2021 <- data2021 %>%
  filter(severity=="H") %>%
  select(-c(plot, severity))
H_2021 <- H_2021[rowSums(H_2021 != 0) > 0,]

H_2021_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(H_2021)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  H_2021_bootstrap <- rbind(H_2021_bootstrap, matrix)
}  

write.csv(H_2021_bootstrap, "H_2021_bootstrap.csv", row.names=FALSE)

boots_2021 <- rbind(U_2021_bootstrap, L_2021_bootstrap, H_2021_bootstrap) %>%
  mutate(year=2021)

write.csv(boots_2021, "boots_2021.csv", row.names=F)

################################################################################

beta = NULL
for(i in 1:999){
  matrix <- H_2021_bootstrap %>%
    filter(boot==i) %>%
    select(-boot)
  
  beta[i] <- mean(vegdist(matrix))
}
mean(beta)
sd(beta)
