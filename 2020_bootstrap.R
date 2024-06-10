library(tidyverse)
library(vegan)

data2020 <- read_csv("CoverSep2020.csv") %>% 
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
U_2020 <- data2020 %>%
  filter(severity=="U") %>%
  select(-c(plot, severity))
U_2020 <- U_2020[rowSums(U_2020 != 0) > 0,]

U_2020_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(U_2020)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  U_2020_bootstrap <- rbind(U_2020_bootstrap, matrix)
}  

write.csv(U_2020_bootstrap, "U_2020_bootstrap.csv", row.names=FALSE)

# Low Severity
L_2020 <- data2020 %>%
  filter(severity=="L") %>%
  select(-c(plot, severity))
L_2020 <- L_2020[rowSums(L_2020 != 0) > 0,]

L_2020_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(L_2020)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  L_2020_bootstrap <- rbind(L_2020_bootstrap, matrix)
}  

write.csv(L_2020_bootstrap, "L_2020_bootstrap.csv", row.names=FALSE)

# High Severity
H_2020 <- data2020 %>%
  filter(severity=="H") %>%
  select(-c(plot, severity))
H_2020 <- H_2020[rowSums(H_2020 != 0) > 0,]

H_2020_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(H_2020)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  H_2020_bootstrap <- rbind(H_2020_bootstrap, matrix)
}  

write.csv(H_2020_bootstrap, "H_2020_bootstrap.csv", row.names=FALSE)

boots_2020 <- rbind(U_2020_bootstrap, L_2020_bootstrap, H_2020_bootstrap) %>%
  mutate(year=2020)

write.csv(boots_2020, "boots_2020.csv", row.names=F)
################################################################################

beta = NULL
for(i in 1:999){
   matrix <- H_2020_bootstrap %>%
    filter(boot==i) %>%
    select(-boot)
  
  beta[i] <- mean(vegdist(matrix))
}
mean(beta)
sd(beta)