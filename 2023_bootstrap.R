library(tidyverse)
library(vegan)

data2023 <- read_csv("CoverSep2023.csv") %>% 
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
U_2023 <- data2023 %>%
  filter(severity=="U") %>%
  select(-c(plot, severity))
U_2023 <- U_2023[rowSums(U_2023 != 0) > 0,]

U_2023_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(U_2023)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  U_2023_bootstrap <- rbind(U_2023_bootstrap, matrix)
}  

write.csv(U_2023_bootstrap, "U_2023_bootstrap.csv", row.names=FALSE)

# Low Severity
L_2023 <- data2023 %>%
  filter(severity=="L") %>%
  select(-c(plot, severity))
L_2023 <- L_2023[rowSums(L_2023 != 0) > 0,]

L_2023_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(L_2023)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  L_2023_bootstrap <- rbind(L_2023_bootstrap, matrix)
}  

write.csv(L_2023_bootstrap, "L_2023_bootstrap.csv", row.names=FALSE)

# High Severity
H_2023 <- data2023 %>%
  filter(severity=="H") %>%
  select(-c(plot, severity))
H_2023 <- H_2023[rowSums(H_2023 != 0) > 0,]

H_2023_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(H_2023)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  H_2023_bootstrap <- rbind(H_2023_bootstrap, matrix)
}  

write.csv(H_2023_bootstrap, "H_2023_bootstrap.csv", row.names=FALSE)

boots_2023 <- rbind(U_2023_bootstrap, L_2023_bootstrap, H_2023_bootstrap) %>%
  mutate(year=2023)

write.csv(boots_2023, "boots_2023.csv", row.names=F)
