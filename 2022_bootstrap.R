library(tidyverse)
library(vegan)

data2022 <- read_csv("CoverSep2022.csv") %>%
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
U_2022 <- data2022 %>%
  filter(severity=="U") %>%
  select(-c(plot, severity))
U_2022 <- U_2022[rowSums(U_2022 != 0) > 0,]

U_2022_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(U_2022)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  U_2022_bootstrap <- rbind(U_2022_bootstrap, matrix)
}  

write.csv(U_2022_bootstrap, "U_2022_bootstrap.csv", row.names=FALSE)

# Low Severity
L_2022 <- data2022 %>%
  filter(severity=="L") %>%
  select(-c(plot, severity))
L_2022 <- L_2022[rowSums(L_2022 != 0) > 0,]

L_2022_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(L_2022)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  L_2022_bootstrap <- rbind(L_2022_bootstrap, matrix)
}  

write.csv(L_2022_bootstrap, "L_2022_bootstrap.csv", row.names=FALSE)

# High Severity
H_2022 <- data2022 %>%
  filter(severity=="H") %>%
  select(-c(plot, severity))
H_2022 <- H_2022[rowSums(H_2022 != 0) > 0,]

H_2022_bootstrap=NULL

for(i in 1:999){
  matrix <- boots(H_2022)
  matrix <- matrix %>% 
    select(-plot) %>%
    mutate(boot=i) %>%
    select(boot, everything())
  H_2022_bootstrap <- rbind(H_2022_bootstrap, matrix)
}  

write.csv(H_2022_bootstrap, "H_2022_bootstrap.csv", row.names=FALSE)

boots_2022 <- rbind(U_2022_bootstrap, L_2022_bootstrap, H_2022_bootstrap) %>%
  mutate(year=2022)

write.csv(boots_2022, "boots_2022.csv", row.names=F)
