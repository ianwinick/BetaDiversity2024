boots2020 <- read_csv("obs2020_boot.csv")
boots2021 <- read_csv("obs2021_boot.csv")
boots2022 <- read_csv("obs2022_boot.csv")
boots2023 <- read_csv("obs2023_boot.csv")

results <- rbind(boots2020, boots2021, boots2022, boots2023)

write.csv(results, "results_boot.csv")
  
data <- results %>%  
  mutate(severity=factor(severity, levels=c("U", "L", "H"))) %>%
  select(-c(1, boot)) %>%
  mutate(year=case_when(
    year==2020 ~ 1,
    year==2021 ~ 2,
    year==2022 ~ 3,
    year==2023 ~ 4
  )) %>%
  pivot_longer(cols=c(obsBeta, Q_obsBeta), names_to="type", values_to="beta") %>%
  mutate(type=case_when(
    type=="obsBeta" ~ "Taxonomic",
    type=="Q_obsBeta" ~ "Functional"
  ))

data %>%
  group_by(year, severity, type) %>%
  summarize(sd=sd(beta), beta=mean(beta)) %>%
  ggplot(aes(year, beta, color=severity)) +
  geom_point() +
  geom_errorbar(aes(ymin=beta-sd, ymax=beta+sd), width=0.1) +
  geom_smooth(aes(color=severity), method="lm", se=F, linetype="dashed", size=0.1) +
  facet_grid(~type) +
  theme_minimal()
