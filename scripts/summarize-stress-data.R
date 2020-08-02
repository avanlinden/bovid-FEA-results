### summarize stress values and compute quantiles

sumStress <- modelStress %>% 
  group_by(species, fsu, load) %>%
  summarise(maxStress = max(effStress), 
            minStress = min(effStress),
            meanStress = mean(effStress), 
            q_95 = quantile(effStress, c(0.95)),
            q_99 = quantile(effStress, c(0.99)))

#refactor levels and reshape to long dataframe
sumStressLong <- sumStress %>%
  mutate(species = factor(species, levels = c("bighorn", "impala")), 
         fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c("axial", "lateral", "flex", "extend", "compress"))
         ) %>%
  pivot_longer(maxStress:q_99, names_to = "stressStat", values_to = "stressValue")

  
#write summary stress data

write_csv(sumStress, path = here("clean-data", "summary-stress-data.csv"))
