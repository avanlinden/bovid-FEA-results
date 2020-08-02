### filter out 1% highest values of stress data and refactor
  
stressQ99 <- modelStress %>% 
  left_join(sumStress) %>% 
  dplyr::filter(effStress < q_99) %>% 
  select(species, fsu, load, element, effStress) %>% 
  mutate(species = factor(species, levels = c("bighorn", "impala")), 
         fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c("axial", "lateral", "flex", "extend", "compress"))
  )

# filter out highest 5% values of stress data and refactor

stressQ95 <- modelStress %>% 
  left_join(sumStress) %>% 
  dplyr::filter(effStress < q_95) %>% 
  select(species, fsu, load, element, effStress) %>% 
  mutate(species = factor(species, levels = c("bighorn", "impala")), 
         fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c("axial", "lateral", "flex", "extend", "compress"))
  )
