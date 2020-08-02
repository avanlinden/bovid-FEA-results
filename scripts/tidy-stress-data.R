
#read in combined stress datra
elemStress <- read_csv(here("clean-data", "element-stress.csv"))

#tidy stress data: 
#1 - columns for model loading regime, FSU, and species;
#2 - calculate effective stress from cauchy stress tensors

modelStress <- elemStress %>%
  mutate(
    load = case_when(
      str_detect(source, "axial") ~ 'axial',
      str_detect(source, "lateral") ~ 'lateral',
      str_detect(source, "extend") ~ 'extend',
      str_detect(source, "flex") ~ 'flex',
      TRUE ~ 'compress'
    )
  ) %>%
  mutate(
    species = case_when(
      str_detect(source, 'bh') ~ 'bighorn',
      TRUE ~ 'impala'
      )
    ) %>%
  mutate(
    fsu = case_when(
      str_detect(source, 'c2c3') ~ 'C2-C3',
      str_detect(source, 'c4c5') ~ 'C4-C5',
      TRUE ~ 'C6-C7'
    )
  ) %>%
  mutate(
    effStress = sqrt(sx ^ 2 + sy ^ 2 + sz ^ 2 - sx * sy - sy * sz - sx * sz + 3 * (sxy * sxy + syz * syz + sxz * sxz)
    )
  ) %>%
  select(species, fsu, load, element, effStress)
