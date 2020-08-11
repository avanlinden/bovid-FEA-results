
### import rigid body quaternion rotation data

modelQuatern<- list.files(path = here("raw-data", "T1-Output"),
                          pattern = "rom.txt",
                          full.names = TRUE) %>% 
  set_names() %>% 
  map_df(read_delim, delim = " ", comment = "*", col_names = c("material", "qx", "qy", "qz", "qw"), .id = "source") %>% 
  mutate(
    load = case_when(
      str_detect(source, "axial") ~ 'axial',
      str_detect(source, "lateral") ~ 'lateral',
      str_detect(source, "extend") ~ 'extend',
      str_detect(source, "flex") ~ 'flex',
      TRUE ~ 'compress'
    )
  ) %>%
  mutate(species = case_when(str_detect(source, 'bh') ~ 'bighorn',
                             TRUE ~ 'impala')) %>%
  mutate(fsu = case_when(
    str_detect(source, 'c2c3') ~ 'C2-C3',
    str_detect(source, 'c4c5') ~ 'C4-C5',
    TRUE ~ 'C6-C7'
  )) %>% 
  select(species, fsu, load, qx, qy, qz, qw)


#export as CSV, perform quaternions - Euler angles conversion in python
write_csv(modelQuatern, path = here("clean-data", "t1_quatern.csv"))
