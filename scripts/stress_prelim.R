### bighorn c2c3
axStress <- read_delim("output/bh_c2c3_axial_stress.txt", delim = " ", comment = "*", col_names = c("element", "sx", "sy", "sz", "sxy", "syz", "sxz"))

axStress <- axStress %>%
  slice((count(axStress)$n-(count(axStress)/10)$n+1):count(axStress)$n)



axVol <- read_delim("output/bh_c2c3_axial_vol.txt", delim = " ", comment = "*", col_names = c("element", "volume"))

axVol <- axVol %>% 
  slice((count(axVol)$n-(count(axVol)/10)$n+1):count(axVol)$n)

right_join(axVol, axStress) %>% 
  mutate(effStress = sqrt(sx^2 + sy^2 + sz^2 - sx*sy - sy*sz - sx*sz + 3*(sxy*sxy + syz*syz+ sxz*sxz))) %>% 
  ggplot(aes(x = effStress, y = volume)) +
  geom_area()