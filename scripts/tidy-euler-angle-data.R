### import euler angle ROM data ===================

rom = read_csv(here("python-interlude", "quaternToAngles.csv"))

# compute total angle vectors and pull out appropriate single axis angles

romDeg <- rom %>% 
  dplyr::filter(load != 'compress') %>% 
  select(-(qx:qw)) %>% 
  mutate(totalDeg = abs(xdeg + ydeg + zdeg)) %>% 
  mutate(maxDeg = abs(case_when(load == 'axial' ~ xdeg,
                           load == 'extend' ~ zdeg,
                           load == 'flex' ~ zdeg,
                           TRUE ~ ydeg)))

write_csv(romDeg, here("clean-data", "ROM-degrees.csv"))
