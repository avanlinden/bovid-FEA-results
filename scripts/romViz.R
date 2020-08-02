library(tidyverse)
library(plotly)

### import range of motion data =============

# import rom data (in quaternions)

modelQuatern<- list.files(path = "T1-OUTPUT", pattern = "rom.txt", full.names = TRUE) %>% 
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

modelQuatern %>% print(n = Inf)

#export as CSV, perform quaternions - Euler angles conversion in python
write_csv(modelQuatern, "t1_quatern.csv")


### ~~ python interlude ~~ ======================

#angleConv.ipynb

### import euler angle ROM data ===================

rom = read_csv("quaternToAngles.csv")


### visualize ROM data ============

test <- rom %>% 
  select(species, fsu, load, xdeg, ydeg, zdeg) %>% 
  mutate(species = as_factor(species)) %>% 
  filter(load != 'compress') %>% 
  filter(fsu == 'C2-C3')

#experimental 3d scatterplot --- I hate it 
plot_ly(test, x = ~xdeg, y = ~ydeg, z = ~zdeg, color = ~species) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'X rotation'),
                      yaxis = list(title = 'Y rotation'),
                      zaxis = list(title = 'Z rotation')))
 
#lollipop plot 
rom %>% 
  select(species, fsu, load, xdeg, ydeg, zdeg) %>% 
  mutate(species = as_factor(species), fsu = as_factor(fsu), load = as_factor(load)) %>% 
  mutate(total = abs(xdeg + ydeg + zdeg)) %>% 
  filter(fsu == 'C2-C3') %>% 
  ggplot(aes(x = load, y = total), group = species) +
    geom_segment(aes(x = load, xend = load, y = 0, yend = total)) +
    geom_point()

#cleveland dot plot - separate bh and im values to plot points separately

bhRom <- rom %>% 
  select(species, fsu, load, xdeg, ydeg, zdeg) %>% 
  mutate(species = as_factor(species), fsu = as_factor(fsu), load = as_factor(load)) %>% 
  mutate(total = abs(xdeg + ydeg + zdeg)) %>% 
  filter(species == 'bighorn') %>% 
  select(fsu, load, bh_total = total)


imRom <- rom %>% 
  select(species, fsu, load, xdeg, ydeg, zdeg) %>% 
  mutate(species = as_factor(species), fsu = as_factor(fsu), load = as_factor(load)) %>% 
  mutate(total = abs(xdeg + ydeg + zdeg)) %>% 
  filter(species == 'impala') %>% 
  select(fsu, load, im_total = total)

totalRom <- bhRom %>% 
  right_join(imRom, by = c("fsu", "load"))

### facet plot cleveland dot by FSU

colors <- c("bighorn" = '#00BFC4', "impala" = '#F8766D')

totalRom %>% 
  rowwise() %>% 
  mutate(mean = mean(bh_total, im_total)) %>% 
  #filter(fsu == 'C2-C3') %>% 
  filter(load != "compress") %>% 
  arrange(mean) %>% 
  mutate(load = factor(load, load)) %>% 
  ggplot() +
    geom_segment(aes(x = load, xend = load, y = bh_total, yend = im_total), color = 'grey') +
    geom_point(aes(x = load, y = bh_total, color = "bighorn"), size = 3) +
    geom_point(aes(x = load, y = im_total, color = "impala"), size = 3) +
    scale_color_manual(values = colors) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~fsu, nrow = 3) +
    ylab("Range of Motion (degrees)") +
    ylim(0, 10)
    
### facet cleveland dot by load

totalRom %>% 
  rowwise() %>% 
  mutate(mean = mean(bh_total, im_total)) %>% 
  #filter(fsu == 'C2-C3') %>% 
  filter(load != "compress") %>% 
  arrange(mean) %>% 
  mutate(fsu = factor(fsu, fsu)) %>% 
  ggplot() +
  geom_segment(aes(x = fsu, xend = fsu, y = bh_total, yend = im_total), color = 'grey') +
  geom_point(aes(x = fsu, y = bh_total, color = "bighorn"), size = 3) +
  geom_point(aes(x = fsu, y = im_total, color = "impala"), size = 3) +
  scale_color_manual(values = colors) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~load, ncol = 1) +
  ylab("Range of Motion (degrees)") +
  ylim(0, 10)
