library(tidyverse)
library(hrbrthemes)

### Import element stress data for Time = 1 from all models ======

elemStress <- list.files(path = "T1-OUTPUT", pattern = "stress.txt", full.names = TRUE) %>% 
  set_names() %>% 
  map_df(read_delim, delim = " ", comment = "*", col_names = c("element", "sx", "sy", "sz", "sxy", "syz", "sxz"), .id = "source")

### Tidy and reformat stress data =============
#Add columns identifying species, FSU, and model loading conditions; calculate effective stress

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
  mutate(species = case_when(str_detect(source, 'bh') ~ 'bighorn',
                             TRUE ~ 'impala')) %>%
  mutate(fsu = case_when(
    str_detect(source, 'c2c3') ~ 'C2-C3',
    str_detect(source, 'c4c5') ~ 'C4-C5',
    TRUE ~ 'C6-C7'
  )) %>%
  mutate(effStress = sqrt(
    sx ^ 2 + sy ^ 2 + sz ^ 2 - sx * sy - sy * sz - sx * sz + 3 * (sxy * sxy + syz *
                                                                    syz + sxz * sxz)
  )) %>%
  select(species, fsu, load, element, effStress)

### Visualize stress distributions =========

library(ggridges)

#regular density plot for one model
modelStress %>% 
  filter(species == 'bighorn', fsu == 'C2-C3', load == 'axial') %>% 
  ggplot(aes(x = effStress)) +
    geom_density()

#ridgeline plot, not sure about this one
modelStress %>% 
  filter(fsu == 'C2-C3', load == 'axial') %>% 
  ggplot(aes(x = effStress, y = species)) +
    geom_density_ridges2(rel_min_height = 0.01)

#overlapping transparent density plot
modelStress %>% 
  filter(fsu == 'C2-C3', load == 'axial') %>% 
  ggplot(aes(x = effStress)) +
  geom_density(aes(fill = species, colour = species, alpha = 0.5), trim = FALSE)

#violin plot 
modelStress %>% 
  filter(fsu == 'C2-C3', load == 'axial') %>% 
  ggplot(aes(species, effStress)) +
    geom_violin(draw_quantiles = c(0.95, 0.99))

### quantiles ==========
#update to dplyr 1.0


stressQuant <- modelStress %>% 
  group_by(species, fsu, load) %>% 
  summarise(qvalue = quantile(effStress, c(0.95, 0.99)), q = c(0.95, 0.99))

summStress <- modelStress %>% 
  group_by(species, fsu, load) %>%
  summarise(maxStress = max(effStress), 
            minStress = min(effStress),
            meanStress = mean(effStress), 
            q_95 = quantile(effStress, c(0.95)),
            q_99 = quantile(effStress, c(0.99))
  )

sumStress %>%
  mutate(fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c(
           "axial", "lateral", "flex", "extend", "compress"
         ))) %>%
  pivot_longer(maxStress:q_99, names_to = "stressStat", values_to = "stressValue") %>%   ggplot(aes(x = fsu, y = maxStress, group = species)) +
  geom_point(aes(color = species), size = 3) +
  geom_line(aes(color = species), size = 1) +
  geom_point(aes(x = fsu, y = meanStress, color = species), size = 3) +
  geom_line(aes(x = fsu, y = meanStress, color = species),
            linetype = 3,
            size = 1) +
  #geom_point(aes(x = fsu, y = q_99, color = species)) +
  #geom_line(aes(x = fsu, y = q_99, color = species), linetype = 3) +
  facet_wrap( ~ load, nrow = 1) +
  theme_bw() +
  ylab("effective stress (MPa)") +
  theme(
    title = element_text(size = 12),
    strip.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )




summStress %>%
  pivot_longer(maxStress:q_99, names_to = "stressStat", values_to = "stressValue")

#plot with linetype based on quantiles

summStress %>%
  mutate(fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c(
           "axial", "lateral", "flex", "extend", "compress"
         ))) %>%
  pivot_longer(maxStress:q_99, names_to = "stressStat", values_to = "stressValue") %>%
  filter(stressStat %in% c('q_99', 'meanStress')) %>%
  ggplot(aes(
    x = fsu,
    y = stressValue,
    group = interaction(species, stressStat),
    color = species,
    linetype = stressStat
  )) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap( ~ load, nrow = 1) +
  theme_bw() +
  ylab("effective stress (MPa)") +
  theme(
    title = element_text(size = 12),
    strip.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )


### stress distribution plots filtered <99 quantile =======================

modelStress %>% 
  group_by(species, fsu, load) %>% 
  filter(effStress)

q99_Stress <- modelStress %>% 
  left_join(summStress) %>% 
  filter(effStress < q_99) %>% 
  select(species, fsu, load, element, effStress)

q95_Stress <- modelStress %>% 
  left_join(summStress) %>% 
  filter(effStress < q_95) %>% 
  select(species, fsu, load, element, effStress)

q99_Stress %>% 
  filter(load != 'compress') %>% 
  ggplot(aes(x = effStress)) +
    geom_density(aes(fill = species, colour = species), alpha = 0.3, trim = TRUE) +
    facet_grid(vars(load), vars(fsu))

q99_Stress %>% 
  #filter(load!= 'compress', fsu == 'C2-C3') %>% 
  filter(load != 'compress') %>%
  ggplot(aes(x = effStress)) +
    geom_density(aes(fill = species, colour = species), alpha = 0.5, trim = TRUE) +
    facet_grid(vars(load), vars(fsu)) +
    theme_bw()
