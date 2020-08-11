## read in Euler Angle ROM data in degrees

romDeg <- read_csv(here("clean-data", "ROM-degrees.csv"))

#visualize with cleveland dot plot

romDeg %>% 
  mutate(species = factor(species, levels = c("bighorn", "impala")), 
         fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c("axial", "lateral", "flex", "extend"))
  )
  
# separate bighorn and impala data points for dot plot
  
bhDeg <- romDeg %>% 
  dplyr::filter(species == 'bighorn') %>% 
  mutate(bhTotal = totalDeg, bhMax = maxDeg) %>% 
  select(fsu, load, bhTotal, bhMax)

imDeg <- romDeg %>% 
  dplyr::filter(species == 'impala') %>% 
  mutate(imTotal = totalDeg, imMax = maxDeg) %>% 
  select(fsu, load, imTotal, imMax)

## manually assign colors for cleveland dot plot

colors <- c(viridisLite::viridis(n = 2, begin = 0.1, end = 0.8), 'grey')

#recombine, refactor, and plot

bhDeg %>% 
  right_join(imDeg, by = c("fsu", "load")) %>%
  mutate(fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
         load = factor(load, levels = c("flex", "lateral", "axial", "extend"))) %>% 
  ggplot() +
    geom_segment(aes(x = load, xend = load, y = bhMax, yend = imMax, color = colors[3])) +
    geom_point(aes(x = load, y = bhMax, color = colors[1]), size = 3) +
    geom_point(aes(x = load, y = imMax, color = colors[2]), size = 3) +
    scale_color_manual(values = colors) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~fsu, nrow = 3) +
    ylab("Range of Motion (degrees)") +
    ylim(0, 10)

    