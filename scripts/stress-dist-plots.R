
### stress distribution figures

q99StressDistRotationPlot <- stressQ99 %>% 
  dplyr::filter(load != 'compress') %>%
  mutate(load = factor(load, levels = c("axial", "lateral", "flex", "extend"), labels = c("axial rotation", "lateral bending", "flexion", "extension"))) %>% 
  ggplot(aes(x = effStress))



stressDistRotationPlot <- q99StressDistRotationPlot +
    geom_density(aes(fill = species, colour = species), alpha = 0.5, trim = TRUE) +
    facet_grid(vars(load), vars(fsu), labeller = label_wrap_gen(10)) +
    scale_colour_viridis_d(begin = 0.1, end = 0.8) +
    scale_fill_viridis_d(begin = 0.1, end = 0.8) +
    theme_bw() +
    labs(x = "effective stress (MPa)", y = "density") +
    theme(
      title = element_text(size = 12),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    ) +
    scale_y_continuous(breaks = c(0.0, 1.0, 2.0)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))

stressDistRotationPlot

ggsave("rotation-stress-dist-plot.pdf", plot = last_plot(), path = here("figures"))

q99StressDistCompressPlot <- stressQ99 %>% 
  dplyr::filter(load == 'compress') %>% 
  ggplot(aes(x = effStress))

stressDistCompressionPlot <- q99StressDistCompressPlot +
  geom_density(aes(fill = species, colour = species), alpha = 0.5, trim = TRUE) +
  facet_grid(vars(load), vars(fsu), labeller = label_wrap_gen(10)) +
  scale_colour_viridis_d(begin = 0.1, end = 0.8) +
  scale_fill_viridis_d(begin = 0.1, end = 0.8) +
  theme_bw() +
  labs(x = "effective stress (MPa)", y = "density") +
  theme(
    title = element_text(size = 12),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  ) + 
  scale_y_continuous(breaks = c(0.0, 0.2, 0.4)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30))

ggsave("compression-stress-dist-plot.pdf", plot = stressDistCompressionPlot, path = here("figures"))
