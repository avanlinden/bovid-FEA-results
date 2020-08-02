### stress mean and max values figure

stressLinePlot <- sumStressLong %>% 
  dplyr::filter(stressStat %in% c('meanStress', 'q_99')) %>% 
  mutate(load = factor(load, levels = c("axial", "lateral", "flex", "extend", "compress"), labels = c("axial rotation", "lateral bending", "flexion", "extension", "compression"))) %>% 
  ggplot(aes(x = fsu, 
             y = stressValue, 
             group = interaction(species, stressStat), 
             color = species, 
             linetype = stressStat)
         )


#create line plot with viridis color scale

sumStressFig <- stressLinePlot +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  facet_wrap( ~ load, nrow = 1, labeller=label_wrap_gen(width = 10)) +
  theme_bw() +
  ylab("effective stress (MPa)") +
  scale_colour_viridis_d(begin = 0.1, end = 0.8) +
  scale_linetype_manual(values = c(1, 2), labels = c("mean", "99% cutoff")) +
  theme(
    title = element_text(size = 12),
    strip.text.x = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10)
  ) +
  labs(linetype = "stress")

ggsave("summary-stress-plot.pdf", plot = last_plot(), path = here("figures"))
