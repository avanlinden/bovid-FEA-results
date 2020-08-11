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

colors <- c(viridisLite::viridis(n = 2, begin = 0.1, end = 0.8))

#recombine, refactor, and plot

### facet by FSU - decided not to

# bhDeg %>% 
#   right_join(imDeg, by = c("fsu", "load")) %>%
#   mutate(fsu = factor(fsu, levels = c("C2-C3", "C4-C5", "C6-C7")),
#          load = factor(load, levels = c("flex", "lateral", "axial", "extend"), 
#                        labels = c("flexion (z)", "lateral bending (y)", "axial rotation (x)", "extension (z)"))) %>% 
#   ggplot() +
#     geom_segment(aes(x = load, xend = load, y = bhMax, yend = imMax), colour = 'grey') +
#     geom_point(aes(x = load, y = bhMax, color = colors[1]), size = 3) +
#     geom_point(aes(x = load, y = imMax, color = colors[2]), size = 3) +
#     scale_color_manual(values = colors, labels = c("bighorn", "impala")) +
#     coord_flip() +
#     theme_bw() +
#     facet_wrap(~fsu, nrow = 3) +
#     ylab("Range of Motion (degrees)") +
#     ylim(0, 10) +
#     theme(title = element_text(size = 10),
#         strip.text.x = element_text(size = 10),
#         strip.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 10),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8)) +
#     labs(color = "species")
  

### facet by load

romDotPlot <- bhDeg %>% 
  right_join(imDeg, by = c("fsu", "load")) %>%
  mutate(fsu = factor(fsu, levels = c("C6-C7", "C4-C5", "C2-C3")),
         load = factor(load, levels = c("extend", "axial", "lateral", "flex"), 
                       labels = c("extension (z)", "axial rotation (x)", "lateral bending (y)", "flexion (z)"))) %>% 
  ggplot() +
  geom_segment(aes(x = fsu, xend = fsu, y = bhMax, yend = imMax), colour = 'grey') +
  geom_point(aes(x = fsu, y = bhMax, color = colors[1]), size = 3) +
  geom_point(aes(x = fsu, y = imMax, color = colors[2]), size = 3) +
  scale_color_manual(values = colors, labels = c("bighorn", "impala")) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~ load, nrow = 4) +
  ylab("range of motion (degrees)") +
  ylim(0, 10) +
  theme(title = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)) +
  labs(color = "species")

ggsave("rom-dot-plot.pdf", plot = last_plot(), path = here("figures"))

