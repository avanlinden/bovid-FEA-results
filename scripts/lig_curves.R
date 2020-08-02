library(tidyverse)
library(fs)
library(broom)

lig_txt_files <- fs::dir_ls("space_delim_lig_files")

lig_df <- lig_txt_files %>% 
  map_dfr(read_table2, .id = "ligID") %>% 
  rename(disp = '0', force = '0_1', dispOops = '0.0000', forceOops = '0.0000_1') %>% 
  mutate(forceOops = as.numeric(forceOops))

coalesce(lig_df$disp, lig_df$dispOops)

lig_df <- lig_df %>% 
  mutate(disp = coalesce(lig_df$disp, lig_df$dispOops), force = coalesce(lig_df$force, lig_df$forceOops)) %>% 
  select(-dispOops, -forceOops) %>% 
  mutate(ligID = str_remove(ligID, "space_delim_lig_files/")) %>% 
  mutate(ligID = str_remove(ligID, "_lc.txt"))
  
lig_df <- lig_df %>% 
  mutate(force = replace_na(force, 316.3435))

lig_df %>% 
  ggplot(aes(x = disp, y = force, color = ligID)) +
    geom_point()

1:10 %>% 
  map(rnorm, n = 10) %>% 
  map_dbl(mean)

lig_sep <- lig_df %>% 
  split(.$ligID) 

approx(x = lig_sep[[1]]$disp, y = lig_sep[[1]]$force, n = 50)

lig_interp <- lig_df %>% 
  split(.$ligID) %>% 
  map(~approx(x = .$disp, y = .$force, n = 50)) %>% 
  unlist(recursive = FALSE) %>% 
  as_tibble()

lig_interp

lig_interpx <- lig_interp %>% 
  select(contains('x')) %>% 
  pivot_longer(everything(), names_to = "ligID", values_to = "dispApprox") %>% 
  mutate(ligID = str_remove(ligID, '.x'))

lig_interpy <- lig_interp %>% 
  select(contains('y')) %>% 
  pivot_longer(everything(), names_to = "ligID", values_to = "forceApprox") %>% 
  mutate(ligID = str_remove(ligID, '.y'))

bind_cols(lig_interpx, lig_interpy) %>% 
  select(-ligID1) %>% 
  ggplot(aes(x = dispApprox, y = forceApprox)) +
    geom_point(aes(color = ligID)) +
    geom_smooth(n = 50)

lig_approx <- bind_cols(lig_interpx, lig_interpy) %>% 
  select(-ligID1)

lig_avg <- loess(forceApprox ~ dispApprox, data = lig_approx, span = 0.75)


avg_force <- approx(x = lig_avg$fitted, n = 50)$y

avg_force

avg_approx <- enframe(avg_force, name = NULL, value = "approxAvgForce") %>% 
  mutate(approxAvgDisp = filter(lig_approx, ligID == 'CAPS')$dispApprox) %>% 
  mutate(ligID = 'AVG') %>% 
  rename(forceApprox = approxAvgForce, dispApprox = approxAvgDisp)

ligForceDisp <- lig_approx %>% 
  bind_rows(avg_approx)

distinct(ligForceDisp, ligID)

dir.create(temp <- tempfile())

ligForceDisp %>% 
  mutate(dispApprox = round(dispApprox, 5), forceApprox = round(forceApprox, 5)) %>% 
  mutate(lcPoint = as.character(str_glue("<point>{dispApprox}", ",", "{forceApprox}</point>"))) %>% 
  select(-dispApprox, -forceApprox) %>% 
  group_by(ligID) %>% 
  group_walk(~ write.table(.x, file = file.path(getwd(), paste0(.y$ligID, ".txt")), quote = FALSE, col.names = FALSE, row.names = FALSE))

lig_approx %>% 
  mutate(dispApprox = round(dispApprox, 5), forceApprox = round(forceApprox, 5))

ligForceDisp %>% 
  filter(ligID == 'AVG')

avg25 <- approx(x = avg_approx$dispApprox, y = avg_approx$forceApprox, n = 25) %>% 
  unlist(use.names = FALSE)

avg25disp <- enframe(round(avg25[1:25], 5), name = NULL, value = "disp")

avg25force <- enframe(round(avg25[26:50], 5), name = NULL, value = 'force')

bind_cols(avg25disp, avg25force) %>% 
  write_delim(path = "AVG_25_lc.txt", delim = " ", col_names = FALSE)


approx(x = dispApprox, y = forceApprox, n = 25)
