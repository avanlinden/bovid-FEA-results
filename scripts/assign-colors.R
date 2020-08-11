library(scales)

show_col(viridisLite::viridis(n = 25))

show_col(viridisLite::viridis(25)[c(1, 3, 5, 20, 22, 24)])

bovidPal <- viridisLite::viridis(25)[c(1, 3, 5, 20, 22, 24)]

names(bovidPal) <- c("bh_c2c3", "bh_c4c5", "bh_c6c7", "im_c2c3", "im_c4c5", "im_c6c7")

bovidPal

show_col(viridisLite::viridis(n = 2, begin = 0.1, end = 0.8)[1])
