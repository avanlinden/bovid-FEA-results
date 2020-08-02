### Import element stress data from all models for Time = 1

#check where here is
here("raw-data", "T1-Output")

#read in all stress files 
elemStress <-
  list.files(
    path = here("raw-data", "T1-Output"),
    pattern = "stress.txt",
    full.names = TRUE
  ) %>%
  set_names() %>%
  map_df(
    read_delim,
    delim = " ",
    comment = "*",
    col_names = c("element", "sx", "sy", "sz", "sxy", "syz", "sxz"),
    .id = "source"
  )


#write combined stress data to csv
write_csv(elemStress, path = here("clean-data", "element-stress.csv"))

#add giant csv to gitignore

use_git_ignore("clean-data/element-stress.csv")
