### usethis setup

library(usethis)

git_vaccinate()


### load libraries

library(here)
library(tidyverse)

git_sitrep()

here()

### order of script files:

source(here("scripts/import-stress-data.R"))

source(here("scripts/tidy_stress_data.R"))

source(here("scripts/filter-stress-quantiles.R"))

source(here("scripts/summarize-stress-data.R"))

source(here("scripts/assign-colors.R"))

source(here("scripts/summary-stress-plot.R"))

source(here("scripts/stress-dist-plots.R"))

#source(here("scripts/import-quaternions.R")) #don't need if have already done Python angle conversion

source(here("scripts/tidy-euler-angle-data.R"))

source(here("scripts/rom-dot-plot.R"))

# "scripts/lig-curves.R" used to interpolate and average ligament force displacement curves from DeVries 2015; not used for post-experiment data analysis