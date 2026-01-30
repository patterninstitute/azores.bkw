library(azores.fkw)

# Load all the ethology data
load("data/mb.ethology.rda")
load("data/me.ethology.rda")
load("data/msp.ethology.rda")
load("data/zc.ethology.rda")
load("data/ha.ethology.rda")
load("data/md.ethology.rda")

#I want to join now all the dataframes
bkw.ethology <- rbind(mb.ethology,me.ethology,msp.ethology,md.ethology,zc.ethology,ha.ethology)


# Create a new column grouping specific behaviors
bkw.ethology <- bkw.ethology %>%
  mutate(grouped_behaviors = case_when(
    behavior %in% c("foraging", "travelling slow","hiding") ~ "Foraging",
    behavior %in% c("travelling average", "travelling fast") ~ "Travelling",
    behavior %in% c("milling", "resting","socializing","breaching side","breaching back","spy hopping") ~ "Socializing",
    TRUE ~ "Others"
  ))

# Save for a rda file
usethis::use_data(bkw.ethology, overwrite = TRUE)


#I want to join now Mb and Ha the dataframes
mb.ha.ethology <- rbind(mb.ethology,ha.ethology)


# Create a new column grouping specific behaviors
mb.ha.ethology <- mb.ha.ethology %>%
  mutate(grouped_behaviors = case_when(
    behavior %in% c("foraging", "travelling slow","hiding") ~ "Foraging",
    behavior %in% c("travelling average", "travelling fast") ~ "Travelling",
    behavior %in% c("milling", "resting","socializing","breaching side","breaching back","spy hopping") ~ "Socializing",
    TRUE ~ "Others"
  ))

# Save for a rda file
usethis::use_data(mb.ha.ethology, overwrite = TRUE)
