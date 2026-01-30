library(azores.fkw)

# Load all the pod_age_composition
load("data/mb.pod_age_composition.rda")
load("data/me.pod_age_composition.rda")
load("data/msp.pod_age_composition.rda")
load("data/zc.pod_age_composition.rda")
load("data/ha.pod_age_composition.rda")
load("data/md.pod_age_composition.rda")

#Bind all the species
bkw.pod_age_composition <- rbind(mb.pod_age_composition,me.pod_age_composition,msp.pod_age_composition,md.pod_age_composition,zc.pod_age_composition,ha.pod_age_composition)

# Replace specific names in the 'age_group' column
bkw.pod_age_composition <- bkw.pod_age_composition %>%
  mutate(age_group = case_when(
    age_group == "adult" ~ "adults",
    age_group == "calf" ~ "calves",
    age_group == "juvenile" ~ "juveniles",
    age_group == "newborn" ~ "newborns",
    TRUE ~ age_group  # Keep other values unchanged
  ))

# Save for a rda file
usethis::use_data(bkw.pod_age_composition, overwrite = TRUE)

#Bind Mb and Ha the species
mb.ha.pod_age_composition <- rbind(mb.pod_age_composition,ha.pod_age_composition)

# Replace specific names in the 'age_group' column
mb.ha.pod_age_composition <- mb.ha.pod_age_composition %>%
  mutate(age_group = case_when(
    age_group == "adult" ~ "adults",
    age_group == "calf" ~ "calves",
    age_group == "juvenile" ~ "juveniles",
    age_group == "newborn" ~ "newborns",
    TRUE ~ age_group  # Keep other values unchanged
  ))

# Save for a rda file
usethis::use_data(mb.ha.pod_age_composition, overwrite = TRUE)
