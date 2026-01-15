library(azores.fkw)
library(lubridate)
library(dplyr)

path <- here::here("data/")

# Load all the sightings
load("data/mb.sightings.rda")
load("data/me.sightings.rda")
load("data/msp.sightings.rda")
load("data/zc.sightings.rda")
load("data/ha.sightings.rda")
load("data/md.sightings.rda")

#Create a species column before create the sighting file together
mb.sightings$species <- "Mb"
me.sightings$species <- "Me"
md.sightings$species <- "Md"
msp.sightings$species <- "Msp"
ha.sightings$species <- "Ha"
zc.sightings$species <- "Zc"

#Bind all the species
bkw.sightings <- rbind(mb.sightings,me.sightings,msp.sightings,md.sightings,zc.sightings,ha.sightings)

# Calculate the duration of each sighting
bkw.sightings <- bkw.sightings %>%
  mutate(duration = difftime(final_time, initial_time, units = "mins"))

#Now I want to have the number of sightings in each month
# Convert the "date" column to a Date object
bkw.sightings$date <- as.Date(bkw.sightings$date)

# Filter data for a specific month (April)
bkw.sightings.april <- bkw.sightings %>%
  filter(month(date) == 4)
# Filter data for a specific month (May)
bkw.sightings.may <- bkw.sightings %>%
  filter(month(date) == 5)
# Filter data for a specific month (June)
bkw.sightings.june <- bkw.sightings %>%
  filter(month(date) == 6)
# Filter data for a specific month (July)
bkw.sightings.july <- bkw.sightings %>%
  filter(month(date) == 7)
# Filter data for a specific month (August)
bkw.sightings.august <- bkw.sightings %>%
  filter(month(date) == 8)
# Filter data for a specific month (September)
bkw.sightings.september <- bkw.sightings %>%
  filter(month(date) == 9)
# Filter data for a specific month (October)
bkw.sightings.october <- bkw.sightings %>%
  filter(month(date) == 10)
# Filter data for a specific month (November)
bkw.sightings.november <- bkw.sightings %>%
  filter(month(date) == 11)

bkw.sightings.spring <- rbind(bkw.sightings.april,bkw.sightings.may,bkw.sightings.june)
bkw.sightings.summer <- rbind(bkw.sightings.july,bkw.sightings.august,bkw.sightings.september)
bkw.sightings.autumn <- rbind(bkw.sightings.october,bkw.sightings.november)

bkw.sightings.spring$season <- "spring"
bkw.sightings.summer$season <- "summer"
bkw.sightings.autumn$season <- "autumn"

bkw.sightings <- rbind(bkw.sightings.spring,bkw.sightings.summer,bkw.sightings.autumn)
bkw.sightings$family <- "Ziphiidae"

# Save for a rda file
usethis::use_data(bkw.sightings, overwrite = TRUE)

# Filter only for have Mb and Ha sightings
mb.ha.sightings <- bkw.sightings %>%
  filter(species %in% c("Mb", "Ha"))
# Save for a rda file
usethis::use_data(mb.ha.sightings, overwrite = TRUE)
