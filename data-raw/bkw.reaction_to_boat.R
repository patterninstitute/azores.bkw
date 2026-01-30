library(azores.fkw)

#Load reaction_to_boat
load("mb.reaction_to_boat.rda")
load("me.reaction_to_boat.rda")
load("msp.reaction_to_boat.rda")
load("zc.reaction_to_boat.rda")
load("ha.reaction_to_boat.rda")
load("md.reaction_to_boat.rda")

# I want to join all the dataframes
bkw.reaction_to_boat <- rbind(mb.reaction_to_boat,me.reaction_to_boat,msp.reaction_to_boat,md.reaction_to_boat,zc.reaction_to_boat,ha.reaction_to_boat)

# Save for a rda file
usethis::use_data(bkw.reaction_to_boat, overwrite = TRUE)

# I want to join Mb and Ha dataframes
mb.ha.reaction_to_boat <- rbind(mb.reaction_to_boat,ha.reaction_to_boat)

# Save for a rda file
usethis::use_data(mb.ha.reaction_to_boat, overwrite = TRUE)
