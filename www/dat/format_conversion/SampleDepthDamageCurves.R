# Preview Depth Damage Curves
library(tidyverse)


fname <- "C:/Users/mbayly/Desktop/Projects/EN2591/hazus/HAZUS_depth_damage_functions.csv"
dd_curve <- read.csv(fname)
head(dd_curve)
dds <- dd_curve[,c(1:34)]

# Convert from wide to long
dl <- gather(dds, depth, damage, ft04m:ft24, factor_key=TRUE)
# Fix Depth
dl$depth<- as.character(dl$depth)
dl$depth <- gsub("ft", "", dl$depth)
dl$depth <- gsub("m", "-", dl$depth)
dl$depth <- as.numeric(sub("([0-9.]+)-$", "-\\1", dl$depth))
dl$depth_m <- dl$depth*0.3048
# Fix Damage
dl$damage <- as.numeric(as.character(dl$damage))
dl$damage <- dl$damage/100

# Fix ID
dl$DmgFnId <- as.character(dl$DmgFnId)


dl$Description <- tolower(dl$Description)
# Extract floors 
  dl$Floors <- NA
  dl$Floors <- ifelse(grepl("two story", dl$Description), 2, dl$Floors)
  dl$Floors <- ifelse(grepl("one story", dl$Description), 1, dl$Floors)
  dl$Floors <- ifelse(grepl("three or more floors", dl$Description), 3, dl$Floors)
  dl$Floors <- ifelse(grepl("one & 1/2 story", dl$Description), 1.5, dl$Floors)
  dl$Floors <- ifelse(grepl("two or more stories", dl$Description), 2, dl$Floors)
  dl$Floors <- ifelse(grepl("two floors", dl$Description), 2, dl$Floors)
  dl$Floors <- ifelse(grepl("one floor", dl$Description), 1, dl$Floors)
  dl$Floors <- ifelse(grepl("split level", dl$Description), 1, dl$Floors)
  

# Extract Basement 
  dl$Basement <- 0
  dl$Basement <- ifelse(grepl("basement", dl$Description), 1, dl$Basement)
  dl$Basement <- ifelse(grepl("no basement", dl$Description), 0, dl$Basement)
  
  
# salt_water 
  dl$salt_water <- NA
  dl$salt_water <- ifelse(grepl("fresh", dl$Description), "fresh", dl$salt_water)
  dl$salt_water <- ifelse(grepl("salt", dl$Description), "salt", dl$salt_water)
 
  
# duration 
  dl$duration <- NA
  dl$duration <- ifelse(grepl("long", dl$Description), "long", dl$duration)
  dl$duration <- ifelse(grepl("short", dl$Description), "short", dl$duration)
  
# foundation 
  dl$foundation <- NA
  dl$foundation <- ifelse(grepl("pier", dl$Description), "pier", dl$foundation)
  dl$foundation <- ifelse(grepl("slab", dl$Description), "slab", dl$foundation)
  dl$foundation <- ifelse(grepl("pile", dl$Description), "pile", dl$foundation)
  
  
# zone 
  dl$fema_zone <- NA
  dl$fema_zone <- ifelse(grepl("a-zone", dl$Description), "a-zone", dl$fema_zone)
  dl$fema_zone <- ifelse(grepl("v-zone", dl$Description), "v-zone", dl$fema_zone)
  
# Export
  write.csv(dl, row.names = FALSE, file="C:/Users/mbayly/Desktop/Projects/EN2591/hazus/HAZUS_depth_damage_functions_long.csv")
 
  
  
dsub <- dl %>% filter(Occupancy == 'COM1' & (foundation=="slab" | is.na(foundation)))
#View(dsub)

dsub <- dl %>% filter(Occupancy == 'RES1' & duration == "short")
dsub <- dl %>% filter(Occupancy == 'RES1')
ggplot(dsub, aes(x=depth_m, y=damage, group=DmgFnId)) +
  geom_line(aes(color=DmgFnId)) + 
  theme(legend.position = "none")

ggplot(dsub, aes(x=depth_m, y=damage, group=zone)) +
  geom_line(aes(color=zone))

ggplot(dsub, aes(x=depth_m, y=damage, group=Source)) +
  geom_line(aes(color=Source))

ggplot(dsub, aes(x=depth_m, y=damage, group=foundation)) +
  geom_line(aes(color=foundation))

ggplot(dsub, aes(x=depth_m, y=damage, group=duration)) +
  geom_line(aes(color=duration))

ggplot(dsub, aes(x=depth_m, y=damage, group=salt_water)) +
  geom_line(aes(color=salt_water))
