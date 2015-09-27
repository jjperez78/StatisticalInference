library(lubridate, quietly = TRUE)
library(RColorBrewer, quietly = TRUE)
library(lattice, quietly = TRUE)
library(xtable, quietly = TRUE)
library(knitr, quietly = TRUE)
library(dplyr, quietly = TRUE)

# First steps. Download and read the file with the data.
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#download.file(url,"StormData.csv.bz2")

# Load data and check how it looks like

StormData <- read.csv("StormData.csv.bz2" , stringsAsFactors = FALSE)
head(StormData)

## NORMATIZE EVTYPE
# Normalize the data in the approved cathegories from the 985 cases of EVTYPE to the 48 normalized ones

# First we preselect just the columns we will use later to speed the process.

StormData<- StormData[complete.cases(StormData[,c("EVTYPE","FATALITIES","INJURIES", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]),c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

# This is how EVTYPE looks now

List <- unique(StormData$EVTYPE)
str(List)

# First we normalize the cathegories

StormData[ (grep( pattern = "Winter Weather" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("WinterWeather")
StormData[ (grep( pattern = "Winter Storm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("WinterStorm")
StormData[ (grep( pattern = "Wildfire" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Wildfire")
StormData[ (grep( pattern = "Waterspout" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Waterspout")
StormData[ (grep( pattern = "Ash" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("VolcanicAsh")
StormData[ (grep( pattern = "Tsunami" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Tsunami")
StormData[ (grep( pattern = "Tropical Storm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("TropicalStorm")
StormData[ (grep( pattern = "Depression" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("TropicalDepression")
StormData[ (grep( pattern = "Tornado" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Tornado")
StormData[ (grep( pattern = "Thunderstorm Win" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "Strong Wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongWind")
StormData[ (grep( pattern = "Storm " , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongSurgeTide")
StormData[ (grep( pattern = "Sleet" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Sleet")
StormData[ (grep( pattern = "Seiche" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Seiche")
StormData[ (grep( pattern = "Rip Current" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("RipCurrent")
StormData[ (grep( pattern = "Marine Thunderstorm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Marine.Thunderstorm")
StormData[ (grep( pattern = "Marine Strong Win" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("MarineStrongWind")
StormData[ (grep( pattern = "Marine High Win" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("MarineHighWind")
StormData[ (grep( pattern = "Marine Hail" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("MarineHail")
StormData[ (grep( pattern = "Lightning" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Lightning")
StormData[ (grep( pattern = "Lakeshore" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("LakeshoreFlood")
StormData[ (grep( pattern = "Lake-Effect Snow" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("LakeEffectSnow")
StormData[ (grep( pattern = "Ice " , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("IceStorm")
StormData[ (grep( pattern = "Typhoon " , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HurricaneTyphoon")
StormData[ (grep( pattern = "Hurricane" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HurricaneTyphoon")
StormData[ (grep( pattern = "High Wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "High Surf" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighSurf")
StormData[ (grep( pattern = "Heavy Snow" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavySnow")
StormData[ (grep( pattern = "Heavy Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Heat" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Heat")
StormData[ (grep( pattern = "Hail" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Hail")
StormData[ (grep( pattern = "Freezing Fog" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FreezingFog")
StormData[ (grep( pattern = "Funnel Cloud" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FunnelCloud")
StormData[ (grep( pattern = "Freeze" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FrostFreeze")
StormData[ (grep( pattern = "Frost" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FrostFreeze")
StormData[ (grep( pattern = "Flash Flood" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FlashFlood")
StormData[ (grep( pattern = "Coastal Flood" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("CoastalFlood")
StormData[ (grep( pattern = "Flood" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Flood")
StormData[ (grep( pattern = "Extreme Cold" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Wind Chill" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Excessive Heat" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "Dust Storm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DustStorm")
StormData[ (grep( pattern = "Dust Devil" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DustDevil")
StormData[ (grep( pattern = "Drought" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Drought")
StormData[ (grep( pattern = "Dense Smoke" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DenseSmoke")
StormData[ (grep( pattern = "Dense Fog" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DenseFog")
StormData[ (grep( pattern = "Debris Flow" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DebrisFlow")
StormData[ (grep( pattern = "Blizzard" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Blizzard")
StormData[ (grep( pattern = "Avalanche" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Avalanche")
StormData[ (grep( pattern = "Astronomical" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("AstronomicalLowTide")


# Now we correct typos and other values easily assignable to each cathegory

StormData[ (grep( pattern = "Snow" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavySnow")
StormData[ (grep( pattern = "Lighting" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Lightning")
StormData[ (grep( pattern = "Record Cold" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Funnel" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FunnelCloud")
StormData[ (grep( pattern = "Cold" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ColdWindChill")
StormData[ (grep( pattern = "Record High" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "High Seas" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("AstronomicalLowTide")
StormData[ (grep( pattern = "High Tides" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("AstronomicalLowTide")
StormData[ (grep( pattern = "Heavy Precipatation" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Wild Fires" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Wildfires")
StormData[ (grep( pattern = "Low Temperature Record" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Record Rainfall" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Record Warmth" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "High Record" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "High winds" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyWind")
StormData[ (grep( pattern = "Hypothermia" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "TSTM" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "Dry" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Drought")
StormData[ (grep( pattern = "Warmth" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "Volcanic" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("VolcanicAsh")
StormData[ (grep( pattern = "Severe Thunderstorm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstorWind")
StormData[ (grep( pattern = "Thunderstorm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstorWind")
StormData[ (grep( pattern = "Microburst" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongWing")
StormData[ (grep( pattern = "Waytersput" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Waterspout")
StormData[ (grep( pattern = "Ice" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("IceStorm")
StormData[ (grep( pattern = "Fire" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Wildfire")
StormData[ (grep( pattern = "Thunderstorm." , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "Icy" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("IceStorm")
StormData[ (grep( pattern = "Surf" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighSurf")
StormData[ (grep( pattern = "WindChill" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Freezing Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Sleet")
StormData[ (grep( pattern = "Hyperthermia(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "Driest" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Drought")
StormData[ (grep( pattern = "Record(.*)Cool" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "Record(.*)Warm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "Record(.*)Precipitation" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Unseasonable(.*)Warm(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "Unseasonable(.*)Hot" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "(.*)DUst(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DustStorm")
StormData[ (grep( pattern = "Warm(.*)Weather" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Heat")
StormData[ (grep( pattern = "(.*)Warm(.*)Wet(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("TropicalDepression")
StormData[ (grep( pattern = "Warm" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "GUSTY LAKE WIND" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("LakeEffectSnow")
StormData[ (grep( pattern = "below normal precipitation" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Drought")
StormData[ (grep( pattern = "(.*)High(.)Winds(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighWinds")
StormData[ (grep( pattern = "(.*)Shower(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "(.*)Hot(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "(.*)Low(.*)Tide." , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("AstronomicalLowTide")
StormData[ (grep( pattern = "(.*)Low(.*)Tide" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("AstronomicalLowTide")
StormData[ (grep( pattern = "(.*)Low(.*)Wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Blizzard")
StormData[ (grep( pattern = "(.*)Low(.*)Rainfall" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Drought")
StormData[ (grep( pattern = "(.*)Low(.*)Temp(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdChillWind")
StormData[ (grep( pattern = "REcord Low" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdChillWind")
StormData[ (grep( pattern = "Marine (.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- NA
StormData[ (grep( pattern = "(.*)tning" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Lightning")
StormData[ (grep( pattern = "W(.*)spout" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Waterspout")
StormData[ (grep( pattern = "Thunder(.*)Wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "Gusty(.*)Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Gusty(.*)Wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyWind")
StormData[ (grep( pattern = "Typh(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HurricaneTyphoon")
StormData[ (grep( pattern = "Rock(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DebrisFlow")
StormData[ (grep( pattern = "Land(.*)spout" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DebrisFlow")
StormData[ (grep( pattern = "(.*)Slide(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DebrisFlow")
StormData[ (grep( pattern = "(.*)Coast(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("CoastalFlood")
StormData[ (grep( pattern = "Whilrwind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Tornado")
StormData[ (grep( pattern = "High(.*)wind" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "Exces(.*)Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Cool(.*)Wet" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FrostFreeze")
StormData[ (grep( pattern = "Wet" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("TropicalDepression")
StormData[ (grep( pattern = "Fog" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DenseFog")
StormData[ (grep( pattern = "Wall" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HurricaneTyphoon")
StormData[ (grep( pattern = "Exces(.*)Rain" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyRain")
StormData[ (grep( pattern = "Swell" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StormSurgeTide")
StormData[ (grep( pattern = "Stream" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FlashFlood")
StormData[ (grep( pattern = "Mix(.*)Preci(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Sleet")
StormData[ (grep( pattern = "Preci(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyWind")
StormData[ (grep( pattern = "High W(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "(.*)Drizzle(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FrostFreeze")
StormData[ (grep( pattern = "(.*)red(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("RipCurl")
StormData[ (grep( pattern = "(.*)Gradient(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Tornado")
StormData[ (grep( pattern = "(.*)Wind(.*)Wave(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighSurf")
StormData[ (grep( pattern = "(.*)Land(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("DebrisFlow")
StormData[ (grep( pattern = "(.*)Aval(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Avalanche")
StormData[ (grep( pattern = "(.*)Wint(.*)Mix(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("WinterWeather")
StormData[ (grep( pattern = "(.*)Whirl(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Whirlwind")
StormData[ (grep( pattern = "(.*)(.*)Unseason(.*)Cool" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExtremeColdWindChill")
StormData[ (grep( pattern = "(.*)Cool(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FreezingFog")
StormData[ (grep( pattern = "(.*)Spray(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FreezingFog")
StormData[ (grep( pattern = "(.*)High(.*)Temp(.*)Record(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ExcessiveHeat")
StormData[ (grep( pattern = "(.*)Rising(.*)Water(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("FlashFlood")
StormData[ (grep( pattern = "(.*)Turbu(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Blizzard")
StormData[ (grep( pattern = "(.*)Wind(.*)Storm(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "(.*)DownBu(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongWind")
StormData[ (grep( pattern = "(.*)Wind(.*)Advisory" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongWind")
StormData[ (grep( pattern = "non-severe Wind Damage" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("StrongWind")
StormData[ (grep( pattern = "Wind Damage" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HeavyWind")
StormData[ (grep( pattern = "WIND GUSTS" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Blizzard")
StormData[ (grep( pattern = "WIND" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("HighWind")
StormData[ StormData[StormData$EVTYPE=="Wind", "EVTYPE"], "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "EROSION" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("CoastalFlood")
StormData[ (grep( pattern = "WND" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "SMOKE" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("DenseSmoke")
StormData[ (grep( pattern = "DROWNING" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Flood")
StormData[ (grep( pattern = "ROGUE WAVE" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Tsunami")
StormData[ (grep( pattern = "ROUGH SEAS" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("HighSurf")
StormData[ (grep( pattern = "Glaze" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("LakeEffectSnow")
StormData[ (grep( pattern = "Wind" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("HighWind")
StormData[ (grep( pattern = "HEAVY SEAS" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("HighSurf")
StormData[ (grep( pattern = "BEACH EROS(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("CoastalFlood")
StormData[ (grep( pattern = "TORNDAO" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Tornado")
StormData[ (grep( pattern = "(.*)Gustnado(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("ThunderstormWind")
StormData[ (grep( pattern = "(.*)Glaze(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("LakeEffectSnow")

# The rest of cathegories will be consither togheter as Uncategorized

StormData[ (grep( pattern = "Summary" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "None" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Monthly" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "(.*)Urban(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Apache" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Southeast" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Month" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Year" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Marine (.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Other" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Failure" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "(.*)Record(.*)" , x = StormData$EVTYPE, ignore.case = TRUE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "HIGH" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "NORTHERN LIGHTS" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "REMNANTS OF FLOYD" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "DAM BREAK" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "No Severe Weather" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "Metro Storm, May 26" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "HEAVY MIX" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "VOG" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "MILD PATTERN" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")
StormData[ (grep( pattern = "EXCESSIVE" , x = StormData$EVTYPE, ignore.case = FALSE) ), "EVTYPE"] <- c("Uncategorized")

# Now we remove invalid data before we can remove the "?" mark
StormData <- StormData[!is.na(StormData[,"EVTYPE"]) , ]
StormData[ StormData$EVTYPE == "?" , "EVTYPE"] <- c("Uncategorized")

# Now EVTYPE looks like that.
List <- unique(StormData$EVTYPE)
str(List)

## NORMALIZE EXPONENTS
# On the next step I will normalize the exponents. I will subsitute the accepted exponents for its value. Any not accepted exponent
# will be assiged to zero. This operation is necessary in order to change the class for this column from character to numeric.

## PROPDMGEXP

StormData[ (grep( pattern = "[Hh]" , x = StormData$PROPDMGEXP, ignore.case = TRUE) ), "PROPDMGEXP"] <- as.numeric(0)
StormData[ StormData$PROPDMGEXP == "+", "PROPDMGEXP"] <- as.numeric(1)
StormData[ StormData$PROPDMGEXP == "-", "PROPDMGEXP"] <- as.numeric(1)
StormData[ (grep( pattern = "[0-9]" , x = StormData$PROPDMGEXP, ignore.case = TRUE) ), "PROPDMGEXP"] <- as.numeric(1)
StormData[ (grep( pattern = "[Kk]" , x = StormData$PROPDMGEXP, ignore.case = TRUE) ), "PROPDMGEXP"] <- as.numeric(10^3)
StormData[ (grep( pattern = "[Mm]" , x = StormData$PROPDMGEXP, ignore.case = TRUE) ), "PROPDMGEXP"] <- as.numeric(10^6)
StormData[ (grep( pattern = "[Bb]" , x = StormData$PROPDMGEXP, ignore.case = TRUE) ), "PROPDMGEXP"] <- as.numeric(10^9)
StormData[ StormData$PROPDMGEXP == "?", "PROPDMGEXP"] <- as.numeric(0)
StormData$PROPDMGEXP <- as.numeric(StormData$PROPDMGEXP)


# CROPDMGEXP

# List of exponents
List<-unique(StormData$CROPDMGEXP)
View(List)

StormData[ (grep( pattern = "[Hh]" , x = StormData$CROPDMGEXP, ignore.case = TRUE) ), "CROPDMGEXP"] <- as.numeric(0)
StormData[ StormData$CROPDMGEXP == "+", "CROPDMGEXP"] <- as.numeric(1)
StormData[ StormData$CROPDMGEXP == "-", "CROPDMGEXP"] <- as.numeric(1)
StormData[ (grep( pattern = "[0-9]" , x = StormData$CROPDMGEXP, ignore.case = TRUE) ), "CROPDMGEXP"] <- as.numeric(1)
StormData[ (grep( pattern = "[Kk]" , x = StormData$CROPDMGEXP, ignore.case = TRUE) ), "CROPDMGEXP"] <- as.numeric(10^3)
StormData[ (grep( pattern = "[Mm]" , x = StormData$CROPDMGEXP, ignore.case = TRUE) ), "CROPDMGEXP"] <- as.numeric(10^6)
StormData[ (grep( pattern = "[Bb]" , x = StormData$CROPDMGEXP, ignore.case = TRUE) ), "CROPDMGEXP"] <- as.numeric(10^9)
StormData[ StormData$CROPDMGEXP == "?", "CROPDMGEXP"] <- as.numeric(0)
StormData$CROPDMGEXP <- as.numeric(StormData$CROPDMGEXP)

# Before we calculate the impact I will replace every NA value for Zero
StormData[is.na( StormData$PROPDMG) , "PROPDMG"] <- 0
StormData[is.na( StormData$PROPDMGEXP) , "PROPDMGEXP"] <- 0
StormData[is.na( StormData$CROPDMG) , "CROPDMG"] <- 0
StormData[is.na( StormData$CROPDMGEXP) , "CROPDMGEXP"] <- 0


## ANALYSIS 

## HUMAN COST
# Preselection of columns of interest

# Data for the heath impact analysis. We select only the complete cases for the variables Event Type, Injuries and Fatalities.

StormData_Health<- StormData[complete.cases(StormData[,c("EVTYPE","FATALITIES","INJURIES")]),c("EVTYPE","FATALITIES","INJURIES")]

# The total efect of each Event Type will be the sum of the total fatalities and injuries caused by this. We arrange the resulting data
# frame by the descending sum of these factors.

StormData_Health <- arrange(summarise ( group_by( StormData_Health , EVTYPE ), SUM = sum( FATALITIES + INJURIES) ) , desc(SUM))

# The worst event type, in terms of human heath, is the maximum value of this dataframe

StormData_HealthSUM[ StormData_HealthSUM$SUM == max(StormData_HealthSUM$SUM) , ]

# The result is:

Evtype_Health <- as.character(StormData_HealthSUM[ StormData_HealthSUM$SUM == max(StormData_HealthSUM$SUM) , 1])
Evtype_HealthValue <- round( as.numeric(StormData_HealthSUM[ StormData_HealthSUM$SUM == max(StormData_HealthSUM$SUM) , 2]) , 2)

# Now we create a plot with
# To reduce the categories I will merge all cathegories that represent less that 1% of the total deads.

StormData_HealthGraph <- StormData_HealthSUM
StormData_HealthGraph[ StormData_HealthGraph$SUM < (sum(StormData_HealthGraph$SUM) * 0.01), "EVTYPE"] <- "Others"
StormData_HealthGraph <- arrange( summarise( group_by( StormData_HealthGraph, EVTYPE), SUM = sum( SUM) ) , desc(SUM))

# Generate graph
linch <-  max(strwidth(StormData_HealthGraph$EVTYPE, "inch")+0.4, na.rm = TRUE)
par(mai=c(1.02,linch,0.82,0.42))

graph1 <- barplot(height = StormData_HealthGraph$SUM, names.arg = StormData_HealthGraph$EVTYPE ,horiz=TRUE, main = "Total Diseases by Atmospheric Event since 1950", xlab = "Recorded diseases since 1950", col=rainbow(12), las=1)
print(graph1)


## ECONOMIC IMPACT

# Will create a new dataframe to perform the analysis of the economic impact of every Event Type. I will define the total economical 
# impact as the sum of PROPDMG and CROPDMG. Every one of this fields will be multiplied for the corresponding factor, calculated
# using PROPDMG_EXP and CROPDMG_EXP.

StormData_Econ <- StormData[ complete.cases( StormData [ , c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP") ] ), ]


# We assign a value to every exponente. I expect a high value so I will work with Millions of dollars. So each exponente will be
# assigned to the folling values: K = 10^-3, M = 1, B = 10^3 and the other characters will be zero. The final calculation will 
# consider the value in dollars, when given, and the amount of properties damages. In case of two events have the same amount
# of calculated impact in dollars, the one with the highest number of damaged properties will be choosen first.


##if ( sum( StormData_Econ$PROPDMGEXP == "K") > 0) StormData_Econ[ StormData_Econ$PROPDMGEXP == "K" , "PROPDMGEXP" ] <- 1
##if ( sum( StormData_Econ$PROPDMGEXP == "M") > 0) StormData_Econ[ StormData_Econ$PROPDMGEXP == "M" , "PROPDMGEXP" ] <- 10^3
##if ( sum( StormData_Econ$PROPDMGEXP == "B") > 0) StormData_Econ[ StormData_Econ$PROPDMGEXP == "B" , "PROPDMGEXP" ] <- 10^6

# First we create the factors columns

StormData_Econ$PFAC <- StormData_Econ [ , "PROPDMGEXP"]
StormData_Econ$CFAC <- StormData_Econ [ , "CROPDMGEXP"]
StormData_Econ$Monetary <- 0
StormData_Econ$Properies <- 0

# Now we adjust the values on the columns With the exponents and also for the Flags PFAC and CFAC
# Start removing Unsassigned values.
# PROPDMG

# List of exponents
List<-unique(StormData_Econ$PROPDMGEXP)
View(List)

# Calcute the economic impact for every group
StormData_Econ$Monetary <- StormData_Econ$PROPDMG * StormData_Econ$PROPDMGEXP + StormData_Econ$CROPDMG * StormData_Econ$CROPDMGEXP
StormData_Econ$PROPTotal <- StormData_Econ$PROPDMG * StormData_Econ$PROPDMGEXP
StormData_Econ$CROPTotal <- StormData_Econ$CROPDMG * StormData_Econ$CROPDMGEXP

# Our final data frame
StormData_Econ <- StormData_Econ[ , c("EVTYPE","Monetary","PROPTotal","CROPTotal")]

# We calculate the total economical impact of every event type. We arrange the resulting data frame by the descending sum of 
# these factors. The result will be in Billions of Dollars and Thousens of houses

StormData_EconSUM <- arrange(summarise ( group_by( StormData_Econ , EVTYPE ), Monetary = sum( Monetary) / 10^9, Properties = sum(PROPTotal)/10^9, Crop = sum(CROPTotal)/10^9 ) , desc(Monetary, Properties, Crop))

# The worst event type, in terms of economical impact, is

StormData_EconSUM[ StormData_EconSUM$Monetary == max(StormData_EconSUM$Monetary) , ]

# The result is:

Evtype_Monetary <- as.character( StormData_EconSUM[ StormData_EconSUM$Monetary == max(StormData_EconSUM$Monetary) , "EVTYPE"] )
Evtype_MonetaryValue <- round(as.numeric(StormData_EconSUM[ StormData_EconSUM$Monetary == max(StormData_EconSUM$Monetary) , "Monetary"]) , 2)

# The worst event type, in terms of Damaged Properties, is

StormData_EconSUM[ StormData_EconSUM$Properties == max(StormData_EconSUM$Properties) , ]

# The result is:

Evtype_Properties <- as.character(StormData_EconSUM[ StormData_EconSUM$Properties == max(StormData_EconSUM$Properties) , "EVTYPE" ])
Evtype_PropertiesValue <- round( as.numeric(StormData_EconSUM[ StormData_EconSUM$Properties == max(StormData_EconSUM$Properties) , "Properties"]) , 2)

# The worst event type, in terms of Damaged Crops, is

StormData_EconSUM[ StormData_EconSUM$Crop == max(StormData_EconSUM$Crop) , ]

# The result is:

Evtype_Crop <- as.character(StormData_EconSUM[ StormData_EconSUM$Crop == max(StormData_EconSUM$Crop) , "EVTYPE"])
Evtype_CropValue <- round( as.numeric(StormData_EconSUM[ StormData_EconSUM$Crop == max(StormData_EconSUM$Crop) , "Crop"]) , 2)

# Now we create two plots. One for the total Economic Value and the other will be a double char for the impact in properties and crops.

StormData_EcoGraphTotal <- StormData_EconSUM[,c(1,2)]
StormData_EcoGraphProp <- StormData_EconSUM[,c(1,3)]
StormData_EcoGraphCrop <- StormData_EconSUM[,c(1,4)]

## Graph1 : Total loss for Atmospheric events in billions of dollars.
# To reduce the categories I will merge all cathegories that represent less that 1% of the total deads.

StormData_EcoGraphTotal[ StormData_EcoGraphTotal$Monetary < ( sum( StormData_EcoGraphTotal$Monetary) * 0.01), "EVTYPE"] <- "Others"
StormData_EcoGraphTotal <- arrange( summarise( group_by( StormData_EcoGraphTotal, EVTYPE), Monetary = sum(Monetary) ) , desc(Monetary))

# Generate graph
linch <-  max(strwidth(StormData_EcoGraphTotal$EVTYPE, "inch")+0.4, na.rm = TRUE)
par(mai=c(1.02,linch,0.82,0.42))

graph1 <- barplot(height = StormData_EcoGraphTotal$Monetary, names.arg = StormData_EcoGraphTotal$EVTYPE , horiz=TRUE, main = "Total loss due to Atmospheric Events", xlab = "Loss in Billions of dollars", col=rainbow(14), las=1)
print(graph1)



## Graph2 : Value in billions of dollars for Damaged Properties due to Atmospheric events.
# To reduce the categories I will merge all cathegories that represent less that 1% of the total deads.

StormData_EcoGraphProp [ StormData_EcoGraphProp$Properties < ( sum( StormData_EcoGraphProp$Properties) * 0.01), "EVTYPE"] <- "Others"
StormData_EcoGraphProp <- arrange( summarise( group_by( StormData_EcoGraphProp, EVTYPE), Properties = sum(Properties) ) , desc(Properties))

# Generate graph
linch <-  max(strwidth(StormData_EcoGraphProp$EVTYPE, "inch")+0.4, na.rm = TRUE)
par(mai=c(1.02,linch,0.82,0.42))

graph2 <- barplot(height = StormData_EcoGraphProp$Properties, names.arg = StormData_EcoGraphProp$EVTYPE , horiz=TRUE, main = "Total Value for damaged properties due to Atmospheric Events", xlab = "Loss in Billions of dollars", col=rainbow(12), las=1)



## Graph3 : Value in billions of dollars of Damaged Crops due to Atmospheric events.
# To reduce the categories I will merge all cathegories that represent less that 1% of the total deads.

StormData_EcoGraphCrop[ StormData_EcoGraphCrop$Crop < ( sum( StormData_EcoGraphCrop$Crop) * 0.01), "EVTYPE"] <- "Others"
StormData_EcoGraphCrop <- arrange( summarise( group_by( StormData_EcoGraphCrop, EVTYPE), Crop = sum(Crop) ) , desc(Crop))

# Generate graph
linch <-  max(strwidth(StormData_EcoGraphCrop$EVTYPE, "inch")+0.4, na.rm = TRUE)
par(mai=c(1.02,linch,0.82,0.42))

graph3 <- barplot(height = StormData_EcoGraphCrop$Crop, names.arg = StormData_EcoGraphCrop$EVTYPE , horiz=TRUE, main = "Total Value for damaged crops due to Atmospheric Events", xlab = "Loss in Billions of dollars", col=rainbow(14), las=1)


# Now we plot the last two graphs in a combined panel.

linch <-  max(strwidth(StormData_EcoGraphProp$EVTYPE, "inch")+0.4, na.rm = TRUE)
par(mfrow=c(2,1), mai=c(1.02,linch,0.82,0.42))
barplot(height = StormData_EcoGraphProp$Properties, names.arg = StormData_EcoGraphProp$EVTYPE , horiz=TRUE, main = "Total loss in properties due to Atmospheric Events", xlab = "Loss in Billions of dollars", col=rainbow(12), las=1)
barplot(height = StormData_EcoGraphCrop$Crop, names.arg = StormData_EcoGraphCrop$EVTYPE , horiz=TRUE, main = "Total loss in crops due to Atmospheric Events", xlab = "Loss in Billions of dollars", col=rainbow(14), las=1)


