# Script - Saturations 011918

# Setup
x <- c("ggmap", "rgdal", "rgeos", "maptools","plyr", "dplyr", "tidyr", "tmap", "reshape2", "XLConnect")
install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

# --- Import data before QC flagging (only want observed equipment for this) -----
## *use Sarah / Keith script for importing data for consistency*
read.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/', remove="*.csv$") 
read.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/', remove="*_clean.csv$")

list.imported <- full_join(
  list.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/', remove="*.csv$"),
  list.imported <- list.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/', remove="*_clean.csv$"),
  by="imported.name")

weights <- readr::read_csv("/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/0 - Weights/FinalWeights_BySiteID_072816.csv")

## *Manually add Keith's vintage lookups*
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$ResInt_YearBuilt %in% c(-1, 0, 12, 95, 1040, 0000)] <- NA
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1996] <- 2007
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==2089] <- 2006
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1903] <- 1989
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1661] <- 1988
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1357] <- 1985
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1665] <- 1985
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1325] <- 1983
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==463] <- 1977
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1586] <- 1968
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==139] <- 1962
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1416] <- 1962
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1566] <- 1958
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1359] <- 1954
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1226] <- 1953
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==2075] <- 1951
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1843] <- 1950
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1422] <- 1949
SF_ri_custdat$ResInt_YearBuilt[SF_ri_custdat$siteid==1424] <- 1920

##  *Data cleaning used by Sarah / Keith* 
SFsmall_appliance$Portability[is.na(SFsmall_appliance$Portability)] <- "Portable Plug-In"

SFextlightingLamp$LightingLampsPerFixture <- as.numeric(SFextlightingLamp$LightingLampsPerFixture)
SFextlightingLamp <- SFextlightingLamp %>% left_join(select(SFextlighting, siteid, room_it, fixture_it, LightingFixtureQuantity))

SFlighting$LightingFixtureQuantity <- as.numeric(SFlighting$LightingFixtureQuantity)

SFlightingLamp$LightingLampsPerFixture <- as.numeric(SFlightingLamp$LightingLampsPerFixture)
SFlightingLamp <- SFlightingLamp %>% left_join(select(SFlighting, siteid, room_it, fixture_it, LightingFixtureQuantity))

SFtv$TV_Type[is.na(SFtv$TV_Type)] <- "Unknown"

SFventilation$VentType[is.na(SFventilation$VentType)] <- "Unknown"

## *Apply filters to drop placeholders*
HVACcooling <- HVACcooling %>% filter(HVACType!="MULTIFAMILY CENTRAL SYSTEM")
HVACheating <- HVACheating %>% filter((Fuel_clean=="Electric" | Fuel_clean=="Natural Gas") & Type_clean!="Multifamily Central System") 
SFcomputer <- SFcomputer %>% filter(Computer_Type!="Notebook") 
SFcomputerdisplay <- SFcomputerdisplay %>% filter(Flag_IntegratedDisplay==FALSE) 
SFdryer <- SFdryer %>% filter((DryerFuel=="Electricity" | DryerFuel=="Natural Gas") & is.na(ComboStacked_Unit)) 
SFsmall_appliance <- SFsmall_appliance %>% filter(Category!="Lighting")
SFventilation <- SFventilation %>% filter(! VentType %in% c("Bathroom Vent", "Ceiling Fan", "Laundry Room Fan")) 
SFwheater <- SFwheater %>% filter(WHFuel=="Electricity" | WHFuel=="Natural Gas") 


## *Calculate quantities within tables, where necessary*
HVACcooling$Qty <- rowSums(select(HVACcooling, UnitACQuantity, LUL_Qty), na.rm=TRUE)
HVACcooling$Qty[HVACcooling$Qty==0] <- 1

HVACheating$Qty <- rowSums(select(HVACheating, ElecResistQuantity, package_quantity, plugin_quantity, LUL_Qty), na.rm=TRUE)
HVACheating$Qty[HVACheating$Qty==0] <- 1

SFcookeq$Qty <- 1 + (SFcookeq$CooktopFuel!=SFcookeq$OvenFuel)*1
SFcookeq$Qty[is.na(SFcookeq$Qty)] <- 1

SFelectronics$Qty <- SFelectronics$elect_qty

SFextlighting$Qty <- SFextlighting$LightingFixtureQuantity

SFextlightingLamp$Qty <- SFextlightingLamp$LightingFixtureQuantity*SFextlightingLamp$LightingLampsPerFixture
SFextlightingLamp$Qty <- with(SFextlightingLamp, ifelse(is.na(Qty), LightingFixtureQuantity, Qty))

SFlighting$Qty <- SFlighting$LightingFixtureQuantity

SFlightingLamp$Qty <- SFlightingLamp$LightingFixtureQuantity*SFlightingLamp$LightingLampsPerFixture
SFlightingLamp$Qty <- with(SFlightingLamp, ifelse(is.na(Qty), LightingFixtureQuantity, Qty))

SFlrg_unusual_load$Qty <- SFlrg_unusual_load$LUL_Qty

SFventilation$Qty <- SFventilation$LUL_Qty

# *END OF IMPORT AND CLEANING*

# Measure Saturation Script
## *John C script for creating equipment saturation master table. This table will have a row for each equipment type and columns for with a T/F or 1/0 flag*

### Master table start
Master.EquipmentSat <- select(SFMaster_housegeometry, siteid, BuildType_rolled.y)
Master.EquipmentSat <- left_join(Master.EquipmentSat, weights, by = "siteid") # add weights
#### add HVAC
Master.EquipmentSat$HVAC_WindowAC <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "WINDOW SHAKER" & HVACcooling$siteid == x))}, numeric(1))
Master.EquipmentSat$HVAC_PTACWALL <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "THROUGH WALL" & HVACcooling$site == x))}, numeric(1))
Master.EquipmentSat$HVAC_PORTABLE <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "PORTABLE AC UNIT" & HVACcooling$site == x))}, numeric(1))

Master.EquipmentSat$HVAC_CAC <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "CENTRAL AIR" & HVACcooling$site == x))}, numeric(1)) ## CAC

Master.EquipmentSat$HVAC_Ductless <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "DUCTLESS MINI-SPLIT AIR CONDIT" & HVACcooling$siteid == x))}, numeric(1)) ## DHP

Master.EquipmentSat$EVAP <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACcooling, HVACcooling$HVACType == "EVAPORATIVE COOLER" & HVACcooling$siteid == x))}, numeric(1)) ## Evap Cooler

Master.EquipmentSat$FurnaceMotor <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(HVACheating, HVACheating$Type_clean == "Forced Air Furnace" & HVACheating$siteid == x))}, numeric(1)) ## Furnace Motors

# add large unusual loads
Master.EquipmentSat$WaterCircPump <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Water Circulation/Pump" & SFlrg_unusual_load$siteid == x))}, numeric(1)) 

Master.EquipmentSat$WellPump <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Well Pump" & SFlrg_unusual_load$siteid == x))}, numeric(1))

#add appliances
#clotheswashers
Master.EquipmentSat$CW <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFclwasher, SFclwasher$siteid == x))}, numeric(1))

Master.EquipmentSat$CWFront <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFclwasher, SFclwasher$WasherType == "Front-Loading" & SFclwasher$siteid == x))}, numeric(1))

Master.EquipmentSat$CWTop <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFclwasher, SFclwasher$WasherType == "Top-Loading" & SFclwasher$siteid == x))}, numeric(1))

#dryers
Master.EquipmentSat$Dryer <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFdryer, SFdryer$siteid == x))}, numeric(1))

Master.EquipmentSat$DryerElec <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFdryer, SFdryer$DryerFuel == "Electricity" & SFdryer$siteid == x))}, numeric(1))
  
Master.EquipmentSat$DryerGas <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFdryer, SFdryer$DryerFuel == "Natural Gas" & SFdryer$siteid == x))}, numeric(1))

#dishwashers
Master.EquipmentSat$DW <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFdishwasher, SFdishwasher$siteid == x))}, numeric(1))

#refrigeration
Master.EquipmentSat$Ref <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFrefrig, SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)" & SFrefrig$siteid == x))}, numeric(1))
  
Master.EquipmentSat$ESRef <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFrefrig, SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)" & (SFrefrig$RefEStar == 1) & SFrefrig$siteid == x))}, numeric(1))

Master.EquipmentSat$Freezer <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)") & SFrefrig$siteid == x))}, numeric(1))
  
Master.EquipmentSat$ESFreezer <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)") & (SFrefrig$RefEStar == 1) & SFrefrig$siteid == x))}, numeric(1))

# Electronics - TVs and STBs
Master.EquipmentSat$TV <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$siteid == x))}, numeric(1))

Master.EquipmentSat$TVCRT <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$siteid == x))}, numeric(1))

  
Master.EquipmentSat$TVCRT <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "CRT" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVLCD <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "LCD" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVLED <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "LED" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVOLED <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "OLED" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVPlasma <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "Plasma" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVProjection <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "Projection" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVUnknown <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_Type == "Unknown" & SFtv$siteid == x))}, numeric(1))
Master.EquipmentSat$TVSTB <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFtv, SFtv$TV_STBPresent == 1 & SFtv$siteid == x))}, numeric(1))

#Lighting
Master.EquipmentSat$Lighting_Int_INC <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, SFlightingLamp$LightingLampCategory == "Incandescent" & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_CFL <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, SFlightingLamp$LightingLampCategory == "Compact Fluorescent" & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_HAL <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, SFlightingLamp$LightingLampCategory == "Halogen" & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_LED <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, SFlightingLamp$LightingLampCategory == "LED" & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_LF <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, SFlightingLamp$LightingLampCategory == "Linear Fluorescent" & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_OTH <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Other" | SFlightingLamp$LightingLampCategory == "Rope Lamps") & SFlightingLamp$siteid == x))}, numeric(1))
Master.EquipmentSat$Lighting_Int_MissingNF <- vapply(Master.EquipmentSat$siteid, function(x) {nrow(subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Missing Lamp" | SFlightingLamp$LightingLampCategory == "Non-functional Lamp") & SFlightingLamp$siteid == x))}, numeric(1))

## saturation rate tables
SatTable_UnWeight <- Master.EquipmentSat %>% group_by(BuildType_rolled.y) %>% summarise_at(.vars = names(.)[c(10:45)], funs(mean), na.rm = T)

SatTable_Weight <- Master.EquipmentSat %>% group_by(BuildType_rolled.y) %>% summarise_at(.vars = names(.)[c(10:45)], funs(weighted.mean(.,w = weight_strata, na.rm = TRUE)))

SatTotal_UnWeight <- Master.EquipmentSat %>% summarise_at(.vars = names(.)[c(10:45)], funs(mean), na.rm = T)
SatTotal_UnWeight$BuildType_rolled.y <- NA
SatTotal_UnWeight <- select(SatTotal_UnWeight, BuildType_rolled.y, everything())
SatTotal_Weight <- Master.EquipmentSat %>% summarise_at(.vars = names(.)[c(10:45)], funs(weighted.mean(.,w = weight_strata, na.rm = TRUE)))
SatTotal_Weight$BuildType_rolled.y <- NA
SatTotal_Weight <- select(SatTotal_Weight, BuildType_rolled.y, everything())

SatTable_UnWeight <- rbind(SatTable_UnWeight, SatTotal_UnWeight)
SatTable_Weight <- rbind(SatTable_Weight, SatTotal_Weight)
rm(SatTotal_UnWeight, SatTotal_Weight)

SatTable_UnWeight_L <- gather(SatTable_UnWeight, Measure, Saturation, -BuildType_rolled.y)
SatTable_UnWeight_L <- spread(SatTable_UnWeight_L, BuildType_rolled.y, Saturation)
names(SatTable_UnWeight_L)[5] <- "Total"
library(readxl)
write.csv(PenTable.L, "/users/cornwell/desktop/PenTable.L.csv")

names <- colnames(PenTable)
write.csv(names, "/users/cornwell/desktop/names.csv")

Master.EquipmentPen.Unweighted <- Master.EquipmentPen
  Master.EquipmentPen.Unweighted[cols] <- lapply(Master.EquipmentPen[cols], function(x) x*1)
  PenTable.Unweighted <- Master.EquipmentPen.Unweighted %>% group_by(BuildType_rolled.y) %>% summarise_at(.vars = names(.)[c(4,10:45)], sum, na.rm = T)
 
  PenTable.Unweighted.L <- as.data.frame(t(PenTable.Unweighted))
  colnames(PenTable.Unweighted.L) = PenTable.Unweighted.L[1, ] # the first row will be the header
  PenTable.Unweighted.L = PenTable.Unweighted.L[-1, ]          # removing the first row.
  PenTable.Unweighted.L[, 1:3] <- lapply(PenTable.Unweighted.L[, 1:3], function(x) as.numeric((x)))
  PenTable.Unweighted.L <- mutate(PenTable.Unweighted.L, Total = rowSums(PenTable.Unweighted.L[, 1:3]))
  write.csv(PenTable.Unweighted.L, "/users/cornwell/desktop/PenTable.Unweighted.L.csv")
  
table(completed$BuildType_rolled
      )