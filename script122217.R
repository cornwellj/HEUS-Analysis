## setup
x <- c("ggmap", "rgdal", "rgeos", "maptools","plyr", "dplyr", "tidyr", "tmap", "reshape2")
install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

## use Sarah / Keith script for importing data for consistency
## --- Import data before QC flagging (only want observed equipment for this) -----
read.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/', remove="*.csv$") 
read.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/', remove="*_clean.csv$")

list.imported <- full_join(
  list.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/2 - Restructured Tables/', remove="*.csv$"),
  list.imported <- list.csv.folder('/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/', remove="*_clean.csv$"),
  by="imported.name")

weights <- readr::read_csv("/Volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/0 - Weights/FinalWeights_BySiteID_072816.csv")


# Manually add Keith's vintage lookups
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

## --- Data cleaning used by Sarah / Keith  -----
SFsmall_appliance$Portability[is.na(SFsmall_appliance$Portability)] <- "Portable Plug-In"

SFextlightingLamp$LightingLampsPerFixture <- as.numeric(SFextlightingLamp$LightingLampsPerFixture)
SFextlightingLamp <- SFextlightingLamp %>% left_join(select(SFextlighting, siteid, room_it, fixture_it, LightingFixtureQuantity))

SFlighting$LightingFixtureQuantity <- as.numeric(SFlighting$LightingFixtureQuantity)

SFlightingLamp$LightingLampsPerFixture <- as.numeric(SFlightingLamp$LightingLampsPerFixture)
SFlightingLamp <- SFlightingLamp %>% left_join(select(SFlighting, siteid, room_it, fixture_it, LightingFixtureQuantity))

SFtv$TV_Type[is.na(SFtv$TV_Type)] <- "Unknown"

SFventilation$VentType[is.na(SFventilation$VentType)] <- "Unknown"

## --- Apply filters to drop placeholders  -----
HVACcooling <- HVACcooling %>% filter(HVACType!="MULTIFAMILY CENTRAL SYSTEM")
HVACheating <- HVACheating %>% filter((Fuel_clean=="Electric" | Fuel_clean=="Natural Gas") & Type_clean!="Multifamily Central System") 
SFcomputer <- SFcomputer %>% filter(Computer_Type!="Notebook") 
SFcomputerdisplay <- SFcomputerdisplay %>% filter(Flag_IntegratedDisplay==FALSE) 
SFdryer <- SFdryer %>% filter((DryerFuel=="Electricity" | DryerFuel=="Natural Gas") & is.na(ComboStacked_Unit)) 
SFsmall_appliance <- SFsmall_appliance %>% filter(Category!="Lighting")
SFventilation <- SFventilation %>% filter(! VentType %in% c("Bathroom Vent", "Ceiling Fan", "Laundry Room Fan")) 
SFwheater <- SFwheater %>% filter(WHFuel=="Electricity" | WHFuel=="Natural Gas") 


## --- Calculate quantities within tables, where necessary -----
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

## John C script for creating equipment penetration master table. This table will have a column for each equipment type with a T/F or 1/0 flag.
## Master table start
Master.EquipmentPen <- select(SFMaster_housegeometry, siteid, BuildType_rolled.y)
Master.EquipmentPen <- left_join(Master.EquipmentPen, weights, by = "siteid") # add weights
# add HVAC
Master.EquipmentPen$HVAC_WindowAC <- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "WINDOW SHAKER")$siteid ## Window AC
Master.EquipmentPen$HVAC_PTACWALL <- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "THROUGH WALL")$siteid ## PTAC through wall units
Master.EquipmentPen$HVAC_PORTABLE<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "PORTABLE AC UNIT")$siteid ## Portable units
Master.EquipmentPen$CAC<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "CENTRAL AIR")$siteid ## CAC
Master.EquipmentPen$DHP<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "DUCTLESS MINI-SPLIT AIR CONDIT")$siteid ## DHP
Master.EquipmentPen$EVAP<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "EVAPORATIVE COOLER")$siteid ## Evap Cooler
Master.EquipmentPen$FurnaceMotor<- Master.EquipmentPen$siteid %in% subset(HVACheating, HVACheating$Type_clean == "Forced Air Furnace")$siteid # Furnace motors

# add large unusual loads
Master.EquipmentPen$WaterCircPump <- Master.EquipmentPen$siteid %in% subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Water Circulation/Pump")$siteid ## Water circulation pumps
Master.EquipmentPen$WellPump <- Master.EquipmentPen$siteid %in% subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Well Pump")$siteid ## Well pumps
#add appliances
Master.EquipmentPen$CW <- 
  Master.EquipmentPen$siteid %in% SFclwasher$siteid ## Clothes Washer
Master.EquipmentPen$CWFront <- 
  Master.EquipmentPen$siteid %in% subset(SFclwasher, SFclwasher$WasherType == "Front-Loading")$siteid ## Front loading clotheswasher
Master.EquipmentPen$CWTop <- 
  Master.EquipmentPen$siteid %in% subset(SFclwasher, SFclwasher$WasherType == "Top-Loading")$siteid ## Top loading clothes washer

Master.EquipmentPen$Dryer <- Master.EquipmentPen$siteid %in% SFdryer$siteid ## Dryer
Master.EquipmentPen$DryerElec <- Master.EquipmentPen$siteid %in% subset(SFdryer, SFdryer$DryerFuel == "Electricity")$siteid ## Electric dryer
Master.EquipmentPen$DryerGas <- Master.EquipmentPen$siteid %in% subset(SFdryer, SFdryer$DryerFuel == "Natural Gas")$siteid ## Gas dryer

Master.EquipmentPen$DW <- Master.EquipmentPen$siteid %in% SFdishwasher$siteid ## Dishwasher

Master.EquipmentPen$Ref <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)"))$siteid ## Refrigerator
Master.EquipmentPen$ESRef <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)") & (SFrefrig$RefEStar == 1))$siteid ## Refrigerator Energy Star

Master.EquipmentPen$Freezer <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)"))$siteid ## Freezer 
Master.EquipmentPen$ESFreezer <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)") & (SFrefrig$RefEStar == 1))$siteid ##Freezer Energy Star

# Electronics - TVs and STBs
Master.EquipmentPen$TV <- Master.EquipmentPen$siteid %in% SFtv$siteid 
Master.EquipmentPen$TVCRT <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "CRT"))$siteid
Master.EquipmentPen$TVLCD <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "LCD"))$siteid
Master.EquipmentPen$TVLED <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "LED"))$siteid
Master.EquipmentPen$TVOLED <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "OLED"))$siteid
Master.EquipmentPen$TVPlasma <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "Plasma"))$siteid
Master.EquipmentPen$TVProjection <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "Projection"))$siteid
Master.EquipmentPen$TVUnknown <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_Type == "Unknown"))$siteid
Master.EquipmentPen$TVSTB <- Master.EquipmentPen$siteid %in% subset(SFtv, (SFtv$TV_STBPresent == 1))$siteid

#Lighting
Master.EquipmentPen$Lighting_Int_INC <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Incandescent"))$siteid
Master.EquipmentPen$Lighting_Int_CFL <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Compact Fluorescent"))$siteid
Master.EquipmentPen$Lighting_Int_HAL <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Halogen"))$siteid
Master.EquipmentPen$Lighting_Int_LED <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "LED"))$siteid
Master.EquipmentPen$Lighting_Int_LF <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Linear Fluorescent"))$siteid
Master.EquipmentPen$Lighting_Int_Other <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Other" | SFlightingLamp$LightingLampCategory == "Rope Lamps"))$siteid
Master.EquipmentPen$Lighting_Int_Missing_NF <- Master.EquipmentPen$siteid %in% subset(SFlightingLamp, (SFlightingLamp$LightingLampCategory == "Missing Lamp" | SFlightingLamp$LightingLampCategory == "Non-functional Lamp"))$siteid

## transform factors to numeric - T/F to 1/0
cols <- sapply(Master.EquipmentPen, is.logical)
Master.EquipmentPen[cols] <- lapply(Master.EquipmentPen[cols], function(x) as.numeric((x)))

## assign proportion weights to cells
Master.EquipmentPen[cols] <- lapply(Master.EquipmentPen[cols], function(x) x*Master.EquipmentPen$weight_strata_prop)

## penetration rate tables
PenTable <- Master.EquipmentPen %>% group_by(BuildType_rolled.y) %>% summarise_at(.vars = names(.)[c(4,10:45)], sum, na.rm = T)




PenTable.L <- as.data.frame(t(PenTable))
colnames(PenTable.L) = PenTable.L[1, ] # the first row will be the header
PenTable.L = PenTable.L[-1, ]          # removing the first row.
PenTable.L[, 1:3] <- lapply(PenTable.L[, 1:3], function(x) as.numeric((x)))
x <- mutate(x, Total = rowSums(x[, 2:3]))
PenTable.L <- mutate(PenTable.L, Total = rowSums(PenTable.L[, 1:3]))


cols <- sapply(Master.EquipmentPen)
Master.EquipmentPen[cols] <- lapply(Master.EquipmentPen[cols], function(x) as.numeric((x)))
Master.EquipmentPen[cols] <- lapply(Master.EquipmentPen[cols], function(x) x*Master.EquipmentPen$weight_strata_prop)

library(xlsx)
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

HVAC_Window_M <- subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "WINDOW SHAKER")
HVAC_Window_M_C <- HVAC_Window_M %>% group_by(HVACManu, HVACModel_clean) %>% summarise(n())
Master.EquipmentPen$HVAC_WindowAC <- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "WINDOW SHAKER")$siteid
Master.EquipmentPen$HVAC_PTACWALL <- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "THROUGH WALL")$siteid
Master.EquipmentPen$HVAC_PORTABLE<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "PTAC" & HVACcooling$PTACType == "PORTABLE AC UNIT")$siteid
Master.EquipmentPen$CAC<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "CENTRAL AIR")$siteid
Master.EquipmentPen$DHP<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "DUCTLESS MINI-SPLIT AIR CONDIT")$siteid
Master.EquipmentPen$EVAP<- Master.EquipmentPen$siteid %in% subset(HVACcooling, HVACcooling$HVACType == "EVAPORATIVE COOLER")$siteid
Master.EquipmentPen$FurnaceMotor<- Master.EquipmentPen$siteid %in% subset(HVACheating, HVACheating$Type_clean == "Forced Air Furnace")$siteid

# add LUL
Master.EquipmentPen$WaterCircPump <- Master.EquipmentPen$siteid %in% subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Water Circulation/Pump")$siteid
Master.EquipmentPen$WellPump <- Master.EquipmentPen$siteid %in% subset(SFlrg_unusual_load, SFlrg_unusual_load$End.Use == "Well Pump")$siteid
#add appliances
Master.EquipmentPen$CW <- Master.EquipmentPen$siteid %in% SFclwasher$siteid
Master.EquipmentPen$CWFront <- Master.EquipmentPen$siteid %in% subset(SFclwasher, SFclwasher$WasherType == "Front-Loading")$siteid
Master.EquipmentPen$CWTop <- Master.EquipmentPen$siteid %in% subset(SFclwasher, SFclwasher$WasherType == "Top-Loading")$siteid

Master.EquipmentPen$Dryer <- Master.EquipmentPen$siteid %in% SFdryer$siteid
Master.EquipmentPen$DryerElec <- Master.EquipmentPen$siteid %in% subset(SFdryer, SFdryer$DryerFuel == "Electricity")$siteid
Master.EquipmentPen$DryerGas <- Master.EquipmentPen$siteid %in% subset(SFdryer, SFdryer$DryerFuel == "Natural Gas")$siteid

Master.EquipmentPen$DW <- Master.EquipmentPen$siteid %in% SFdishwasher$siteid

Master.EquipmentPen$Ref <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)"))$siteid
Master.EquipmentPen$ESRef <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle != "Upright Freezer" & SFrefrig$RefStyle != "Chest Freezer" & SFrefrig$RefStyle != "Ice Maker (standalone)") & (SFrefrig$RefEStar == 1))$siteid

Master.EquipmentPen$Freezer <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)"))$siteid
Master.EquipmentPen$ESFreezer <- Master.EquipmentPen$siteid %in% subset(SFrefrig, (SFrefrig$RefStyle == "Upright Freezer" | SFrefrig$RefStyle == "Chest Freezer" | SFrefrig$RefStyle == "Ice Maker (standalone)") & (SFrefrig$RefEStar == 1))$siteid

