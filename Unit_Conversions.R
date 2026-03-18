#### Script for unit coversions for discharge and stream chemistry
### DWH march 2026



#### Discharge ----
# mm/day to Liters/second streamflow conversion
# Converts streamflow in mm/day to Liters/second
# param mm Flow in mm/day
# param km2 Watershed area in km2

## Function
mm_to_Ls = function(mm, km2) {
  # mm/day * 1m/1000mm * (km2 * 1000000m2/1km2) * 1day/86400sec * 1000L/1m3
  Ls <- mm * (1/1000) * (km2 * 1000000) * (1/86400) * 1000
  return(Ls)
}

##test function; #HPB area is 4.299446 km^2
# mm_to_Ls(2, 4.299446)


#### Streamflow chem ----
## convert RHESSys g/m2/day to mg/L

gm2day_to_mgL = function(gm2day, mm) {
  #covert mm/day to m/day
  m <- mm/1000
  #divide g/m2/day by m/day: this results in g/m3. which is the same as mg/L
  mgL <- (gm2day / m)
  return(mgL)
}

## Test function
# gm2day_to_mgL(gm2day = 0.002, mm = 2)
















