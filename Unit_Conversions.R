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

##test function; #HPB is 4.299446 km^2
mm_to_Ls(2, 4.299446)


#### Streamflow chem ----
## convert RHESSys g/m2/day to mg/L

gm2day_to_mgL = function(gm2day, mm) {
  # area cancels — both numerator and denominator are per m2
  #covert mm/day to m/day
  m <- mm/1000
  #divide g/m2/day by m/day: this results in g/m3. which is the same as mg/L
  mgL <- (gm2day / m)
  return(mgL)
}


#The previous unit conversion I'd been using where I normalized to area for flux and flow
Previous_gm2daytomgL = function(gm2day, mm, km2) {

  cmd <- mm * (1/1000) * (km2 * 1000000)

  chem_gN_day <- gm2day * (km2 * 1000000)

  chem_g_m3 <- chem_gN_day / cmd
  #gm3 is same as mg/L
  return(chem_g_m3)

  # mutate(streamflow_cmday = (streamflow / 1000 ) * (4241374),
  #        streamflow_cms = streamflow_cmday / 86400) |>
  #   mutate(rhessys_Q_L_s = streamflow_cms * 1000) |>
  #   mutate(NO3_gN_day = streamflow_NO3 * (4241374)) |>
  #   mutate(Q_L_day = rhessys_Q_L_s * 86400) |>
  #   mutate(NO3_g_L = NO3_gN_day / Q_L_day) |>
  #   mutate(NO3_mg_L = NO3_g_L * 1000)

}


gm2day_to_mgL(gm2day = 0.002, mm = 2)

Previous_gm2daytomgL(gm2day = 0.002, mm = 2, km2 = 4.299446)















