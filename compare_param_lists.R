### Compare two sets of parameters
library(tidyverse)

#### Two functions to ----
#Parse into data frames
parse_def = function(lines) {
  lines = lines[nzchar(trimws(lines))]   # remove blank lines
  lines = lines[!grepl("^#", lines)]     # remove comments
  parts = strsplit(trimws(lines), "\\s+")
  df = data.frame(
    value = sapply(parts, `[`, 1),
    param = sapply(parts, `[`, 2),
    stringsAsFactors = FALSE
  )
  df$value = as.numeric(df$value)
  df
} #end fun

#compare fun
comp_fun <- function(a, b){
  a_df = parse_def(a) |> rename(a = value)
  b_df = parse_def(b) |> rename(b = value)

  comparison <- full_join(a_df, b_df, by = "param") |>
    mutate(
      difference  = a - b,
      pct_diff     = round((a - b) / b * 100, 1),
      is_different = a != b   )

  return(comparison)
} #end fun


##### Read in data def files as list of parms ----

#soils from param library
silty <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_siltyloam.def") #ID 8
sandy <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_sandyloam.def") #ID 12
loam <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_loam.def") #ID 9

#compare params I've used to coweeta paper
my_Decid <- readLines("./HPB_files_NewMaps/defs/veg_deciduousOG.def") #
cow_Decid <- readLines("C:/Users/dwh18/Downloads/416d1d3b-7e69-4c4b-ac7f-1d06bb8b6944/rhessysWS18/rhessys_ws18_local/defs/stratum_deciduous.def")

my_Siltloam <- readLines("./HPB_files_NewMaps/defs/soil_siltyloam.def") #
cow_Siltloam <- readLines("C:/Users/dwh18/Downloads/416d1d3b-7e69-4c4b-ac7f-1d06bb8b6944/rhessysWS18/rhessys_ws18_local/defs/soil_silt-loam.def")

my_Evgreen <- readLines("C:/Users/dwh18/OneDrive/Desktop/R_Projects/CCR_RHESSys/HPB_files_NEW/defs/veg_evergreen.def")
cow_Evgreen <- readLines("C:/Users/dwh18/Downloads/416d1d3b-7e69-4c4b-ac7f-1d06bb8b6944/rhessysWS18/rhessys_ws18_local/defs/stratum_evergreen.def")

my_zone <- readLines("./HPB_files_NewMaps/defs/zone.def") #
cow_zone <- readLines("C:/Users/dwh18/Downloads/416d1d3b-7e69-4c4b-ac7f-1d06bb8b6944/rhessysWS18/rhessys_ws18_local/defs/zone_zone.def")




#### Compare defs I've been using to coweeta

loam_comp <- comp_fun(my_Siltloam, cow_Siltloam)

decid_comp <- comp_fun(my_Decid, cow_Decid)
colnames(decid_comp)[c(1, 3)] <- c("Rhessys_df", "Coweeta_def")

# write.csv(decid_comp, "C:/Users/dwh18/Downloads/RHESSys_decid_def_comp.csv", row.names = F)

evergreen_comp <- comp_fun(my_Evgreen, cow_Evgreen)

zone_comp <- comp_fun(my_zone, cow_zone)

















# # old soil comp
# #### Soil comp
# silty_df = parse_def(silty) |> rename(siltyloam = value)
# sandy_df = parse_def(sandy) |> rename(sandyloam = value)
# loam_df = parse_def(loam) |> rename(loam = value)
#
#
#
# # Join and find differences
# comparison = full_join(silty_df, sandy_df, by = "param") |>
#   full_join(loam_df, by = "param") |>
#   mutate(
#     difference_silt_sand   = siltyloam - sandyloam,
#     pct_diff_silt_sand     = round((siltyloam - sandyloam) / sandyloam * 100, 1),
#     difference_silt_loam  = siltyloam - loam,
#     pct_diff_silt_loam     = round((siltyloam - loam) / sandyloam * 100, 1),
#     is_different = siltyloam != sandyloam,
#     is_different2 = siltyloam != loam
#   ) |>
#   arrange(desc(abs(difference_silt_sand)))
