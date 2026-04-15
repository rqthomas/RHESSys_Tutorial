getwd()

setwd("RHESSys_Tutorial/HPB_files_NewMaps/")
getwd()
#devtools::install_github("RHESSys/RHESSysIOinR")
library(RHESSysIOinR)

dates = c("1950 1 1 1", "2350 1 2 24") #has to end with a 24 hour so that the tec output will work

#dates = c("1950 1 1 1", "1952 1 2 24") #has to end with a 24 hour so that the tec output will work

name = "hpb_RouteTest_1"

output_folder = "out/apr13RouteTest/COWTest3"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

input_rhessys = IOin_rhessys_input(
  version          = "../../RHESSys/rhessys/rhessys7.5",
  tec_file         =  paste0("tecfiles/",name,".tec"), #for using tecfile built below
  world_file       = "worldfiles/hpb.world", #what was made in pre-processing
  world_hdr_prefix = "hpb_spintest",
  flowtable        = "worldfiles/hpb.flow", #what was made in pre-processing
  start            = dates[1],
  end              = dates[2],
  output_folder    = output_folder,
  output_prefix    = name,
  commandline_options = "-b -g -climrepeat"
  #commandline_options = "-b -g -climrepeat -str worldfiles/stream.hpb -stro"
)

#header directly copied from hpb.hdr
input_hdr = IOin_hdr(
  basin      = "defs/basin.def",
  hillslope  = "defs/hill.def",
  zone       = "defs/zone.def",
  soil       = "defs/soil_siltyloam.def",
  landuse    = "defs/lu_undev.def",
  stratum    = "defs/veg_deciduous.def",
  basestations = "clim/ccr_base"
)

#set param list
pars_list = list(
  ## set hydro parameters
  list(input_hdr$soil_def[1], "m", 0.108),
  list(input_hdr$soil_def[1], "Ksat_0",  404.3),
  list(input_hdr$soil_def[1], "sat_to_gw_coeff",  0.1),
  list(input_hdr$hillslope_def[1], "gw_loss_coeff", 0.9)
    )

# #set up manually tweak params for rhessys format; can also put multi here  #when you run this it will generate new folders within def called zone, soil, etc, that have updated def file values, but doesn't change original files
input_def_pars = IOin_def_pars_simple(pars_list)


#make tec file locally
input_tec_data = IOin_tec_std(start = dates[1], end = dates[2], output_state = T)


#run model with inputs defined above
run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files     = input_hdr,
  tec_data      = input_tec_data, #can be NULL if calling a local file and not using IOin_tec_std
  def_pars      = input_def_pars,
  output_filter = NULL, #cant get any version working currently so using legacy output
  return_cmd    = F   # flip to FALSE to run
)



########################################
#### Run 5 years off of spinup ####


dates = c("2020 1 1 1", "2025 1 2 24") #has to end with a 24 hour so that the tec output will work

name = "hpb_RouteTest_SR1"

input_rhessys = IOin_rhessys_input(
  version          = "../../RHESSys/rhessys/rhessys7.5",
  tec_file         =  paste0("tecfiles/",name,".tec"), #for using tecfile built below
  world_file       = "worldfiles/hpb.world.Y2350M1D2H23.state", #what was made in pre-processing
  world_hdr_prefix = name,
  flowtable        = "worldfiles/hpb.flow", #what was made in pre-processing
  start            = dates[1],
  end              = dates[2],
  output_folder    = output_folder,
  output_prefix    = name,
  # commandline_options = "-b -g -climrepeat" #if adding -p add before -g to get patch grow
  commandline_options = "-b -g -climrepeat -str worldfiles/stream.hpb -stro"
)

#header directly copied from hpb.hdr
input_hdr = IOin_hdr(
  basin      = "defs/basin.def",
  hillslope  = "defs/hill.def",
  zone       = "defs/cow_zone.def",
  soil       = "defs/soil_siltyloam.def",
  landuse    = "defs/lu_undev.def",
  stratum    = "defs/cow_veg_deciduous.def",
  basestations = "clim/ccr_base"
)

#make tec file locally
input_tec_data = IOin_tec_std(start = dates[1], end = dates[2], output_state = F)


#run model with inputs defined above
run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files     = input_hdr,
  tec_data      = input_tec_data, #can be NULL if calling a local file and not using IOin_tec_std
  def_pars      = input_def_pars,
  output_filter = NULL, #cant get any version working currently so using legacy output
  return_cmd    = F   # flip to FALSE to run
)




#### Look at output ####
#read in validation csv
# target_df <- read_csv("../Target_Data_comp/TargetData_15mar26.csv") |>
#   rename(date = Date)
#
# #get plotting function
# source('read_plot_rhessys_fun.R')
#
# read_plot_rhessys(outname = "out/RouteTest100yr/hpb_RouteTest_1", date_filter = as.Date("1950-01-01"),
#  ncol = 5, validate = F)
#
#
# ##read in streamrouting
# col_names <- c("day","month","year","reachID","Qout","lateralinput","Qin","waterdepth","reservoir.store","NO3_out","NH4_out","DON_out","DOC_out")
# df <- read.table("out/RouteTest/hpb_streamroute_test_streamrouting.daily",
#                  header = TRUE, col.names = col_names, fill = TRUE)
# head(df)
