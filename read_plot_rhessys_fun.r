

##function to read in and plot RHESSys outputs quickly
#libraries
library(RHESSysIOinR)
library(tidyverse)

#unit conversion functions
# source("../Unit_Conversions.R")
#### Unit coversions

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







#### function
read_plot_rhessys = function(outname, date_filter = NULL, ncol = 3, validate = FALSE,
                              vars = c("lai.x", "plantc.x", "soilc", "plantn", "soiln",
                                       "streamflow", "unsat_stor", "sat_def",
                                       "streamflow_DON", "streamflow_DOC", "streamflow_NO3")) {

  dat = readin_rhessys_output(outname)

  out = left_join(dat$bd, dat$bdg,
                  by = c("day", "month", "year", "basinID", "wy", "yd", "date"))

  if (!is.null(date_filter)) out = out |> filter(date > date_filter)

  # Base model output with unit conversions
  out_processed = out |>
    select(date, year, yd, any_of(vars)) |>
    mutate(
      soil_moisture    = sat_def - unsat_stor,
      streamflow_L_s   = mm_to_Ls(streamflow, 4.299446),
      DOC_mgL          = gm2day_to_mgL(streamflow_DOC, streamflow),
      NO3_mgL          = gm2day_to_mgL(streamflow_NO3, streamflow)
    )

  # -------------------- Standard plot (validate = FALSE) --------------------
  if (!validate) {
    out_processed |>
      pivot_longer(-c(date, year, yd)) |>
      ggplot(aes(x = date, y = value)) +
      geom_point(size = 0.5) +
      facet_wrap(~name, scales = "free_y", ncol = ncol) +
      labs(title = outname, x = "Date", y = NULL) +
      theme_bw()

  # -------------------- Validation plot (validate = TRUE) --------------------
  } else {

    # if (!file.exists("target_data.csv")) {
    #   stop("target_data.csv not found in working directory: ", getwd())
    # }
    #
    # target = read_csv("target_data.csv", show_col_types = FALSE) |>
    #   mutate(date = as.Date(date))

    target <- target_df

    val_start = as.Date("2020-01-01")
    val_end   = as.Date("2026-12-31")

    # -------------------- Plot 1: Multi-variable validation --------------------
    # Uses Q_L_s_Flowmate as the streamflow target
    modeled_long = out_processed |>
      filter(date >= val_start, date <= val_end) |>
      select(date,
             lai     = lai.x,
             DOC_mgL,
             NO3_mgL,
             Q_L_s   = streamflow_L_s) |>
      pivot_longer(-date, names_to = "variable", values_to = "modeled")

    target_long = target |>
      filter(date >= val_start, date <= val_end) |>
      select(date,
             lai     = lai_MODIS,
             DOC_mgL,
             NO3_mgL,
             Q_L_s   = Q_L_s_Flowmate) |>   # Flowmate for main validation plot
      pivot_longer(-date, names_to = "variable", values_to = "observed")

    combined = left_join(modeled_long, target_long, by = c("date", "variable"))

    p1 = ggplot(combined, aes(x = date)) +
      geom_line(aes(y = modeled, color = "Modeled"), linewidth = 0.5) +
      geom_point(aes(y = observed, color = "Observed"), size = 1.5, na.rm = TRUE) +
      scale_color_manual(values = c("Modeled"  = "steelblue",
                                     "Observed" = "firebrick")) +
      facet_wrap(~variable, scales = "free_y", ncol = 2) +
      labs(title    = paste("Validation:", outname),
           subtitle = paste("Flowmate Q |", val_start, "to", val_end),
           x = "Date", y = NULL, color = NULL) +
      theme_bw() +
      theme(legend.position = "bottom")

    # -------------------- Plot 2: PT streamflow only from 2024 --------------------
    pt_start = as.Date("2024-01-01")

    modeled_q = out_processed |>
      filter(date >= pt_start, date <= val_end) |>
      select(date, Q_L_s = streamflow_L_s)

    target_q = target |>
      filter(date >= pt_start, date <= val_end) |>
      select(date, Q_L_s_PT)

    combined_q = left_join(modeled_q, target_q, by = "date")

    p2 = ggplot(combined_q, aes(x = date)) +
      geom_line(aes(y = Q_L_s, color = "Modeled"), linewidth = 0.5) +
      geom_line(aes(y = Q_L_s_PT, color = "Observed (PT)"),
                 size = 0.5, na.rm = TRUE) +
      scale_color_manual(values = c("Modeled"       = "steelblue",
                                     "Observed (PT)" = "firebrick")) +
      labs(title    = paste("Streamflow validation:", outname),
           subtitle = paste("PT gauge | 2024 onwards"),
           x = "Date", y = "Q (L/s)", color = NULL) +
      theme_bw() +
      theme(legend.position = "bottom")

    # Print both plots
    print(p1)
    print(p2)
  }
}
