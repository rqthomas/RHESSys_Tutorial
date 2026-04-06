#### Trying stream routing generation in R
#https://github.com/RHESSys/RHESSys/wiki/In-Stream-Routing

library(terra)

##check on data
getwd()
s <- rast("./HPB_files_NewMaps/spatial_data/hpb_stream750.tif")
plot(s, type = "classes")
cat("Reach IDs found:", sort(unique(na.omit(values(s)))), "\n")
cat("Number of reaches:", length(unique(na.omit(values(s)))), "\n")

### Function to make streamtable
generate_streamtable <- function(
    stream_rast,       # stream raster with reach IDs matching subbasin IDs
    dem_rast,          # DEM
    patch_rast,        # patch raster
    subbasin_rast,     # subbasin raster (reach IDs match these)
    hill_rast,         # hillslope raster (2 per subbasin, sequentially numbered)
    output_file,
    ManningsN      = 0.035,
    streamTopWidth = 2.0,
    streamBotWidth = 1.0,
    streamDepth    = 0.5
) {
  stream   <- rast(stream_rast)
  dem      <- rast(dem_rast)
  patch    <- rast(patch_rast)
  subbasin <- rast(subbasin_rast)
  hill     <- rast(hill_rast)

  reach_ids <- sort(unique(na.omit(values(stream))))
  n_reaches <- length(reach_ids)
  message("Found ", n_reaches, " stream reaches.")

  # --- Build hillslope-to-subbasin lookup from your numbering scheme ---
  # Basin 2 -> hills 1,2 | Basin 4 -> hills 3,4 | Basin 6 -> hills 5,6
  # Pattern: hill IDs = (basin_id - 2) + 1 and (basin_id - 2) + 2
  # i.e. for basin b: hills (b/2)*2 - 1 and (b/2)*2
  # We derive this empirically from the rasters to be safe

  hill_vals    <- values(hill)
  subbasin_vals <- values(subbasin)

  # For each cell, record which subbasin its hillslope belongs to
  valid <- !is.na(hill_vals) & !is.na(subbasin_vals)
  hill_sub_df <- unique(data.frame(
    hill_id    = hill_vals[valid],
    subbasin_id = subbasin_vals[valid]
  ))
  # Each hillslope ID maps to exactly one subbasin
  hill_to_sub <- setNames(hill_sub_df$subbasin_id, hill_sub_df$hill_id)

  # --- Build reach topology from DEM ---
  # Mean elevation per reach to determine upstream/downstream order
  reach_elev <- sapply(reach_ids, function(rid) {
    cells <- which(values(stream) == rid)
    mean(values(dem)[cells], na.rm = TRUE)
  })
  names(reach_elev) <- reach_ids

  # Flow topology: for each reach, downstream reach is the one it flows into
  # We find this by looking at the outlet cell of each reach (lowest elevation)
  # and checking which reach ID is in its downstream neighborhood

  flow_dir <- terrain(dem, v = "flowdir")  # terra's flow direction

  get_downstream_reach <- function(rid) {
    cells <- which(values(stream) == rid)
    elev_vals <- values(dem)[cells]
    outlet_cell <- cells[which.min(elev_vals)]

    # Walk downstream from outlet until we hit a different reach ID
    visited <- c()
    current <- outlet_cell
    for (i in 1:20) {  # max steps to find next reach
      nbrs <- adjacent(stream, current, directions = 8)[1, ]
      nbrs <- nbrs[!is.na(nbrs)]
      nbr_reach_ids <- values(stream)[nbrs]

      # Check if any neighbor belongs to a different reach
      different <- nbrs[!is.na(nbr_reach_ids) & nbr_reach_ids != rid]
      if (length(different) > 0) {
        return(values(stream)[different[1]])
      }

      # Otherwise follow flow direction downhill
      nbr_elevs <- values(dem)[nbrs]
      if (all(is.na(nbr_elevs))) break
      current <- nbrs[which.min(nbr_elevs)]
      if (current %in% visited) break
      visited <- c(visited, current)
    }
    return(NA)  # outlet reach, no downstream
  }

  message("Building reach topology...")
  downstream_of <- setNames(
    sapply(reach_ids, get_downstream_reach),
    reach_ids
  )

  # Invert to get upstream reaches for each reach
  upstream_of <- lapply(reach_ids, function(rid) {
    reach_ids[!is.na(downstream_of) & downstream_of == rid]
  })
  names(upstream_of) <- reach_ids

  # --- Build adjacent patch/hillslope info per reach ---
  message("Finding adjacent patches and hillslopes...")

  get_adjacent_patches <- function(rid) {
    reach_cells <- which(values(stream) == rid)

    # Get all neighboring cells that are NOT stream
    nbr_cells <- unique(unlist(lapply(reach_cells, function(cell) {
      nbrs <- adjacent(stream, cell, directions = 8)[1, ]
      nbrs <- nbrs[!is.na(nbrs)]
      nbrs[is.na(values(stream)[nbrs])]  # non-stream neighbors
    })))

    if (length(nbr_cells) == 0) return(data.frame())

    adj_df <- data.frame(
      patch_id = values(patch)[nbr_cells],
      hill_id  = values(hill)[nbr_cells]
    )
    adj_df <- na.omit(adj_df)
    adj_df <- unique(adj_df)

    # zone_id = subbasin_id for each hillslope (since zone == subbasin in your setup)
    adj_df$zone_id <- hill_to_sub[as.character(adj_df$hill_id)]

    return(adj_df)
  }

  # --- Compute reach geometry from DEM ---
  get_reach_geometry <- function(rid) {
    cells <- which(values(stream) == rid)
    elev_vals <- values(dem)[cells]

    elev_range <- diff(range(elev_vals, na.rm = TRUE))
    res_m      <- res(dem)[1]
    length_m   <- length(cells) * res_m
    slope      <- max(elev_range / length_m, 0.0001)

    list(length = round(length_m, 2), slope = round(slope, 6))
  }

  # --- Write streamtable ---
  message("Writing streamtable to: ", output_file)
  con <- file(output_file, "w")
  writeLines(as.character(n_reaches), con)

  for (rid in reach_ids) {
    geom    <- get_reach_geometry(rid)
    adj     <- get_adjacent_patches(rid)
    n_adj   <- nrow(adj)

    ups   <- upstream_of[[as.character(rid)]]
    downs <- downstream_of[as.character(rid)]
    downs <- downs[!is.na(downs)]

    # Header: reachID botWidth topWidth maxHeight slope Manning length nAdjacentPatches
    line <- paste(
      rid,
      streamBotWidth,
      streamTopWidth,
      streamDepth,
      geom$slope,
      ManningsN,
      geom$length,
      n_adj
    )

    # Adjacent patch triplets: patchID zoneID hillID
    if (n_adj > 0) {
      triplets <- paste(
        apply(adj, 1, function(row)
          paste(row["patch_id"], row["zone_id"], row["hill_id"])
        ),
        collapse = " "
      )
      line <- paste(line, triplets)
    }

    # Upstream reaches
    line <- paste(line, length(ups), paste(ups, collapse = " "))

    # Downstream reaches
    line <- paste(line, length(downs), paste(downs, collapse = " "))

    writeLines(trimws(line), con)
  }

  close(con)
  message("Done. Streamtable has ", n_reaches, " reaches.")

} #end of function


#### use function
generate_streamtable(
  stream_rast   = "./HPB_files_NewMaps/spatial_data/hpb_stream750.tif",
  dem_rast      = "./HPB_files_NewMaps/spatial_data/hpb_dem.tif",
  patch_rast    = "./HPB_files_NewMaps/spatial_data/hpb_patch_map.tif",
  subbasin_rast = "./HPB_files_NewMaps/spatial_data/hpb_basin750_filled_6apr26.tif",
  hill_rast     = "./HPB_files_NewMaps/spatial_data/hpb_halfbasin750_filled_6apr26.tif",
  output_file   = "./HPB_files_NewMaps/stream.hpb",
  ManningsN      = 0.035,
  streamTopWidth = 3.0,
  streamBotWidth = 1.5,
  streamDepth    = 0.6
)




### function to readin file to check
read_streamtable <- function(file) {
  lines <- readLines(file)
  n_reaches <- as.integer(lines[1])

  rows <- lapply(lines[-1], function(line) {
    vals <- as.numeric(strsplit(trimws(line), "\\s+")[[1]])
    idx <- 1

    reach_id   <- vals[idx]; idx <- idx + 1
    bot_width  <- vals[idx]; idx <- idx + 1
    top_width  <- vals[idx]; idx <- idx + 1
    max_height <- vals[idx]; idx <- idx + 1
    slope      <- vals[idx]; idx <- idx + 1
    manning    <- vals[idx]; idx <- idx + 1
    length_m   <- vals[idx]; idx <- idx + 1
    n_adj      <- vals[idx]; idx <- idx + 1

    # Read all patch/zone/hill triplets
    patch_ids <- zone_ids <- hill_ids <- c()
    if (n_adj > 0) {
      for (i in 1:n_adj) {
        patch_ids <- c(patch_ids, vals[idx]);     idx <- idx + 1
        zone_ids  <- c(zone_ids,  vals[idx]);     idx <- idx + 1
        hill_ids  <- c(hill_ids,  vals[idx]);     idx <- idx + 1
      }
    }

    n_up     <- vals[idx]; idx <- idx + 1
    up_ids   <- if (n_up > 0) vals[idx:(idx + n_up - 1)] else NA
    idx      <- idx + n_up

    n_down   <- vals[idx]; idx <- idx + 1
    down_ids <- if (n_down > 0) vals[idx:(idx + n_down - 1)] else NA

    data.frame(
      reach_id       = reach_id,
      bot_width      = bot_width,
      top_width      = top_width,
      max_height     = max_height,
      slope          = slope,
      manning        = manning,
      length_m       = length_m,
      n_adj_patches  = n_adj,
      patch_ids      = paste(patch_ids, collapse = ","),
      zone_ids       = paste(zone_ids,  collapse = ","),
      hill_ids       = paste(hill_ids,  collapse = ","),
      n_upstream     = n_up,
      upstream_ids   = paste(up_ids,   collapse = ","),
      n_downstream   = n_down,
      downstream_ids = paste(down_ids, collapse = ",")
    )
  })

  do.call(rbind, rows)
} #end function



####read in and check file
st <- read_streamtable("./HPB_files_NewMaps/stream.yourwatershed")





