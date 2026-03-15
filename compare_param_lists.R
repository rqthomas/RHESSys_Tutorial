silty <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_siltyloam.def") #ID 8
sandy <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_sandyloam.def") #ID 12
loam <- readLines("https://raw.githubusercontent.com/RHESSys/ParameterLibrary/refs/heads/master/Soil/soil_loam.def") #ID 9


# Parse into data frames
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
}

silty_df = parse_def(silty) |> rename(siltyloam = value)
sandy_df = parse_def(sandy) |> rename(sandyloam = value)
loam_df = parse_def(loam) |> rename(loam = value)


# Join and find differences
comparison = full_join(silty_df, sandy_df, by = "param") |>
  full_join(loam_df, by = "param") |>
  mutate(
    difference_silt_sand   = siltyloam - sandyloam,
    pct_diff_silt_sand     = round((siltyloam - sandyloam) / sandyloam * 100, 1),
    difference_silt_loam  = siltyloam - loam,
    pct_diff_silt_loam     = round((siltyloam - loam) / sandyloam * 100, 1),
    is_different = siltyloam != sandyloam,
    is_different2 = siltyloam != loam
  ) |>
  arrange(desc(abs(difference_silt_sand)))
