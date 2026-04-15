# compare_decid_defs.R
# Reads two RHESSys stratum .def files, compares them side-by-side,
# and writes a colour-coded Excel workbook highlighting differences.
#
# Dependencies: openxlsx
# Install if needed: install.packages("openxlsx")
install.packages("openxlsx")
library(openxlsx)
setwd("RHESSys_Tutorial/")
# ---- 1. Paths ---------------------------------------------------------------
path_my  <- "./HPB_files_NewMaps/defs/veg_deciduousOG.def"
path_cow <- "C:/Users/dwh18/Downloads/416d1d3b-7e69-4c4b-ac7f-1d06bb8b6944/rhessysWS18/rhessys_ws18_local/defs/stratum_deciduous.def"

# ---- 2. Parser --------------------------------------------------------------
parse_def <- function(path) {
  raw <- readLines(path, warn = FALSE)
  raw <- trimws(raw)
  raw <- raw[nchar(raw) > 0]          # drop blank lines

  # Each line: <value> <param_name>
  # Split on first whitespace only (value may be e.g. "-999.900000")
  parts <- strsplit(raw, "\\s+")

  # Lines with exactly 2 tokens → value + param
  # Lines with 1 token  → treat as value-only (rare header lines)
  df <- data.frame(
    param = sapply(parts, function(x) if (length(x) >= 2) paste(x[-1], collapse = " ") else NA_character_),
    value = sapply(parts, function(x) x[1]),
    stringsAsFactors = FALSE
  )
  df
}

df_my  <- parse_def(path_my)
df_cow <- parse_def(path_cow)

# ---- 3. Merge on param name -------------------------------------------------
merged <- merge(df_my, df_cow, by = "param", all = TRUE,
                suffixes = c("_my", "_cow"))

# Coerce values to numeric where possible for numeric comparison
num_my  <- suppressWarnings(as.numeric(merged$value_my))
num_cow <- suppressWarnings(as.numeric(merged$value_cow))

merged$diff_flag <- ifelse(
  is.na(merged$value_my) | is.na(merged$value_cow),
  "MISSING IN ONE FILE",
  ifelse(merged$value_my != merged$value_cow, "DIFFERENT", "SAME")
)

# Numeric difference (where applicable)
merged$numeric_diff <- ifelse(
  !is.na(num_my) & !is.na(num_cow),
  num_cow - num_my,
  NA_real_
)

# Reorder columns for readability
out <- merged[, c("param", "value_my", "value_cow", "numeric_diff", "diff_flag")]
colnames(out) <- c("Parameter", "veg_deciduousOG (my)", "stratum_deciduous (cow)",
                   "Numeric Diff (cow - my)", "Status")

# Sort: different first, then missing, then same
order_map <- c("DIFFERENT" = 1, "MISSING IN ONE FILE" = 2, "SAME" = 3)
out <- out[order(order_map[out$Status], out$Parameter), ]

# write.csv(out, "C:/Users/dwh18/Downloads/RHESSys_decid_def_comp.csv", row.names = F)


# # ---- 4. Build workbook ------------------------------------------------------
# wb <- createWorkbook()
# addWorksheet(wb, "DEF Comparison")
#
# # Header style
# hs <- createStyle(
#   fgFill    = "#2C3E50",
#   fontColour = "#FFFFFF",
#   textDecoration = "bold",
#   halign    = "CENTER",
#   border    = "Bottom",
#   fontSize  = 11
# )
#
# # Row styles
# style_diff    <- createStyle(fgFill = "#FADBD8")   # red-tint  → different
# style_missing <- createStyle(fgFill = "#FDEBD0")   # orange    → missing
# style_same    <- createStyle(fgFill = "#EAFAF1")   # green     → same
# style_center  <- createStyle(halign = "CENTER")
#
# writeData(wb, "DEF Comparison", out, startRow = 1, startCol = 1, headerStyle = hs)
#
# # Apply row colours
# for (i in seq_len(nrow(out))) {
#   row_i  <- i + 1   # +1 for header
#   status <- out$Status[i]
#   sty <- switch(status,
#                 "DIFFERENT"           = style_diff,
#                 "MISSING IN ONE FILE" = style_missing,
#                 style_same)
#   addStyle(wb, "DEF Comparison", style = sty,
#            rows = row_i, cols = 1:5, gridExpand = TRUE)
# }
#
# # Centre numeric columns
# addStyle(wb, "DEF Comparison", style = style_center,
#          rows = 2:(nrow(out) + 1), cols = 2:5, gridExpand = TRUE, stack = TRUE)
#
# # Column widths
# setColWidths(wb, "DEF Comparison",
#              cols = 1:5,
#              widths = c(32, 22, 26, 22, 22))
#
# # Freeze top row
# freezePane(wb, "DEF Comparison", firstRow = TRUE)
#
# # ---- 5. Summary sheet -------------------------------------------------------
# addWorksheet(wb, "Summary")
#
# counts <- as.data.frame(table(Status = out$Status))
# colnames(counts) <- c("Status", "Count")
#
# summary_info <- data.frame(
#   Item  = c("File 1 (my)", "File 2 (cow)",
#             "Total parameters compared",
#             counts$Status),
#   Value = c(path_my, path_cow,
#             nrow(out),
#             counts$Count),
#   stringsAsFactors = FALSE
# )
#
# writeData(wb, "Summary", summary_info, startRow = 1, startCol = 1,
#           headerStyle = hs)
# setColWidths(wb, "Summary", cols = 1:2, widths = c(35, 80))
#
# # ---- 6. Save ----------------------------------------------------------------
# out_path <- "./HPB_files_NewMaps/def_comparison_deciduous.xlsx"
# saveWorkbook(wb, out_path, overwrite = TRUE)
# message("Saved: ", normalizePath(out_path))
