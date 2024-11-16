# Get the username
username <- Sys.info()["login"]

# Map usernames to their working and data directories
dir_map <- list(
  Leonie = list(
    wdir = "C:/Users/Leonie/HESSENBOX/Daten/analysis_R/",
    data_dir = "C:/Users/Leonie/HESSENBOX/Daten/analysis_R/"
  ),
  david = list(
    wdir = "D:/qol_prospective",
    data_dir = "D:/qol_prospective/data"
  ),
  dpedrosac = list(
    wdir = "/media/storage/qol_prospective",
    data_dir = "/media/storage/qol_prospective/data"
  )
)

# Set directories based on the username
if (username %in% names(dir_map)) {
  wdir <- dir_map[[username]]$wdir
  data_dir <- dir_map[[username]]$data_dir
  setwd(wdir)
} else {
  cat("Username unknown\n")
}
