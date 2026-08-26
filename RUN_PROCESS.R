# Master execution script for the SMI facility management reproducible analysis.

script_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
source_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)

repo_dir <- if (length(script_file) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]]), winslash = "/", mustWork = TRUE))
} else if (!is.na(source_file)) {
  dirname(source_file)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(repo_dir)

dir.create("Outputs/Figures", recursive = TRUE, showWarnings = FALSE)
dir.create("Outputs/Tables", recursive = TRUE, showWarnings = FALSE)

# Remove previously generated outputs so each run starts from a clean state.
unlink(file.path("Outputs", "Figures", "*"), force = TRUE)
unlink(file.path("Outputs", "Tables", "*"), force = TRUE)

source("Scripts/01_facility_management_models.R")
