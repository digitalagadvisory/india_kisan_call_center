# create summary data
dir <- ".../kccindia/raw"
library(data.table)
library(future.apply)
library(progressr)

handlers(global = TRUE)
handlers("progress", "beepr")

ff <- list.files(dir, pattern = "^kcc.*rds$", full.names = TRUE, recursive = TRUE)
write.table(ff, file.path(dir, paste0("list-of-files_", Sys.Date(),".txt")), 
            row.names = FALSE, col.names = FALSE)

getSummary <- function(f){
  x <- readRDS(f) 
  x <- x$data
  year <- unique(sapply(strsplit(x$CreatedOn, "-"), "[[", 1))
  month <- unique(sapply(strsplit(x$CreatedOn, "-"), "[[", 2))
  did <- strsplit(basename(f), "_")[[1]][5]
  sid <- did <- strsplit(basename(f), "_")[[1]][3]
  
  out <- data.frame(state = unique(x$StateName), stateid = sid,
                    district = unique(x$DistrictName), districtid = did, 
                    year = year, month = month, 
                    block = unique(x$BlockName), ncall = nrow(x), stringsAsFactors = FALSE)
  return(out)
}

plan(multiprocess, workers = 8)
# track progress?
dd <- future_lapply(ff, function(x) {getSummary(x);
  message(paste0(basename(x), " completed. ", Sys.time()))},
  future.conditions = "message", future.stdout = NA)
