# Where things can go wrong
# wrong crop name --- match if crop name appears in anywhere else, e.g., "QueryText", "KccAns"
# create list of unique crops -> create new column for the crop name
# wrong/missing QueryType --- narrow down QueryTypes, create dictionary of possible options, create new column for the new QueryType
# season missing -> not fixing now; use month instead
# Admin unit names spelled differently than in spatial database -> create match table, needed before making maps

# library(topicmodels)
# library(tidyverse)
# library(DBI)
# library(RSQLite)
# library(dbplyr)

# list of stop words
liststopwords <- "data/stop_words.rds"

if(!file.exists(liststopwords)){
  library(tidytext)
  data("stop_words")
  # save a copy of stop_words for future use
  stopwords <- stop_words$word
  saveRDS(stopwords, liststopwords)
} else {
  stopwords <- readRDS(liststopwords)
}

# add few more words if needed
# stopwords <- c(stopwords, "information")

# try(setwd("/data/Google-Drive/data/India/kccindia/"), silent = TRUE)
# try(setwd("G:/My Drive/data/India/kccindia"), silent = TRUE)
try(setwd("/share/spatial02/users/anighosh/kccindia"), silent = TRUE)

# since the queries and languages vary with states, let's create databases by states, but for all years
# Simplify query
# run on each line
summaryQuery <- function(f){
  r <- readRDS(f)
  r <- r$data
  xs <- unlist(strsplit(tolower(r$QueryText), "\\s+"))
  xs <- gsub("[[:punct:]]", "", xs)
  xs <- xs[xs!=""]
  # remove anything starting with a number
  i <- suppressWarnings(as.numeric(substr(xs, 1, 1)))
  xs <- xs[is.na(i)]
  crop <- unique(tolower(r$Crop))
  crop <- crop[crop!=""]
  return(list(query = xs, crop = crop))
}

# combine for states
saveStateResults <- function(sid, ff, stopwords, dir){
  cat("cleaning state ", sid, "\n"); flush.console()
  sff <- ff[grep(sid, ff)]
  qq <- lapply(sff, summaryQuery)
  # summary of crop name
  crops <- sort(unique(unlist(lapply(qq, function(x)return(x$crop)))))
  # summary query text
  qr <- unlist(lapply(qq, function(x)return(x$query)))
  qr <- qr[!qr %in% stopwords]
  qr <- qr[!qr %in% crops]
  d <- data.frame(count = unclass(table(qr)), stringsAsFactors = FALSE)
  d$query <- row.names(d)
  row.names(d) <- NULL
  d <- d[order(d$query),]
  # write information in excel with 2 sheets; not the best option, but I need this to visualize outside R  
  outfile <- paste0(dir, "/", sid, "_key_query.xlsx") 
  dl <- list("query" = d, "crops" = crops)
  openxlsx::write.xlsx(dl, file = outfile, colnames = TRUE, row.name = FALSE, overwrite = TRUE)
}

# run for states
ff <- list.files(path = "raw/", pattern = "^kcc.*rds$", full.names = TRUE, recursive = TRUE)
sids <- unique(substr(basename(ff), 5, 8))
outdir <- "interim/query_summary"
dir.create(outdir, FALSE, TRUE)
lapply(sids, saveStateResults, ff, stopwords, outdir)


state <- 1:39
for (i in 1:length(state)){
  stateid <- ifelse(length(state[[i]]) == 1, sprintf("%02d", state[[i]]), state[[i]])
  statecode <- paste0("kcc_s_", stateid)
  print(stateid)
  f <- grep(statecode, ff, value = TRUE)
  d <- readRDS(f[1])
  d <- d$data
  print(head(d))
}

statecode <- paste0("kcc_s_", 20)
f <- grep(statecode, ff, value = TRUE)


# save only one state
i <- 20
stateid <- ifelse(length(state[[i]]) == 1, sprintf("%02d", state[[i]]), state[[i]])
statecode <- paste0("kcc_s_", stateid)
print(stateid)
f <- grep(statecode, ff, value = TRUE)
d <- readRDS(f[1])
d <- d$data
print(head(d))
