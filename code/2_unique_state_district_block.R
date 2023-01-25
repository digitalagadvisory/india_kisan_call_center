
library(fst)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

datadir <- ".../kccindia"
try(setwd(datadir))

# to get column names
# f <- read_fst("interim/dataset.fst", from = 1, to = 10)
f <- read_fst("interim/dataset.fst", columns = c("StateName","DistrictName","BlockName"))
sdb <- f %>% distinct()
write.csv(sdb, "interim/india_state_district_block.csv", row.names = FALSE)

# category
ct <- read_fst("interim/dataset.fst", columns = c("Sector"))

# quick summary
toc <- read_fst("interim/dataset.fst", columns = c("CreatedOn"))

tocSummary <- toc %>% 
  mutate(date = as_date(CreatedOn)) %>%
  group_by(dates = cut(date, breaks = "1 month")) %>%
  summarise(ncalls = n()) %>%
  mutate(dates = format(as_datetime(dates), "%FT%H:%M:%S%z"))

tocSummary <- tocSummary %>% mutate(months = month(dates, label = TRUE), years = year(dates)) %>% 
  filter(years > 2008)

# more colors, but randomly generated
cols <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99",
          "#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928")

p1 <- ggplot() +
  geom_bar(data=tocSummary, aes(y = ncalls, x = years, fill = months), stat="identity",
           position='stack', width = 0.5) +
  scale_fill_manual(values = cols)  +
  scale_x_continuous(labels = 2009:2021, breaks = 2009:2021) +
  xlab("number of calls") +
  labs(title = "Annual summary of calls") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("interim/annual_summary_call_volume.png", p1, width = 8, height = 5, units = "in", dpi = 200)


# processing the sqlite databse is much easier
try(setwd("/data/Google-Drive/data/India/kccindia/"))

library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), file.path("interim", "kcc_db.sqlite"))
tb <- dbListTables(con)
db <- tbl(con, tb)


# run summary by years to find unique state, district and block combination
years <- seq(2014, 2019)

qsum <- list()

for (i in 1:length(years)){
  qs <- paste0("CreatedOn LIKE ", "'", years[i],"%'")
  print(qs)
  qsr <- db %>%  filter(sql(qs)) %>% select(StateName, DistrictName, BlockName) %>% collect() %>% distinct()
  qsum[[i]] <- qsr
  flush.console()
}

qdf <- do.call(rbind, qsum)
qdf <- distinct(qdf)
saveRDS(qdf, "interim/state_district_block.rds")
