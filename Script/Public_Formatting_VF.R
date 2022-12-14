# ---
# title: "Formatting Fence GPS points"
# author: "Brandon Mayer"
# ---

library(tidyverse)
library(sf)
library(smoothr)

paste0(getwd(),"/Data/RAWFenceFolder")
filelist <-list.files(paste0(getwd(),"/Data/RAWFenceFolder"),pattern = ".txt", full.names = T, all.files = TRUE, recursive = TRUE); filenames <- stringr::str_replace_all(basename(filelist),".txt","")
endlist <- stringr::str_replace_all(list.files(paste0(getwd(),"/Data/ProcessedFences"),pattern = ".csv"),".csv","")
VAP <-read_csv(paste0(getwd(),"/Data/VenceActivityPeriods.csv"))
NeedProcessing <- setdiff(VAP$Fence[!is.na(VAP$Off)],filenames);(paste("Listed fences have incomplete `fence off` dates:",VAP$Fence[is.na(VAP$Off)]))
if (length(endlist)>0) {
  filelist <- filelist[grepl(paste(NeedProcessing, collapse = "|"), filenames)]
}

# Load in relevant data associated with the virtual fence
if (length(filelist)>0) {
for (i in 1:length(filelist)) {
  x <- read.table( filelist[i], sep=",", header=TRUE)
  x$Fence <- stringr::str_remove(basename(filelist[i]),".txt")
  x <- x %>% left_join(VAP,by = "Fence")
  write.csv(x, file = paste0(stringr::str_replace(paste0(getwd(),"/Data/ProcessedFences/",basename(filelist[i])),".txt",".csv")),row.names = F)
}
(endlist <- list.files(paste0(getwd(),"/Data/ProcessedFences"),pattern = ".csv", full.names = T, all.files = TRUE, recursive = TRUE) %>%
    lapply(function(x) {read.csv(x)}))
}

