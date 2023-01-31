# ---
# title: "Formatting Fence GPS points"
# author: "Brandon Mayer"
# ---

library(tidyverse)
library(sf)
library(smoothr)
world <- ne_countries(scale = "medium", returnclass = "sf")

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
    VAPclean <- VAP[!is.na(VAP$Off),] 
    VAPcleanSplit <- VAPclean %>% split(VAPclean$Herd)
    for (h in 1:length(VAPcleanSplit)) {
      for (f in 1:length(unique(VAPcleanSplit[[h]]$Fence))) {
        x1<- x
        x1 <- x1 %>% left_join(VAPcleanSplit[[h]][f,],by = "Fence")
        if (!is.na(unique(x1$Herd))) {
  write.csv(x1, file = paste0(stringr::str_replace(paste0(getwd(),"/Data/ProcessedFences/",unique(x1$Herd),"_",basename(filelist[i])),".txt",".csv")),row.names = F)
}}}}}
Endlist <- list.files(paste0(getwd(),"/Data/ProcessedFences"),pattern = ".csv",full.names = T)

# Checking df for errors
# (list.files(paste0(getwd(),"/Data/ProcessedFences"),pattern = ".csv", full.names = T, all.files = TRUE, recursive = TRUE) %>%
#       lapply(function(x) {read.csv(x)}))

Processed <- stringr::str_replace_all(list.files(paste0(getwd(),"/Data/ProcessedFences"),pattern = ".csv", full.names = F, all.files = TRUE, recursive = TRUE),".csv","")
Shapefiles <- stringr::str_split(stringr::str_replace_all(list.files(paste0(getwd(),"/Data/Shapefiles"),pattern = ".shp", full.names = F, all.files = TRUE, recursive = TRUE),".shp",""), "_") %>% sapply(., "[[", 2) %>% unique()

NeedProcessing <- setdiff(Processed,Shapefiles)
if (length(NeedProcessing)>0) {
  endlist <- Endlist[grepl(paste(NeedProcessing, collapse = "|"), Processed)]%>%
    lapply(function(x) {read.csv(x)})
}
 
for (j in 1:length(endlist)) {
  test <- endlist[[j]] %>% filter(!is.na(Fgroup))%>% select(Fgroup,latitude, longitude, Shock, Sound) %>% group_by(Fgroup)%>% mutate(points = seq(1:length(latitude)))
  meta <- endlist[[j]] %>% filter(!is.na(Fgroup))%>% select(Fgroup,Fence, Herd, On, Off) %>% group_by(Fence, Herd, On, Off) %>% summarise()
  FencePoints<- st_as_sf(test, coords = c("longitude", "latitude"), 
                         crs = 4326)
  FencePoints <- FencePoints%>% group_by(Fgroup) %>% group_split()
  Fullfence <- list();FullBufferSh <- list();FullBufferSo <- list()
  
  FullFenceShape<- st_as_sf(endlist[[j]], coords = c("longitude", "latitude"),
                            crs = 4326) %>% st_cast("POINT")%>% st_combine()%>% st_cast("POLYGON")
  
  for (i in 1:length(FencePoints)) {
    
    # lapply(list, function)
    points <- st_cast(st_geometry(FencePoints[[i]]), "POINT") 
    # Number of total linestrings to be created
    n <- length(points) - 1
    # Build linestrings
    linestrings <- lapply(X = 1:n, FUN = function(x) {
      pair <- st_union(c(points[x], points[x + 1]))
      line <- st_cast(pair, "LINESTRING")
      return(line)
      
    })
    
    # One MULTILINESTRING object with all the LINESTRINGS
    multilinetring <- st_multilinestring(do.call("rbind", linestrings))
    multilinetring <- st_sfc(multilinetring,crs = 4326)
    Fullfence[i] <- multilinetring
    # one linestringsbuffers object with all buffers
    bufferrawSh <- st_buffer(multilinetring,
                             dist = unique(test$Shock))
    bufferrawSo <- st_buffer(multilinetring,
                             dist = unique(test$Shock)+unique(test$Sound))
    # -------------------------------Test Script--------------------------------
    # Attmepting to use rgeos with sp
    rgeostrial <-sp::spTransform(as(multilinetring,"Spatial"),  sp::CRS("+init=epsg:4326"))
    rgeostrialsh<- rgeos::gBuffer(rgeostrial,width = unique(test$Shock),capStyle="SQUARE")
    rgeostrialso<- rgeos::gBuffer(rgeostrial,width = (unique(test$Shock)+unique(test$Sound)),capStyle="SQUARE")
    rgst_sh <- st_as_sf(rgeostrialsh,crs = 4326)
    rgst_so <- st_as_sf(rgeostrialso,crs = 4326)

    rgeos::gDifference(rgeostrialsh,rgeostrialso)
    
    # Removed buffering to show you the problem with st_buffer
    # bufferSh <- smooth(densify(bufferrawSh, max_distance = 10), method = "ksmooth", smoothness = 18,)
    # bufferSo <- smooth(densify(bufferrawSo, max_distance = 10), method = "ksmooth", smoothness = 18,)
    bufferSh <-bufferrawSh
    bufferSo <-bufferrawSo
    
    buffersSo <-st_difference(bufferSo,bufferSh)
    FinalVFSo<- st_intersection(buffersSo,FullFenceShape)
    FinalVFSh<- st_intersection(bufferSh,FullFenceShape)
    
    FullBufferSh[i] <-FinalVFSh;FullBufferSo[i] <-FinalVFSo
  }


  FulllinestringsbuffersSh <- st_multipolygon(FullBufferSh);FulllinestringsbuffersSo <- st_multipolygon(FullBufferSo)
  FulllinestringsbuffersSh <- st_sfc(FulllinestringsbuffersSh,crs = 4326);FulllinestringsbuffersSo <- st_sfc(FulllinestringsbuffersSo,crs = 4326)
  
  Herd <- unique(endlist[[j]]$Herd);Fence<-unique(endlist[[j]]$Fence)
  # st_write(FullFenceShape %>% merge(meta),paste0(getwd(),"/Data/Shapefiles/",Herd,"_",Fence,"_FF.shp"));
  # st_write(FulllinestringsbuffersSo %>% merge(meta),paste0(getwd(),"/Data/Shapefiles/",Herd,"_",Fence,"_Sound.shp"));
  # st_write(FulllinestringsbuffersSh %>% merge(meta),paste0(getwd(),"/Data/Shapefiles/",Herd,"_",Fence,"_Shock.shp"))
  print(ggplot(data = world) +
          geom_sf() +
          geom_sf(data = FullFenceShape)+
          geom_sf(data = FulllinestringsbuffersSh) +
          geom_sf(data = FulllinestringsbuffersSo)+
          coord_sf(xlim = c(max(endlist[[j]]$longitude)+.001, min(endlist[[j]]$longitude)-.001), ylim = c(min(endlist[[j]]$latitude)-.001, max(endlist[[j]]$latitude)+.001), expand = FALSE))

}
