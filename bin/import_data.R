# Data can be downloaded from:
# https://www.fzp.czu.cz/en/r-9409-science-research/r-9674-leading-research-groups/r-9669-hydrological-and-climate-variability/r-9713-team-news/webapp-for-hydroclimate-reconstruction.html

owda_raw <- data.table(readRDS("./data/OWDA_1000.rds")) 
owda_raw[Lat < 37.75 & Lon > -2 & Lon < 11, scPDSI :=NA]
owda_raw <- owda_raw [complete.cases(owda_raw),]
owda_raw[,id := .GRP, by = list(Lat, Lon)]
setkey(owda_raw, id, Time)
names(owda_raw) <- c("time", "lat", "lon", "scPDSI", "id")
owda_coords <- owda_raw[time == 992, .(id, lat, lon)] #needed for creating maps

owda_for_som <- owda_raw[, .(id, time, scPDSI)]
owda_for_som <- as.matrix(reshape(owda_for_som, 
                                 ids = "id", 
                                 timevar = "time", 
                                 direction = "wide"))
owda_for_som <- owda_for_som[complete.cases(owda_for_som), -1] #DO I have to remove first column???

save(owda_raw, owda_for_som, owda_coords, file = "./data/owda_for_som.rdata")
