#!/usr/bin/Rscript
library(raster)
library(readxl)
rm(list = ls())
args = commandArgs(trailingOnly=TRUE)


xcl = read_xlsx("VALIDASI_joz.xlsx", sheet = "Koordinat")
STA =xcl$DAS
xcl = as.matrix(xcl[2:length(STA), ] )
STA = STA[2:length(STA)]

STADAS = STA[2:(which(is.na(STA))-1)]
xclDAS = xcl[2:(which(is.na(STA))-1), ]
STADAY = STA[(which(is.na(STA))+3) : length(STA)]
xclDAY = xcl[(which(is.na(STA))+3) : length(STA), ]
lonDAY = as.numeric(xclDAY[,4])
latDAY = as.numeric(xclDAY[,3])
pat = "data CHIRPS/Harian/"

LNC = list.files("data CHIRPS/Harian/", pattern = "nc")
DAT = brick(paste0(pat, LNC[1]))


day_extract = function(x, y, tahun, kosong_tanggal){
  # xy = data.frame(lon = 106.8500, lat = -6.1800)
  # tahun = 2019
  # x = lonDAY[1];y = latDAY[1]
  itah = grep(LNC, pattern = tahun)
  DAT = brick(paste0(pat, LNC[itah]))
  xy = data.frame(lon = x, lat = y)
  ExDat = as.matrix(extract(DAT, xy, buffer = 5000)[[1]])
  namai = gsub(as.character(as.matrix(rownames(ExDat))), pattern = "X", replacement = "")
  ExDat = as.numeric(as.matrix(ExDat))
  namai = gsub(namai, pattern = "\\.", replacement = "-")
  namax =seq(as.Date(paste0(tahun, "-01-01")), as.Date(paste0(tahun,"-12-31")), by = "day")
  namax = namax[(as.character(namax) %in% namai)]
  namai = substr(as.POSIXct(namax, tz = "UTC")-(3600*24), 1, 10)
  if(tahun == substr(kosong_tanggal, 1, 4)){
    if(kosong_tanggal != 0){
      namai = c(namai, kosong_tanggal)
      ExDat = c(as.character(as.matrix(ExDat)), "")
    }
  }
  
  ExDat = cbind(Tahun =  substr(namai, 1, 4), Bulan =  substr(namai, 6, 7), Tanggal = substr(namai, 9, 10), CH = as.numeric(ExDat))
  write.csv2(ExDat, file = paste0("OUTPUT/Harian/CHP/", substr(LNC[itah], 1, (nchar(LNC[1])-3)), ".csv") , row.names = F, na  = "")
  return(ExDat)
}



semuaposDAY = function(mana){
  gabung = day_extract(x = lonDAY[mana], y = latDAY[mana], tahun = "2014", kosong_tanggal = "2019-09-30")
  for(i in 1:(length(LNC)-1)){
    gabung = rbind(gabung, day_extract(x = lonDAY[mana], y = latDAY[mana], tahun = paste0(2014+i), kosong_tanggal = "2019-09-30"))
  }
  write.csv2(gabung, file = paste0("OUTPUT/Harian/CHP/", STADAY[mana], "CHP_GABUNG.csv") , row.names = F)
  return(gabung)
}

out =cbind(STA = STADAY[1], semuaposDAY(mana = 1))
for(i in 1:(length(STADAY)-1)){
  outx =cbind(STA = STADAY[i+1], semuaposDAY(mana = i+1))
  out = rbind(out, outx)
}

out = data.frame(out, stringsAsFactors = F)
out$CH = as.numeric(out$CH)
write.csv2(out, file = paste0("OUTPUT/Harian/CHP/", "DAILY_CHP_GABUNG.csv") , row.names = F, na = "")

source('PET.R')