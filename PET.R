csx = read.csv2("OUTPUT/Harian/CHP/DAILY_CHP_GABUNG.csv", stringsAsFactors = F)
DEYS = data.frame(as.matrix(read_xlsx("VALIDASI_joz.xlsx", sheet = "DAILY")), stringsAsFactors = F)

DEYS$Tanggal = as.numeric(DEYS$Tanggal)
DEYS$Tanggal[nchar(DEYS$Tanggal) == 1] = paste0("0", DEYS$Tanggal[nchar(DEYS$Tanggal) == 1])
DEYS$Bulan = as.numeric(DEYS$Bulan)
DEYS$Bulan[nchar(DEYS$Bulan) == 1] = paste0("0", DEYS$Bulan[nchar(DEYS$Bulan) == 1])
CHP_DATE = as.POSIXct(paste0(csx$Tahun, "-", csx$Bulan, "-", csx$Tanggal), tz = "UTC")
#ini_ilang_30 = which(OBS_DATE == "2019-09-30")
OBS_DATE = as.POSIXct(paste0(DEYS$Tahun, "-", DEYS$Bulan, "-", DEYS$Tanggal), tz = "UTC")

is.character0 = function(x)
{
  is.character(x) && length(x) == 0L
}
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}
is.numeric0 <- function(x)
{
  is.numeric(x) && length(x) == 0L
}

CHP_DATE = as.character(CHP_DATE)
OBS_DATE = as.character(OBS_DATE)
ready = which(as.character(CHP_DATE) %in% as.character(OBS_DATE))

minready = which(!as.character(CHP_DATE) %in% as.character(OBS_DATE))
CHP_DATE[minready][substr(CHP_DATE[minready], 9, 10) == "01"]
aout = data.frame(STA =csx$STA[ready],
           Tahun = csx$Tahun[ready], 
           Bulan = csx$Bulan[ready], 
           Tanggal = csx$Tanggal[ready],   
           CH = csx$CH[ready], stringsAsFactors = F)

write.csv2(aout, file = "OUTPUT/Harian/CHP/REV_DAILY_CHP_GABUNG.csv", row.names = F, na = "")