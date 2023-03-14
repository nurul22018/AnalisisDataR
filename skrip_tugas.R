#skrip baru
#aktivasi package readr
library(readr)

#aktivasi package dplyr
library(dplyr)

#reading data
ds_tugas = read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

#aktivasi package janitor
library(janitor)

#identifikasi elemen duplikasi
get_dupes(ds_tugas, pidlink)

#menghapus data yang duplikasi
ds2 = ds_tugas[!duplicated(ds_tugas$pidlink),]
