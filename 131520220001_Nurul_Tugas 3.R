#Mengaktifkan package
library(car)
library(nortest)
library(ggplot2)
library(dplyr)
library(lmtest)


#1. Mengidentifikasi outlier berdasarkan boxplot
boxplot(w5peflj_ashp$pef)


#2. Menentukan nilai cutoff untuk outlier
boxplot.stats(w5peflj_ashp$pef)$out

quartiles <- quantile(w5peflj_ashp$pef, probs=c(.25, .75), na.rm = FALSE)

IQR <- IQR(w5peflj_ashp$pef)

Lower <- quartiles[1] - 1.5*IQR

Upper <- quartiles[2] + 1.5*IQR


#3. Membuat data tanpa outlier
data_no <- subset(w5peflj_ashp$pef, w5peflj_ashp$pef > Lower & w5peflj_ashp$pef < Upper)


#4. Melakukan uji normalitas
#a. dengan outliers
lillie.test(w5peflj_ashp$pef)

#b. tanpa outliers
lillie.test(data_no)


#5. Membuat qqline
qqnorm(w5peflj_ashp$pef); qqline(w5peflj_ashp$pef)
qqnorm(data_no); qqline(data_no)


#6. Membuat scatterplot pef dan tinggi badan

smoothScatter(w5peflj_ashp$pef~w5peflj_ashp$height, xlab = "Tinggi Badan (cm)",
              ylab = "Peak Expiratory Flow", 
              main = "Sebaran PEF berdasarkan Tinggi Badan")

abline(lm(w5peflj_ashp$pef~w5peflj_ashp$height, data = w5peflj_ashp), col = "blue")

lines(lowess(w5peflj_ashp$height, w5peflj_ashp$pef), col = "red")


#7. Membuat scatterplot pef dan usia
smoothScatter(w5peflj_ashp$pef~w5peflj_ashp$age, xlab = "Usia (Tahun)",
              ylab = "Peak Expiratory Flow", 
              main = "Sebaran PEF berdasarkan Usia")

abline(lm(w5peflj_ashp$pef~w5peflj_ashp$age, data = w5peflj_ashp), col = "blue")

lines(lowess(w5peflj_ashp$age, w5peflj_ashp$pef), col = "red")