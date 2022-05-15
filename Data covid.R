#INSTALL READXL PACKAGE

FREKUESI
fi <- c(16, 20, 15, 19, 17, 9, 2, 2)
fi

#======================INTERVAL MAX DAN MIN======================
max_interval <- c (max(14:37), max(38:61), max(62:85), max(86:109), max(110:133), max(134:157), max(158:181), max(182:205))
min_interval <- c (min(14:37), min(38:61), min(62:85), min(86:109), min(110:133), min(134:157), min(158:181), min(182:205))


#TITIK TENGAH
Xi <- c((max_interval - min_interval)/2 + min_interval)

#======================MEAN ( RATA- RATA )======================
mean <- sum(fi * Xi) / sum(fi)
mean



#======================MEDIAN======================

#--Letak Median--
med <- 1 / 2 * sum(fi)

#--Tb (Tepi Bawah Kelas Median)--
tb <- 62 - 0.5
tb

#--fkk (Frekuensi Komulatif Kurang dari Kelas Median)--
fkk <- 36

#--fm (frekuensi kelas median) dan l (panjang kelas)--
fm <- 15
l <- 24 

median <- tb + ((1 / 2 * sum(fi) - fkk) / fm) * l
median


#======================MODUS======================

#--b (tepi bawah kelas interval dengan frekuensi terbanyak)--
b <- 38 - 0.5
b

#--b1 (frekuensi kelas modus - frekuensi kelas sebelumnya)--
b1 <- 20 - 16
b1

#--b2 (frekuensi kelas modus - frekuensi kelas setelahnya)--
b2 <- 20 - 15
b2

#--p (panjanng kelas interval)--
p <- 24
p

modus <- b + (b1 / (b1 + b2)) * p
modus


#======================RANGE======================

xmax <- max(Xi)
xmin <- min(Xi)
range <- xmax - xmin
range


#======================MEAN DEVIASI (SIMPANGAN RATA-RATA)======================
sr <- sum(fi * abs(Xi - mean)) / sum(fi)
sr


#======================STANDAR DEVIASI======================
sd <- sqrt(sum(fi * (Xi - mean) ^ 2) / sum(fi))
sd