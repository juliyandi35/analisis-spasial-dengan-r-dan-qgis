# Import peta
library(sf)
Sumatera = read_sf('peta_sumatra.shp')
Sumatera <- st_zm(Sumatera, "ZM")  # Menghilangkan dimensi Z

library(sp)
Data <- read.csv("data_beras4.csv")
Data

# Membuat plot peta
library(ggplot2)
ggplot() +
  geom_sf(data = Sumatera) +
  labs(title = "Peta Kabupaten/Kota di Sumatera")

#Statistika Deskriptif
library(readxl)
summary(Data)

Data <- merge(Sumatera,Data,by="ID")

library(sp)
Data <- st_as_sf(Data)
ggplot(Data) + geom_sf(aes(fill = Y)) +
  scale_fill_gradient2(
    midpoint = 13000, low = "#28E2E5", high = "#DF536B") +
  theme_bw()+
  geom_text(
    aes(label = KabKot, x = coordinates(as(Data,"Spatial"))[,1], y = coordinates(as(Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Data Beras Sumatera")+xlab("Longitude")+ylab("Latitude")

# Model OLS
regresi<-lm(formula=Y~X1+X2+X3+X4+X5,data=Data)
summary(regresi)

#Uji Heterogenitas (Breusch-Pagan)
library(lmtest)
bptest(regresi)

# Uji Multikolinearitas
library(car)
vif(regresi)

library(GWmodel)
library(spdep)
#Penskalaan Data ke Dalam Data Frame Spasial# 
library(dplyr)

# moran test
x = coordinates(as(Data,"Spatial"))[,1]
y = coordinates(as(Data,"Spatial"))[,2]
coords <-cbind(x,y)
jarak <-as.matrix(1/dist(coords))
lm.morantest(regresi,listw=mat2listw(jarak), alternative="two.sided") # Moran Index

# GWR
# convert to sp
Data.sp = as(Data, "Spatial")
bw<-bw.gwr(Y~X1+X2+X3+X4+X5, data=Data.sp,
           approach="CV",kernel="bisquare",adaptive=TRUE,
           p=2,longlat=FALSE)

#Pemodelan GWR 
GWRModel<-gwr.basic(Y~ X1+X2+X3+X4+X5,data=Data.sp,
                    bw=bw,kernel="bisquare",adaptive=TRUE,p=2,
                    F123.test=T)
# Uji F
GWRModel$Ftests$F1.test
GWRModel$Ftests$F2.test
GWRModel$Ftests$F3.test
GWRModel$Ftests$F4.test

#Estimasi parameter model GWR
GWR_sf = st_as_sf(GWRModel$SDF)

# Tentukan daerah signifikan
GWR_sf$pvalue.X1 = 2*pt(abs(GWR_sf$X1_TV), df = 136, lower.tail = FALSE)
GWR_sf$pvalue.X2 = 2*pt(abs(GWR_sf$X2_TV), df = 136, lower.tail = FALSE)
GWR_sf$pvalue.X3 = 2*pt(abs(GWR_sf$X3_TV), df = 136, lower.tail = FALSE)
GWR_sf$pvalue.X4 = 2*pt(abs(GWR_sf$X4_TV), df = 136, lower.tail = FALSE)
GWR_sf$pvalue.X5 = 2*pt(abs(GWR_sf$X5_TV), df = 136, lower.tail = FALSE)

#Pemilihan Model 
# RMSE
RMSE <- function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}
OLS_RMSE <- RMSE(Data$Y,predict(regresi))
GWR_RMSE <- RMSE(Data$Y,GWR_sf$yhat)

Diagnostic_comparison_tabel <-
  cbind(c("OLS","GWR"), 
        rbind(summary(regresi)$r.squared,GWRModel$GW.diagnostic$gw.R2),
        rbind(OLS_RMSE,GWR_RMSE))
colnames(Diagnostic_comparison_tabel) <- c("Metode","R Squared","RMSE")
rownames(Diagnostic_comparison_tabel) <- NULL
Diagnostic_comparison_tabel <- data.frame(Diagnostic_comparison_tabel)
Diagnostic_comparison_tabel

# 3 model teratas dari hasil GWR
First_Three_Model <- GWR_sf[1:3,]
First_Three_Model
First_Three_Data <- Data[1:3,]

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X1)
#---------------------------------------------------------------#
First_Three_Data$signfikansi_X1 <- NA
# Signifikan
First_Three_Data[First_Three_Model$pvalue.X1 <= 0.05, "signfikansi_X1"] <- "Signifikan"

# Tidak Signifikan
First_Three_Data[First_Three_Model$pvalue.X1 > 0.05, "signfikansi_X1"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=First_Three_Data) +
  geom_sf(mapping=aes(fill =signfikansi_X1)) +
  scale_fill_manual(values = c("#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Signifikansi X1")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X2)
#---------------------------------------------------------------#
First_Three_Data$signfikansi_X2 <- NA
# Signifikan
First_Three_Data[First_Three_Model$pvalue.X2 <= 0.05, "signfikansi_X2"] <- "Signifikan"

# Tidak Signifikan
First_Three_Data[First_Three_Model$pvalue.X2 > 0.05, "signfikansi_X2"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=First_Three_Data) +
  geom_sf(mapping=aes(fill =signfikansi_X2)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Signifikansi X2")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X3)
#---------------------------------------------------------------#
First_Three_Data$signfikansi_X3 <- NA
# Signifikan
First_Three_Data[First_Three_Model$pvalue.X3 <= 0.05, "signfikansi_X3"] <- "Signifikan"

# Tidak Signifikan
First_Three_Data[First_Three_Model$pvalue.X3 > 0.05, "signfikansi_X3"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=First_Three_Data) +
  geom_sf(mapping=aes(fill =signfikansi_X3)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Signifikansi X3")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X4)
#---------------------------------------------------------------#
First_Three_Data$signfikansi_X4 <- NA
# Signifikan
First_Three_Data[First_Three_Model$pvalue.X4 <= 0.05, "signfikansi_X4"] <- "Signifikan"

# Tidak Signifikan
First_Three_Data[First_Three_Model$pvalue.X4 > 0.05, "signfikansi_X4"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=First_Three_Data) +
  geom_sf(mapping=aes(fill =signfikansi_X4)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Signifikansi X4")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X5)
#---------------------------------------------------------------#
First_Three_Data$signfikansi_X5 <- NA
# Signifikan
First_Three_Data[First_Three_Model$pvalue.X5 <= 0.05, "signfikansi_X5"] <- "Signifikan"

# Tidak Signifikan
First_Three_Data[First_Three_Model$pvalue.X5 > 0.05, "signfikansi_X5"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
ggplot(data=First_Three_Data) +
  geom_sf(mapping=aes(fill =signfikansi_X5)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle("Signifikansi X5")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
# Buat kolom baru untuk kombinasi signfikansi
First_Three_Data <- First_Three_Data %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X2,X3,X4,X5",
    
    # Eliminasi 1
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X1,X2,X3,X4",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X2,X3,X5",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X2,X4,X5",
    signfikansi_X1 == "Signifikan" & signfikansi_X3 == "Signifikan" &
      signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X3,X4,X5",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" &
      signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X2,X3,X4,X5",
    
    # Eliminasi 2
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" ~ "X1,X2,X3",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X1,X2,X4",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X2,X5",
    signfikansi_X1 == "Signifikan" & signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X1,X3,X4",
    signfikansi_X1 == "Signifikan" & signfikansi_X3 == "Signifikan" & signfikansi_X5 == "Signifikan" ~ "X1,X3,X5",
    signfikansi_X1 == "Signifikan" & signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X4,X5",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X2,X3,X4",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X2,X3,X5",
    signfikansi_X2 == "Signifikan" & signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan" ~ "X2,X4,X5",
    signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X3,X4,X5",

    # Eliminasi 3
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan"  ~ "X1,X2",
    signfikansi_X1 == "Signifikan" & signfikansi_X3 == "Signifikan"  ~ "X1,X3",
    signfikansi_X1 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X1,X4",
    signfikansi_X1 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X1,X5",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan"  ~ "X2,X3",
    signfikansi_X2 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X2,X4",
    signfikansi_X2 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X2,X5",
    signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan"  ~ "X3,X4",
    signfikansi_X3 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X3,X5",
    signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan"  ~ "X4,X5",
    
    # Eliminasi 4
    signfikansi_X1 == "Signifikan"  ~ "X1",
    signfikansi_X2 == "Signifikan"  ~ "X2",
    signfikansi_X3 == "Signifikan"  ~ "X3",
    signfikansi_X4 == "Signifikan"  ~ "X4",
    signfikansi_X5 == "Signifikan"  ~ "X5",
    
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  "X1,X2,X3,X4,X5" = "black",
  "X1,X2,X3,X4" = "red",
  "X1,X2,X3,X5" = "blue",
  "X1,X2,X4,X5" = "green",
  "X1,X3,X4,X5" = "purple",
  "X2,X3,X4,X5" = "orange",
  "X1,X2,X3" = "yellow",
  "X1,X2,X4" = "cyan",
  "X1,X2,X5" = "magenta",
  "X1,X3,X4" = "gray",
  "X1,X3,X5" = "brown",
  "X1,X4,X5" = "pink",
  "X2,X3,X4" = "turquoise",
  "X2,X3,X5" = "lavender",
  "X2,X4,X5" = "maroon",
  "X3,X4,X5" = "coral",
  "X1,X2" = "navy",
  "X1,X3" = "gold",
  "X1,X4" = "violet",
  "X1,X5" = "salmon",
  "X2,X3" = "skyblue",
  "X2,X4" = "tan",
  "X2,X5" = "plum",
  "X3,X4" = "slategray",
  "X3,X5" = "khaki",
  "X4,X5" = "orchid",
  "X1" = "limegreen",
  "X2" = "darkorange",
  "X3" = "steelblue",
  "X4" = "firebrick",
  "X5" = "darkslategray",
  "Tidak Signifikan" = "white"
)

ggplot(data = First_Three_Data) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")+
  geom_text(
    aes(label = KabKot, x = coordinates(as(First_Three_Data,"Spatial"))[,1], y = coordinates(as(First_Three_Data,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 1.5,
    check_overlap = TRUE
  )+ggtitle(("Variabel Signifikan tiap Lokasi"))+xlab("Longitude")+ylab("Latitude")
