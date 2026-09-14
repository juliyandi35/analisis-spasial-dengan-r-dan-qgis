library(readxl)
Data <- read_excel("data_sebaran gempa.xlsx")
Data <- data.frame(Data)
colnames(Data)
colnames(Data) <- c("No","Event.ID","Date.time","Latitude",
                    "Longitude","Magnitude","Mag.Type","Depth(km)",
                    "Phase.Count","Azimuth.Gap","Location","Agency")
Data_sf <- Data
coordinates(Data_sf) <- c("Longitude","Latitude")
class(Data_sf)

library(sf)
Data_sf <- st_as_sf(Data_sf)
# Mengatur sistem koordinat projektif (Bali NTB berada di DGN95 / UTM Zone 50S)
st_crs(Data_sf) <- st_crs("+proj=utm +zone=51 +south +datum=WGS84")

# Metode Kuadran
# Kondisi:
# VMR = 0 titik menyebar Uniform(sistematik)
# VMR = 1 titik menyebar acak
# VMR > 1 titik menyebar lebih mengelompok

library(spatstat)
# Membuat objek ppp dari data koordinat titik
Data_sp <- as(Data_sf, "Spatial")
X <- ppp(x = coordinates(Data_sp)[,1], y = coordinates(Data_sp)[,2], window=owin(c(120, 130), c(-3, 3)))
plot(X)

summary(X)
contour(density(X,2),axes = F)

# 3x3 grid
q3 <- quadratcount(X, nx = 3, ny = 3)
plot(q3)

mu3 <- mean(q3)
sigma3 <- sd(q3)^2
VMR3 <- sigma3/mu3 # VMR = Variance Means Ration
VMR3

# 4x4 grid
q4 <- quadratcount(X, nx = 4, ny = 4)
plot(q4)

mu4 <- mean(q4)
sigma4 <- sd(q4)^2
VMR4 <- sigma4/mu4
VMR4

# 5x5 grid
q5 <- quadratcount(X, nx = 5, ny = 5)
plot(q5)

mu5 <- mean(q5)
sigma5 <- sd(q5)^2
VMR5 <- sigma5/mu5
VMR5

# 6x6 grid
q6 <- quadratcount(X, nx = 6, ny = 6)
plot(q6)

mu6 <- mean(q6)
sigma6 <- sd(q6)^2
VMR6 <- sigma6/mu6
VMR6

# 7x7 grid
q7 <- quadratcount(X, nx = 7, ny = 7)
plot(q7)

mu7 <- mean(q7)
sigma7 <- sd(q7)^2
VMR7 <- sigma7/mu7
VMR7

# 8x8 grid
q8 <- quadratcount(X, nx = 8, ny = 8)
plot(q8)

mu8 <- mean(q8)
sigma8 <- sd(q8)^2
VMR8 <- sigma8/mu8
VMR8

# 9x9 grid
q9 <- quadratcount(X, nx = 9, ny = 9)
plot(q9)

mu9 <- mean(q9)
sigma9 <- sd(q9)^2
VMR9 <- sigma9/mu9
VMR9

# 10x10 grid
q10 <- quadratcount(X, nx = 10, ny = 10)
plot(q10)

mu10 <- mean(q10)
sigma10 <- sd(q10)^2
VMR10 <- sigma10/mu10
VMR10

VMR <- c(VMR3,VMR4,VMR5,VMR6,VMR7,VMR8,VMR9,VMR10)
VMR

# Metode Empirical K-Function
nn <- nndist(X)
hist(nn)

K<- Kest(X, correction="Ripley")
plot(K)
E<-envelope(X,Kest, nsim=99)
plot(E)

