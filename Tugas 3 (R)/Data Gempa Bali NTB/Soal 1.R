library(readxl)
Data <- read_excel("Juli.xlsx")
Data

library(sp)
Data_sf <- Data
coordinates(Data_sf) <- c("Longitude","Latitude")
class(Data_sf)

library(sf)
Data_sf <- st_as_sf(Data_sf)
# Mengatur sistem koordinat projektif (Bali NTB berada di DGN95 / UTM Zone 50S)
st_crs(Data_sf) <- st_crs("+proj=utm +zone=50 +south +datum=WGS84")

# Menghitung matriks jarak antara titik-titik
matriks_jarak <- st_distance(Data_sf)
diag(matriks_jarak) <- NA
matriks_jarak

# Temukan indeks dari nilai jarak terkecil dan terbesar
jarak_terkecil <- min(matriks_jarak, na.rm = TRUE)
jarak_terbesar <- max(matriks_jarak, na.rm = TRUE)

indeks_jarak_terkecil <- which(matriks_jarak == jarak_terkecil, arr.ind = TRUE)
indeks_jarak_terbesar <- which(matriks_jarak == jarak_terbesar, arr.ind = TRUE)

# Titik dengan jarak terkecil
print("Pasangan titik dengan jarak terkecil:")
indeks_jarak_terkecil

# Titik dengan jarak terbesar
print("Pasangan titik dengan jarak terbesar:")
indeks_jarak_terbesar

# writexl::write_xlsx(data.frame(matriks_jarak),"Matriks Jarak.xlsx")

