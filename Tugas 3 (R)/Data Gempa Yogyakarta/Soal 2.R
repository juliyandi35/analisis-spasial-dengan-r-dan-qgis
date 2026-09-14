library(readxl)
Data <- read_excel("Data Gempa Yogya.xlsx")
Data <- data.frame(Data)
colnames(Data)
colnames(Data) <- c("No","Event.ID","Date.time","Latitude",
                    "Longitude","Magnitude","Mag.Type","Depth(km)",
                    "Phase.Count","Azimuth.Gap","Location","Agency")
Data$`Depth(km)`<- Data$`Depth(km)`*-1

library(plotly)
# Membuat scatter plot 3D dengan plotly
plot_ly(data = Data, x = ~Longitude, y = ~Latitude, z = ~`Depth(km)`, color = ~Magnitude, type = "scatter3d", mode = "markers")
