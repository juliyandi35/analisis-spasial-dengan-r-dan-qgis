# Memuat library yang diperlukan
library(caret)

# Menghasilkan data contoh
set.seed(123)  # Menentukan seed untuk reproduktibilitas
data <- iris  # Menggunakan dataset iris sebagai contoh

# Memuat fungsi createDataPartition untuk pembagian data
train_index <- createDataPartition(y = data$Species, p = 0.9, list = FALSE)

# Memisahkan data menjadi data train dan data test
data_train <- data[train_index, ]
data_test <- data[-train_index, ]

# Menampilkan jumlah baris pada data train dan data test
print(paste("Jumlah baris data train:", nrow(data_train)))
print(paste("Jumlah baris data test:", nrow(data_test)))
