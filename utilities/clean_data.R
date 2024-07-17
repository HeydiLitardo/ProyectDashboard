# clean_data.R

library(dplyr)
library(readr)

function(filepath) {
  # Leer el archivo CSV completo
  raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)
  
  # Extraer la segunda fila como encabezado
  headers <- raw_data[2, ]
  
  # Hacer que los nombres de las columnas sean únicos
  headers <- make.names(headers, unique = TRUE)
  
  # Remover las dos primeras filas y asignar los nuevos encabezados
  cleaned_data <- raw_data[-c(1, 2), ]
  colnames(cleaned_data) <- headers
  
  # Convertir fechas y limpiar datos adicionales si es necesario
  cleaned_data <- cleaned_data %>%
    mutate(`Fecha.y.hora` = as.POSIXct(`Fecha.y.hora`, format = "%d/%m/%Y, %I:%M:%S %p", tz = "UTC")) %>%
    filter(!is.na(`Fecha.y.hora`))
  
  return(cleaned_data)
}
