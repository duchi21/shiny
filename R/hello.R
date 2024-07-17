
library(readxl)
library(ggplot2)
library(dplyr)
analizar_excel <- function(archivo_excel, hoja = 1) {
  # Leer el archivo de Excel
  datos <- read_excel(archivo_excel, sheet = hoja)

  # Seleccionar solo las columnas numéricas
  datos_numericos <- datos %>% select_if(is.numeric)

  # Graficar las columnas numéricas
  for (columna in colnames(datos_numericos)) {
    ggplot(datos, aes_string(x = columna)) +
      geom_histogram(binwidth = 30, fill = "blue", color = "black") +
      labs(title = paste("Histograma de", columna),
           x = columna, y = "Frecuencia") +
      theme_minimal() +
      ggsave(filename = paste0("histograma_", columna, ".png"))
  }

  # Realizar análisis estadísticos
  resumen_estadistico <- datos_numericos %>%
    summarise_all(funs(
      media = mean(., na.rm = TRUE),
      mediana = median(., na.rm = TRUE),
      desviacion_estandar = sd(., na.rm = TRUE),
      minimo = min(., na.rm = TRUE),
      maximo = max(., na.rm = TRUE),
      cuartil_1 = quantile(., 0.25, na.rm = TRUE),
      cuartil_3 = quantile(., 0.75, na.rm = TRUE)
    ))

  print(resumen_estadistico)

  return(list(
    datos_numericos = datos_numericos,
    resumen_estadistico = resumen_estadistico
  ))
}
e<-analizar_excel("C:/Users/CIENCIAS/Downloads/bd_p2.xlsx")
e
