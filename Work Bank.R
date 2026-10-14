library(ggplot2)
library(dplyr)

# 1. Crear el dataframe directamente en R
datos_limpios <- data.frame(
  country = c("Perú", "Chile", "Estados Unidos", "Alemania", "Japón", "Brasil", "México", "India", "Argentina", "Colombia", "Francia", "Reino Unido", "Canadá", "España", "China"),
  region = c("América Latina", "América Latina", "Norteamérica", "Europa", "Asia", "América Latina", "América Latina", "Asia", "América Latina", "América Latina", "Europa", "Europa", "Norteamérica", "Europa", "Asia"),
  income = c("Upper middle income", "High income", "High income", "High income", "High income", "Upper middle income", "Upper middle income", "Lower middle income", "Upper middle income", "Upper middle income", "High income", "High income", "High income", "High income", "Upper middle income"),
  pib_pc = c(6600, 15000, 70000, 50000, 39000, 7500, 10000, 2200, 10600, 6100, 44000, 46500, 52000, 30000, 12500),
  co2_pc = c(2.1, 4.6, 14.2, 8.1, 8.5, 2.4, 3.0, 1.9, 4.2, 1.8, 4.8, 5.1, 15.5, 5.3, 8.0)
)

# 2. Gráfico 1: Diagrama de Dispersión
ggplot(datos_limpios, aes(x = pib_pc, y = co2_pc, color = income)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_x_log10() + 
  scale_y_log10() +
  labs(
    title = "PIB per cápita vs Emisiones de CO2",
    subtitle = "Datos del Banco Mundial",
    x = "PIB per cápita (Escala log)",
    y = "Emisiones de CO2 per cápita (Escala log)",
    color = "Nivel de Ingresos"
  ) +
  theme_minimal()

# 3. Análisis Estadístico: Correlación de Pearson
correlacion <- cor(datos_limpios$pib_pc, datos_limpios$co2_pc, use = "complete.obs")
cat("El coeficiente de correlación de Pearson es:", correlacion, "\n")

# 4. Tabla Resumen por Nivel de Ingresos
resumen_ingresos <- datos_limpios %>%
  group_by(income) %>%
  summarise(
    N_paises = n(),
    PIB_Promedio = mean(pib_pc, na.rm = TRUE),
    CO2_Promedio = mean(co2_pc, na.rm = TRUE)
  )

print(resumen_ingresos)

# 5. Gráfico 2: Boxplot de Emisiones
ggplot(datos_limpios, aes(x = income, y = co2_pc, fill = income)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Distribución de Emisiones de CO2 según Nivel de Ingresos",
    subtitle = "Datos del Banco Mundial",
    x = "Nivel de Ingresos del País",
    y = "Emisiones de CO2 per cápita (Escala log)",
    fill = "Ingresos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))






```r
library(ggplot2)
library(dplyr)

# 1. Calcular la correlación de Pearson
resultado <- cor.test(
  datos_limpios$pib_pc,
  datos_limpios$co2_pc,
  method = "pearson"
)

# Extraer resultados
r <- round(resultado$estimate, 3)
p <- signif(resultado$p.value, 3)

# Crear texto para el gráfico
texto <- paste0(
  "Correlación de Pearson (r) = ", r,
  "\nValor p = ", p,
  "\nN = ", nrow(datos_limpios)
)

# 2. Crear el gráfico
ggplot(datos_limpios,
       aes(x = pib_pc, y = co2_pc)) +
  
  # Puntos de cada país
  geom_point(aes(color = income),
             size = 3.5, alpha = 0.8) +
  
  # Línea de tendencia lineal
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,
              color = "black",
              linewidth = 1) +
  
  # Mostrar la correlación en el gráfico
  annotate("label",
           x = Inf, y = Inf,
           label = texto,
           hjust = 1.1, vjust = 1.2,
           size = 4,
           fill = "white") +
  
  # Etiquetas
  labs(
    title = "Correlación entre el PIB per cápita y las emisiones de CO2",
    subtitle = "Análisis de 15 países",
    x = "PIB per cápita (US$)",
    y = "Emisiones de CO2 per cápita",
    color = "Nivel de ingresos",
    caption = "Fuente declarada: datos del Banco Mundial"
  ) +
  
  theme_minimal(base_size = 12)

# 3. Mostrar los resultados estadísticos en la consola
cat("Correlación de Pearson:", round(r, 3), "\n")
cat("Valor p:", p, "\n")
cat("Intervalo de confianza del 95%:",
    round(resultado$conf.int[1], 3), "a",
    round(resultado$conf.int[2], 3), "\n")
```







```r
# Instalar los paquetes una sola vez si aún no los tienes
# install.packages(c("DT", "dplyr", "tidyr", "ggplot2"))

library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Mostrar la tabla original de países
datatable(
  datos_limpios,
  caption = "Datos económicos y emisiones de CO2 por país",
  extensions = "Buttons",
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel"),
    searching = TRUE
  )
)

# 2. Cambiar columnas por filas
tabla_transpuesta <- datos_limpios %>%
  pivot_longer(
    cols = -country,
    names_to = "Variable",
    values_to = "Valor"
  )

datatable(
  tabla_transpuesta,
  caption = "Variables organizadas en filas",
  options = list(pageLength = 10, scrollX = TRUE)
)

# 3. Mostrar los países como columnas y las variables como filas
tabla_ancha <- datos_limpios %>%
  pivot_longer(
    cols = -country,
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    names_from = country,
    values_from = Valor
  )

datatable(
  tabla_ancha,
  caption = "Tabla con países en columnas",
  options = list(scrollX = TRUE, pageLength = 10)
)

# 4. Resumen estadístico por nivel de ingresos
resumen <- datos_limpios %>%
  group_by(income) %>%
  summarise(
    Numero_paises = n(),
    PIB_promedio = round(mean(pib_pc), 2),
    CO2_promedio = round(mean(co2_pc), 2),
    .groups = "drop"
  )

datatable(
  resumen,
  caption = "Resumen por nivel de ingresos",
  options = list(pageLength = 10)
)
```








```rfinal
# Transponer la tabla completa
tabla_transpuesta <- as.data.frame(
  t(datos_limpios),
  stringsAsFactors = FALSE
)

# Convertir los nombres de las columnas en nombres genéricos
colnames(tabla_transpuesta) <- paste0(
  "Columna_", seq_len(ncol(tabla_transpuesta))
)

# Mostrar la tabla transpuesta
library(DT)

datatable(
  tabla_transpuesta,
  caption = "Tabla transpuesta: filas convertidas en columnas",
  options = list(
    pageLength = 10,
    scrollX = TRUE
  ),
  rownames = TRUE
)
```



