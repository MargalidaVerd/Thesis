# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Crear la tabla de datos
df <- data.frame(
  Country = c("Spain", "Netherlands", "Switzerland", "Sweden", "Latvia", "Romania"),
  Training = c(60, 61, 60, 61, 31, 50),
  Testing = c(11, 12, 11, 11, 11, 10)
)

# Transformar a formato largo
df_long <- df %>%
  tidyr::pivot_longer(cols = c("Training", "Testing"),
                      names_to = "Set",
                      values_to = "Years")

# Ordenar los países en orden de aparición original
df_long$Country <- factor(df_long$Country, levels = df$Country)

# Dibujar gráfico de barras apiladas
ggplot(df_long, aes(x = Country, y = Years, fill = Set)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Training" = "#1f77b4", "Testing" = "#ff7f0e")) +
  labs(title = "Training and Testing Periods by Country",
       y = "Years",
       x = "Country",
       fill = "Dataset") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
