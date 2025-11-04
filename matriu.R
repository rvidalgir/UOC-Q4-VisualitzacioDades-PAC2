# --- Llibres necessàries ---
library(ggplot2)   # per crear gràfics
library(reshape2)  # per convertir matrius en format llarg (melt)

# --- Dades i càlcul de correlacions ---
data("iris")                # carrega el dataset 'iris' integrat a R
df <- iris[, 1:4]            # seleccionem només les variables numèriques
cor_matrix <- cor(df, use = "complete.obs")  # calculem la matriu de correlació

# --- Transformació per poder visualitzar amb ggplot ---
melted <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "Corr")

# --- Creació del heatmap de correlacions ---
p <- ggplot(melted, aes(Var1, Var2, fill = Corr)) +
  geom_tile(color = "white", linewidth = 0.6) +  # cada cel·la és un rectangle
  # Escala de colors: blau per correlacions negatives, vermell per positives
  scale_fill_gradient2(
    low = "#3B4CC0", mid = "white", high = "#B40426",
    midpoint = 0, limits = c(-1, 1), name = "Correlació"
  ) +
  # Mostra el valor numèric dins de cada cel·la
  geom_text(aes(label = sprintf("%.2f", Corr)),
            color = "black", size = 4.2, fontface = "bold") +
  # Títols i peu de figura
  labs(
    title = "Matriu de correlacions — Iris dataset",
    subtitle = "Relació entre les dimensions dels sèpals i pètals per espècie",
    caption = "Font: Dades integrades al paquet 'datasets' de R (Iris dataset, Fisher, 1936)",
    x = NULL, y = NULL
  ) +
  # Estil visual net i llegible
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # gira els noms per llegibilitat
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# --- Mostra el gràfic ---
print(p)
