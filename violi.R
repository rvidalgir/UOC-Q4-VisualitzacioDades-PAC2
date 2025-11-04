# --- Llibres necessàries ---
library(ggplot2)   # per crear el gràfic

# --- Dataset ---
df <- life_expectancy_continent   # dataset amb esperança de vida, continent i nivell de desenvolupament

# --- Preparació de variables ---
# Ordenem el factor "Status" per mostrar primer "Developed" i després "Developing"
df$Status <- factor(df$Status, levels = c("Developed", "Developing"))

# Definim la posició de desplaçament (per separar els violins per continent dins de cada categoria)
pd <- position_dodge(width = 0.8)

# --- Creació del gràfic de violí ---
p <- ggplot(df, aes(x = Status, y = Life.expectancy, fill = Continent)) +
  # Cada violí representa la distribució de valors per continent i nivell de desenvolupament
  geom_violin(
    position = pd, width = 0.7, trim = TRUE, color = "grey25", linewidth = 0.3
  ) +
  # Paleta de colors suau i llegible
  scale_fill_brewer(palette = "Set2", name = "Continent") +
  # Títols, eixos i peu de figura
  labs(
    title = "Distribució de l’esperança de vida per nivell de desenvolupament",
    subtitle = "Violins agrupats per continent",
    x = "Nivell de desenvolupament", y = "Esperança de vida (anys)",
    caption = "Font: WHO Life Expectancy (via Kaggle) · Elaboració pròpia amb R"
  ) +
  # Tema minimalista per millorar la llegibilitat
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# --- Mostrem el gràfic ---
p
