# --- Llibres necessàries ---
library(sf)             # per treballar amb dades espacials (polígons)
library(dplyr)          # per manipulació de dades
library(cartogram)      # per generar cartogrames (deformacions per pes)
library(rnaturalearth)  # per obtenir dades geogràfiques del món
library(ggplot2)        # per visualització
library(stringr)        # per tractament de cadenes de text
library(viridis)        # per escales de color perceptuals

# --- Dataset de CO₂ ---
df <- Co2   # dataset prèviament carregat (Our World in Data)

# --- Filtrat i selecció de dades per a l'any 2023 ---
co2_2023 <- df %>%
  filter(year == 2023) %>%                        # seleccionem només 2023
  filter(!is.na(co2percapita)) %>%                # eliminem valors buits
  filter(nchar(isocode) == 3, !str_starts(isocode, "OWID")) %>%  # excloem codis no de països
  select(isocode, country, co2percapita)          # deixem només les columnes útils

# --- Geometria dels països ---
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%                             # repara possibles errors de geometria
  # Projecció equal-area (necessària per a la deformació proporcional)
  st_transform(crs = "+proj=moll")                # Projecció Mollweide

# --- Unim les dades de CO₂ amb els polígons del món ---
world_dat <- world %>%
  left_join(co2_2023, by = c("iso_a3_eh" = "isocode"))  # unim per codi ISO3

# --- Taula auxiliar ordenada (no necessària per al gràfic però útil per comprovar valors) ---
tabla_co2 <- world_dat %>%
  st_drop_geometry() %>%                          # eliminem geometria
  select(country = name, co2percapita) %>%        # deixem només país i valor
  arrange(desc(co2percapita))                     # ordenem de major a menor

# --- Eliminem països sense dades per evitar errors en el cartograma ---
world_dat <- world_dat %>% filter(!is.na(co2percapita))

# --- Generació del cartograma continu ---
# La superfície de cada país es deforma proporcionalment al seu valor de CO₂ per càpita
set.seed(123)
carto_co2 <- cartogram_cont(world_dat, weight = "co2percapita", itermax = 8)

# --- Creació del gràfic ---
p <- ggplot(carto_co2) +
  geom_sf(aes(fill = co2percapita), color = NA) +      # polígons dels països
  scale_fill_viridis(                                  # escala de color perceptual
    option = "C", direction = 1, name = "CO₂ per càpita",
    na.value = "grey90"
  ) +
  coord_sf() +
  labs(
    title = "Cartograma real de CO₂ per càpita (2023)",
    subtitle = "Deformació de la superfície dels països proporcional a les emissions per habitant",
    caption = "Font: Our World in Data (OWID) • Base geomètrica: Natural Earth • Projecció: Mollweide (equal-area)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    panel.grid = element_blank()
  )

# --- Mostrem el gràfic ---
print(p)
