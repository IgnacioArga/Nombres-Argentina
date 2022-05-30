
# Importo librerias a utilizar --------------------------------------------

library(dplyr)
library(ggplot2)

# Importar datos ----------------------------------------------------------

nombres_arg <- read.csv("historico-nombres_ok.csv")

nombres_arg %>% head(5)

# Selecciono la informacion que me interesa -------------------------------

df_graficar_anio <- nombres_arg %>% 
  filter(
    nombre %in% c("Juan", "Maria", "Diego", "Eva"),
    anio >= 1950,
    anio <  2010
  ) %>% 
  select(-X) %>% 
  mutate(decada = anio %/% 10 * 10)

# Muestro información por década ------------------------------------------

df_graficar_decada <- df_graficar_anio %>% 
  select(-anio) %>% 
  group_by(nombre, decada) %>% 
  summarise(cantidad = sum(cantidad))

# Grafico la informacion --------------------------------------------------

# ESTRUCTURA GENERAL DE GGPLOT
# 
# ggplot(data = <DATA_FRAME>) +
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPING>),
#     stat = <STAT>,
#     position = <POSITION>
#   )+
#   <COORDINATE_FUNCTION>() +
#   <FACET_FUNCTION>()

# Grafico cantidad por decada
ggplot(data = df_graficar_decada) +
  geom_col(mapping = aes(x = decada, y = cantidad))

# Grafico cantidad por decada y nombre
ggplot(data = df_graficar_decada) +
  geom_col(mapping = aes(x = decada, y = cantidad, fill = nombre)) + 
  theme_classic()

# Grafico cantidad por decada y nombre en base 100%
ggplot(data = df_graficar_decada) +
  geom_col(mapping = aes(x = decada, y = cantidad, fill = nombre),
           position = "fill") + 
  theme_classic()

# Grafico cantidad por decada y nombre girando eje
ggplot(data = df_graficar_decada) +
  geom_col(mapping = aes(x = decada, y = cantidad, fill = nombre)) +
  coord_flip()

# Grafico cantidad por anio y nombre
ggplot(data = df_graficar_anio) +
  geom_col(mapping = aes(x = anio, y = cantidad, fill = nombre))

# Grafico cantidad por anio y nombre
ggplot(data = df_graficar_anio) +
  geom_col(mapping = aes(x = anio, y = cantidad, fill = nombre)) +
  facet_wrap(~nombre)

# Grafico dispersion de cantidades anuales por nombre
ggplot(data = df_graficar_anio) +
  geom_boxplot(mapping = aes(x = nombre, y = cantidad))

# Grafico dispersion de cantidades anuales por nombre y decada (y lo ponemos más lindo)
ggplot(data = df_graficar_anio) +
  geom_boxplot(mapping = aes(x = nombre, y = cantidad, fill = nombre)) +
  facet_wrap(~decada) +
  theme_bw()

# Podemos agregarle dinamismo al gráfico y combinar varios gráficos
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)

anim <- ggplot(
    data = df_graficar_anio, 
    aes(x = anio, y = cantidad, color = nombre)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  transition_reveal(anio)

animate(anim, 200, fps = 10, 
        renderer = gifski_renderer("gganim.gif"))


# Grafico animado bonus
 
staticplot <- nombres_arg %>%
  select(-X) %>% 
  arrange(nombre, anio) %>% 
  group_by(nombre) %>% 
  mutate(cantidad = cumsum(cantidad)) %>% 
  ungroup() %>% 
  group_by(anio) %>%
  mutate(rank = rank(-cantidad),
         Value_rel = cantidad/cantidad[rank == 1],
         Value_lbl = paste0(" ",round(cantidad/1000))) %>%
  group_by(nombre) %>% 
  filter(rank <= 10) %>%
  ggplot(
    aes(rank, group = nombre, fill = as.factor(nombre), color = as.factor(nombre))) +
  geom_tile(aes(y = cantidad/2,
                height = cantidad,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(nombre, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = cantidad,label  =  Value_lbl, hjust = 0)) +
  coord_flip(clip  =  "off", expand  =  FALSE) +
  scale_y_continuous(labels  =  scales::comma) +
  scale_x_reverse() +
  guides(color  =  FALSE, fill  =  FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x  =  element_line( size = .1, color = "grey" ),
        panel.grid.minor.x  =  element_line( size = .1, color = "grey" ),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", colour = "grey", vjust = -1),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "grey"),
        plot.caption  = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
        plot.background = element_blank(),
        plot.margin  =  margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(anio, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Cantidad de nombres por año : {closest_state}',  
       subtitle  =  "Top 10 Nombres",
       caption  = "Cantidad de nombres en miles")

animate(anim, 200, fps = 5, 
        renderer = gifski_renderer("gganim.gif"))
