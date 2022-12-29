library(calendR)
library(tidyverse)
library(readxl)
library(lubridate)




Actividades_II_Semestre <- read_excel("~/Documents/Base de Datos SFJ/Actividades II Semestre.xlsx", 
                                      col_types = c("date", "numeric"))
View(Actividades_II_Semestre)



tz(Actividades_II_Semestre$Fecha) <- "America/Santiago"

Actividades <- Actividades_II_Semestre %>%
  transmute(Fecha = ymd(`Fecha`), Cantidad) %>%
  group_by(Fecha) %>%
  summarise(REGISTROS = sum(Cantidad)) %>%
  ungroup() %>%
  mutate(DIA = wday(Fecha, label = T, week_start = 1, locale = "es_ES")) %>%
  mutate(MES = month(Fecha, label = T, locale = "es_ES")) %>%
  mutate(SEMANA = isoweek(Fecha))

Actividades$SEMANA[Actividades$MES=="dic" & Actividades$SEMANA == 1] = 53 

Actividades <- Actividades %>%
  group_by(MES) %>%
  mutate(SEMANA_MES = 1 + SEMANA - min(SEMANA))

Actividades %>%
  ggplot(aes(x = DIA, y = -SEMANA_MES, fill = REGISTROS)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = day(Fecha)), size = 2.5, color = "black") +
  theme(aspect.ratio = 1/2,
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold",
                                  margin = margin(0,0,0.0,0, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "bold",
                                     margin = margin(0,0,0.0,0, unit = "cm"))) +
  scale_fill_viridis_c(name = "Registros", direction = -1, 
                       guide = guide_colourbar(title.position = "top", 
                                               direction = "vertical")) +
  facet_wrap(~MES, nrow = 3, ncol = 3, scales = "free") +
  labs(title = "Actividades",
       subtitle = "II Semestre 2021",
       caption = "Fuente: DIRAC")