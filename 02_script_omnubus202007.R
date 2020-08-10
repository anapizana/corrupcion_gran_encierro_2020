# Paquetes ----
require(foreign)
require(mxmaps)
require(haven)
require(srvyr)
require(dplyr)
require(ggpubr)
require(gtable)
require(grid)
require(gridExtra)
require(tidyverse)

# Paleta de colores ----
# #'000c2d' (azul oscuro)
# #'f72732' (rojo mcci)
# #'fff72d' (crema)
# #'ffffff' (blanco)
# #'333333' (texto)

mcci_discrete <- c(
  '#000c2d', '#0E9A9D', '#8a8c91', '#ecd756', '#f72732'
)

# Directorios ----
inp <- "MCCI/omnibus_202007/01_datos/"
out <- "MCCI/omnibus_202007/03_gráficas/"


# Datos ----
d <- read_sav(paste0(inp, "ONT2 Base MCCI.zsav")) %>% 
  haven::as_factor() %>% 
  rename_all(tolower)

# Recodificación ----
vector_fiuf <- 
  c(
    d$mcci1%>% attr('label'),
    d$mcci2 %>% attr('label'),
    d$mcci3 %>% attr('label'),
    d$mcci4 %>% attr('label'),
    d$mcci5 %>% attr('label'),
    d$mcci5otro %>% attr('label'),
    d$mcci6 %>% attr('label'),
    d$a_mcci7_1 %>% attr('label'),
    d$a_mcci7_2 %>% attr('label'),
    d$a_mcci7_3 %>% attr('label'),
    d$a_mcci7_4 %>% attr('label'),
    d$a_mcci7_5 %>% attr('label'),
    d$a_mcci7_6 %>% attr('label'),
    d$a_mcci7_7 %>% attr('label'),
    d$a_mcci7_8 %>% attr('label'),
    d$a_mcci7_9 %>% attr('label'),
    d$a_mcci7_10 %>% attr('label'),
    d$a_mcci7_11 %>% attr('label'),
    d$a_mcci7_12 %>% attr('label'),
    d$a_mcci7_13 %>% attr('label'),
    d$a_mcci7_13otro %>% attr('label'),
    d$a_mcci7_14 %>% attr('label'),
    d$d1 %>% attr('label'),
    d$d2 %>% attr('label'),
    d$d3 %>% attr('label'),
    d$d5 %>% attr('label'),
    d$d6 %>% attr('label'),
    d$d6otro %>% attr('label'),
    d$d6b %>% attr('label'),
    d$d6botro %>% attr('label'),
    d$pa %>% attr('label'),
    d$pb %>% attr('label'),
    d$pc %>% attr('label'),
    d$pc2 %>% attr('label'),
    d$pf %>% attr('label'),
    d$pg %>% attr('label'),
    d$edo %>% attr('label'),
    d$edadrec %>% attr('label'),
    d$escolrec %>% attr('label')
  )

v_names <- tolower(vector_fiuf)
v_names <- trimws(v_names)
v_names <- str_replace_all(v_names, "[[:punct:]]", "")
v_names <- str_replace_all(v_names, " ", "_")


recode_names <- function(x){
  x = 
    case_when(
      x == "mcci1" ~ paste0("v_mcci1_", v_names[1]),
      x == "mcci2" ~ paste0("v_mcci2_", v_names[2]),
      x == "mcci3" ~ paste0("v_mcci3_", v_names[3]),
      x == "mcci4" ~ paste0("v_mcci4_", v_names[4]),
      x == "mcci5" ~ paste0("v_mcci5_", v_names[5]),
      x == "mcci5otro" ~ paste0("v_mcci5otro_", v_names[6]),
      x == "mcci6" ~ paste0("v_mcci6_", v_names[7]),
      x == "a_mcci7_1" ~ paste0("v_a_mcci7_1_", v_names[8]),
      x == "a_mcci7_2" ~ paste0("v_a_mcci7_2_", v_names[9]),
      x == "a_mcci7_3" ~ paste0("v_a_mcci7_3_", v_names[10]),
      x == "a_mcci7_4" ~ paste0("v_a_mcci7_4_", v_names[11]),
      x == "a_mcci7_5" ~ paste0("v_a_mcci7_5_", v_names[12]),
      x == "a_mcci7_6" ~ paste0("v_a_mcci7_6_", v_names[13]),
      x == "a_mcci7_7" ~ paste0("v_a_mcci7_7_", v_names[14]),
      x == "a_mcci7_8" ~ paste0("v_a_mcci7_8_", v_names[15]),
      x == "a_mcci7_9" ~ paste0("v_a_mcci7_9_", v_names[16]),
      x == "a_mcci7_10" ~ paste0("v_a_mcci7_10_", v_names[17]),
      x == "a_mcci7_11" ~ paste0("v_a_mcci7_11_", v_names[18]),
      x == "a_mcci7_12" ~ paste0("v_a_mcci7_12_", v_names[19]),
      x == "a_mcci7_13" ~ paste0("v_a_mcci7_13_", v_names[20]),
      x == "a_mcci7_13otro" ~ paste0("v_a_mcci7_13otro_", v_names[21]),
      x == "a_mcci7_14" ~ paste0("v_a_mcci7_14_", v_names[22]),
      x == "d1" ~ paste0("v_d1_", v_names[23]),
      x == "d2" ~ paste0("v_d2_", v_names[24]),
      x == "d3" ~ paste0("v_d3_", v_names[25]),
      x == "d5" ~ paste0("v_d5_", v_names[26]),
      x == "d6" ~ paste0("v_d6_", v_names[27]),
      x == "d6otro" ~ paste0("v_d6otro_", v_names[28]),
      x == "d6b" ~ paste0("v_d6b_", v_names[29]),
      x == "d6botro" ~ paste0("v_d6botro_", v_names[30]),
      x == "pa" ~ "cruce_sexo",
      x == "pg" ~ "cruce_ingreso",
      x == "pb" ~ "supr_pb",
      x == "pc" ~ "supr_pc",
      x == "pc2" ~ "supr_pc2",
      x == "pf" ~ "supr_pf",
      x == "pg" ~ "supr_pg",
      x == "edo" ~ "supr_edo",
      x == "edadrec" ~ "cruce_edad",
      x == "escolrec" ~ "cruce_escol"
    )
}


data <- d %>% 
  rename_at(
    vars(starts_with("mcci")),
    funs(recode_names)
  ) %>% 
  rename_at(
    vars(starts_with("a_mcci")),
    funs(recode_names)
  ) %>% 
  rename_at(
    vars(starts_with("d")),
    funs(recode_names)
  ) %>% 
  rename_at(
    vars(starts_with("p")),
    funs(recode_names)
  ) %>% 
  rename_at(
    vars(starts_with("e")),
    funs(recode_names)
  )%>% 
  select(
    sbjnum, starts_with("cruce"), starts_with("v_"), weight = wt
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("v_")), 
    .funs = funs(factor)
  ) %>% 
  mutate_at(
    .vars = vars(starts_with("cruce_")),
    .funs = funs(factor)
  ) 

data$weight <- as.numeric(data$weight)
data[data == "NS / NC"] <- NA

# Proporciones simples ----
# * Tabla de frecuencias ----
v_names_loop <- names(data)[6:35]

test <- data.frame()
for (i in 1:length(v_names_loop)){
  tempo <- data %>% 
    select(v_names_loop[i], weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(starts_with("v_")),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(var_v = starts_with("v_")) %>% 
    mutate(
      v_id = str_replace_all(v_names_loop[i], "v_", "")
    ) %>% 
    drop_na(var_v) 
  
  test <- bind_rows(test, tempo)
  rm(tempo)
}

prop_simples <- test %>% 
  mutate_at(
    vars(starts_with("prop")),
    funs(round(.*100,2))
  ) %>% 
  mutate_at(
    vars(starts_with("var_v")),
    funs(
      ifelse(
        var_v=="Si", "Sí", as.character(var_v)
      )
    )
  )
beepr::beep(1)

openxlsx::write.xlsx(
  prop_simples, paste0(out, "00_simples/00_props_simples.xlsx")
)  


# * Gráficas (rápidas) ----
v_names_loop_gg <- str_remove_all(v_names_loop, "v_")
v_num <- sub("\\_.*", "", v_names_loop_gg)
v_num[8:22] <- 
  c(
    "a_mcci7_1",
    "a_mcci7_2",
    "a_mcci7_3",
    "a_mcci7_4",
    "a_mcci7_5",
    "a_mcci7_6",
    "a_mcci7_7",
    "a_mcci7_8",
    "a_mcci7_9",
    "a_mcci7_10",
    "a_mcci7_11",
    "a_mcci7_12",
    "a_mcci7_13",
    "a_mcci7_13_otro",
    "a_mcci7_14"
  )

vector_fiuf <- vector_fiuf[1:30]
vector_fiuf[1] <- paste0(vector_fiuf[1],"a?")

fiuffi <- "Fuente: Omnibús telefónico Data OPM | julio 2020"

a <- prop_simples

for(i in 1:length(v_names_loop_gg)){
  fiuf <- vector_fiuf[i]
  ggplot(a %>% filter(v_id == v_names_loop_gg[i]), 
         aes(x = reorder(str_wrap(var_v,10), -prop), 
             y = prop, 
             label = paste0(prop, "%"),
             fill = prop)) + 
    geom_bar(stat = "identity", width = .3) +
    geom_text(size = 7, vjust = -0.5) +
    scale_y_continuous(
      limits = c(0,100),
      breaks = seq(0,100,25),
      labels = paste0(
        as.character(seq(0,100,25)), "%"
      )
    ) + 
    scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
    labs(title= str_wrap(fiuf, width = 75),
         caption = fiuffi) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 20),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Arial Narrow"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "none")
  
  
  ggsave(filename = paste0(
    out,"00_simples_rápidas/", v_num[i], ".png"
  ), width = 15, height = 10, dpi = 100, bg = "transparent")
}
beepr::beep(1)
rm(i, a)

# * Gráficas ----
v_names_loop_gg <- str_remove_all(v_names_loop, "v_")
v_num <- sub("\\_.*", "", v_names_loop_gg)
v_num[8:22] <- 
  c(
    "a_mcci7_1",
    "a_mcci7_2",
    "a_mcci7_3",
    "a_mcci7_4",
    "a_mcci7_5",
    "a_mcci7_6",
    "a_mcci7_7",
    "a_mcci7_8",
    "a_mcci7_9",
    "a_mcci7_10",
    "a_mcci7_11",
    "a_mcci7_12",
    "a_mcci7_13",
    "a_mcci7_13_otro",
    "a_mcci7_14"
  )


fiuffi <- "Fuente: Omnibús telefónico Data OPM | julio 2020"


# ** Gráfica 1 ----
i = 1
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i])
fiuf <- vector_fiuf[i]

c <- round(a$prop)
names(c) <- paste0(a$var_v, " [", round(a$prop, 2), "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  


ggplot(df, aes(x = x, y = y, fill = str_wrap(category, 25))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 2 ----
i = 2
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(orden = case_when(
    var_v == "Aumentado" ~ 1,
    str_detect(var_v, "Igual") ~ 2,
    var_v == "Disminuido" ~ 3,
    T ~ 4
  ))
fiuf <- vector_fiuf[i]
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[2],
                                   mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 2.1 (comparado con reforma) ----
i = 2
a <- 
  filter(
    readxl::read_excel("MCCI/encuesta_2020/01_datos/datos_gráficas.xlsx") %>% 
      mutate(props = as.numeric(props)), id=="T1_2"
  ) %>% 
  mutate(
    var_v = case_when(
      variables == "Aumentó" ~ "Aumentado",
      str_detect(variables, "igual") ~ "Permaneció igual",
      variables == "Disminuyó" ~ "Disminuido",
      T ~ variables
    ),
    orden = case_when(
      var_v == "Aumentado" ~ 1,
      str_detect(var_v, "igual") ~ 2,
      var_v == "Disminuido" ~ 3,
      T ~ 4
    ),
    id = paste0("Encuesta nacional en vivienda\nMCCI-Reforma | marzo ", año)
  ) %>% 
  select(var_v, prop = props, orden, id) %>% 
  bind_rows(
    prop_simples %>% 
      filter(v_id == v_names_loop_gg[i]) %>% 
      mutate(
        var_v = case_when(
          str_detect(var_v, "Igual") ~ "Permaneció igual",
          T ~ var_v
        ),
        orden = case_when(
          var_v == "Aumentado" ~ 1,
          str_detect(var_v, "igual") ~ 2,
          var_v == "Disminuido" ~ 3,
          T ~ 4
        ),
        id = "Encuesta nacional telefónica\nMCCI-Data OPM | julio 2020"
      ) %>% 
      select(var_v, prop, orden, id)
  )
fiuf <- vector_fiuf[i]

ggplot(a, aes(y = reorder(var_v, -as.numeric(orden)),
              x = prop,
              label = paste0(prop, "%"),
              fill = as.factor(id))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,75),
    breaks = seq(0,75,25),
    labels = paste0(
      as.character(seq(0,75,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], 
                                   mcci_discrete[2], 
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = "Fuente: Encuestas MCCI-Reforma y MCCI-Data OPM\nNota: la última medición no es comparable debido al diseño muestral.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.y = unit(1.0, 'cm'),
        legend.position = "right")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], "_comparada.png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 3 ----
i = 3
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(orden = case_when(
    var_v == "Aumentará" ~ 1,
    str_detect(var_v, "Igual") ~ 2,
    var_v == "Disminuirá" ~ 3,
    T ~ 4
  ))
fiuf <- vector_fiuf[i]
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[2],
                                   mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 3.1 (comparado con reforma) ----
i = 3
a <- 
  filter(
    readxl::read_excel("MCCI/encuesta_2020/01_datos/datos_gráficas.xlsx") %>% 
      mutate(props = as.numeric(props), orden = as.numeric(orden)), id=="T1_4"
  ) %>% 
  mutate(
    id = paste0("Encuesta nacional en vivienda\nMCCI-Reforma | marzo ", año)
  ) %>% 
  select(var_v = variables, prop = props, orden, id) %>% 
  bind_rows(
    prop_simples %>% 
      filter(v_id == v_names_loop_gg[i]) %>% 
      mutate(
        var_v = case_when(
          str_detect(var_v, "Igual") ~ "Permanecerá igual",
          T ~ var_v
        ),
        orden = case_when(
          var_v == "Aumentará" ~ 1,
          str_detect(var_v, "Igual") ~ 2,
          var_v == "Disminuirá" ~ 3,
          T ~ 4
        ),
        id = "Encuesta nacional telefónica\nMCCI-Data OPM | julio 2020"
      ) %>% 
      select(var_v, prop, orden, id)
  )
fiuf <- vector_fiuf[i]

ggplot(a, aes(y = reorder(var_v, -as.numeric(orden)),
              x = prop,
              label = paste0(prop, "%"),
              fill = as.factor(id))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,75),
    breaks = seq(0,75,25),
    labels = paste0(
      as.character(seq(0,75,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], 
                                   mcci_discrete[2], 
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = "Fuente: Encuestas MCCI-Reforma y MCCI-Data OPM\nNota: la última medición no es comparable debido al diseño muestral.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.y = unit(1.0, 'cm'),
        legend.position = "right")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], "_comparada.png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 4 ----
i = 4
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      str_detect(tolower(var_v), "bien") ~ "Muy bien / Bien",
      str_detect(tolower(var_v), "mal") ~ "Mal / Muy mal", 
      str_detect(tolower(var_v), "regular") ~ "Regular", 
      T ~ "Ns/Nc"
    )
  ) %>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden = case_when(
      str_detect(tolower(var_v), "bien") ~ 1,
      str_detect(tolower(var_v), "regular") ~ 2,
      str_detect(tolower(var_v), "mal") ~ 3,
      T ~ 4
    )
  )
fiuf <- "¿Cómo calificaría la forma en que el Gobierno de Andrés Manuel López Obrador está combatiendo la corrupción?"
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[5],
                                   mcci_discrete[1],
                                   mcci_discrete[2])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 4.1 (comparado con reforma) ----
i = 4
a <- 
  filter(
    readxl::read_excel("MCCI/encuesta_2020/01_datos/datos_gráficas.xlsx") %>% 
      mutate(props = as.numeric(props), orden = as.numeric(orden)), id=="T1_3"
  ) %>% 
  mutate(
    id = paste0("Encuesta nacional en vivienda\nMCCI-Reforma | marzo ", año),
    fecha = case_when(
      año == 2018 ~ as.Date("2018-04-01"),
      año == 2019 ~ as.Date("2019-03-01"),
      año == 2020 ~ as.Date("2020-03-01")
    )
  ) %>% 
  select(var_v = variables, prop = props, orden, id, fecha) %>% 
  bind_rows(
    prop_simples %>% 
      filter(v_id == v_names_loop_gg[i]) %>% 
      mutate(
        var_v = case_when(
          str_detect(tolower(var_v), "bien") ~ "Muy bien / Bien",
          str_detect(tolower(var_v), "mal") ~ "Mal / Muy mal", 
          str_detect(tolower(var_v), "regular") ~ "Regular", 
          T ~ "Ns/Nc"
        )
      ) %>% 
      group_by(var_v, v_id) %>% 
      summarise_all(funs(sum(.,na.rm=T))) %>% 
      mutate(
        prop = round(prop),
        orden = case_when(
          str_detect(tolower(var_v), "bien") ~ 1,
          str_detect(tolower(var_v), "regular") ~ 2,
          str_detect(tolower(var_v), "mal") ~ 3,
          T ~ 4
        ),
        id = "Encuesta nacional telefónica\nMCCI-Data OPM | julio 2020",
        fecha = as.Date("2020-07-01")
      ) %>% 
      select(var_v, prop, orden, id, fecha)
  )


ggplot(a, aes(col = reorder(var_v, as.numeric(orden)),
              y = prop,
              label = paste0(prop, "%"),
              x = fecha)) +
  geom_line(show.legend = T, size = 2) +  geom_point(size = 4) + 
  ggrepel::geom_label_repel(vjust = -0.3, size = 8, show.legend = F, col = mcci_discrete[1]) + 
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_color_manual("", values = c(mcci_discrete[1],
                                    mcci_discrete[2], 
                                    mcci_discrete[5], 
                                    mcci_discrete[3])) +
  scale_x_date(
    breaks = c(
      as.Date("2018-04-01"),
      as.Date("2019-03-01"),
      as.Date("2020-03-01"),
      as.Date("2020-07-01")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title= str_wrap("¿Cómo calificaría la forma en que el presidente en turno está combatiendo la corrupción?",80),
       subtitle = "Enrique Peña Nieto (2018) y Andrés Manuel López Obrador (2019-2020)",
       caption = "Fuente: Encuestas MCCI-Reforma y MCCI-Data OPM\nNota: la última medición no es comparable debido al diseño muestral.") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 25),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "right")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], "_comparada.png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 5 ----
i = 5
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      var_v == "Acceso a servicios públicos como agua, luz, drenaje" ~ "Acceso a servicios públicos como agua, luz, drenaje",
      var_v == "Acceso a servicios de salud" ~ "Acceso a servicios de salud",
      var_v == "Programas sociales" ~ "Programas sociales",
      var_v == "Acceso a apoyos económicos del gobierno" ~ "Acceso a apoyos económicos del gobierno",
      var_v == "Verificaciones de salubridad o protección civil" ~ "Otro",
      var_v == "Registro civil" ~ "Otro",
      var_v == "Policía o servicios de seguridad pública" ~ "Policía o servicios de seguridad pública",
      var_v == "Otro" ~ "Otro",
      var_v == "Ninguno" ~ "Ninguno",
      var_v == "NS/NC" ~ "NS/NC"
    )
  )%>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden = case_when(
      var_v == "Acceso a apoyos económicos del gobierno" ~ 3,
      var_v == "Acceso a servicios de salud" ~ 1,
      var_v == "Acceso a servicios públicos como agua, luz, drenaje" ~ 4,
      var_v == "Ninguno" ~ 7,
      var_v == "NS/NC" ~ 8,
      var_v == "Otro" ~ 6,
      var_v == "Policía o servicios de seguridad pública" ~ 2,
      var_v == "Programas sociales" ~ 5,
    )
  )

fiuf <- vector_fiuf[i]
ggplot(a, 
       aes(x = reorder(str_wrap(var_v,15), orden), 
           y = prop, 
           label = paste0(prop, "%"),
           fill = prop)) + 
  geom_bar(stat = "identity", width = .3) +
  geom_text(size = 7, vjust = -0.2) +
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(10,30,10),
    labels = paste0(
      as.character(seq(10,30,10)), "%"
    )
  ) + 
  scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 6 ----
i = 7
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i])
fiuf <- vector_fiuf[i]

c <- round(a$prop)
names(c) <- paste0(a$var_v, " [", round(a$prop, 2), "%]")
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(rep(names(c), c))  


ggplot(df, aes(x = x, y = y, fill = str_wrap(category, 25))) + 
  geom_tile(color = "white", size = 0.25) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual("", values = c(mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 80),
       caption = fiuffi) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 25),
        legend.key.size = unit(3, "lines"),
        legend.position = "bottom")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 6.1 (desagregación por servicios) ----
a_mcci <- data %>% 
  filter(v_mcci6_durante_la_pandemia_le_han_solicitado_un_pago_indebido_propina_o_mordida_por_realizar_algún_servicio_público_gratuito_que_haya_necesitado_servicios_médicos_programas_sociales_programas_de_apoyo_a_la_emergencia_etc == "Sí") %>% 
  select(starts_with("v_a_mcci"), weight)%>% 
  mutate_at(
    vars(starts_with("v_")),
    funs(
      ifelse(str_detect(., "([0-9]+)"),0,1)
    )
  ) %>% 
  rename_at(
    vars(starts_with("v_a_mcci")),
    funs(
      str_remove_all(.,"([0-9]+)")
    )
  )%>% 
  rename_at(
    vars(starts_with("v")),
    funs(
      str_remove_all(.,"a_mcci__para_qué_tipo_de_servicio_")
    )
  ) %>% 
  select(-v_a_mcci_otro_para_qué_tipo_de_servicio_otro_other_specify)

# Necesario para evitar error
i <- nrow(a_mcci)+1
ii <- nrow(a_mcci)+2
a_mcci[i,] <- 99
a_mcci[i,15] <- 1
a_mcci[ii,] <- 98
a_mcci[ii,15] <- 1
rm(i, ii)

test <- data.frame()
for(i in 1:14){
  tempo <- a_mcci %>% 
    mutate_at(
      vars(starts_with("v")),
      funs(as.factor(.))
    ) %>% 
    select(names(a_mcci)[i], weight) %>% 
    as_survey_design(
      weight = weight
    ) %>% 
    srvyr::group_by_at(vars(starts_with("v_")),
                       .drop = T) %>% 
    srvyr::summarise(count = survey_total(na.rm = T), 
                     prop = survey_mean(na.rm = T)) %>% 
    rename(var_v = starts_with("v_")) %>% 
    mutate(
      v_id = str_replace_all(names(a_mcci)[i], "v_", "")
    ) %>% 
    filter(var_v=="1")
  
  test <- bind_rows(test, tempo)
  rm(tempo)
  
}

a <- test %>% 
  mutate(
    prop = round(prop*100,2),
    var_v = str_replace_all(v_id, "_", " "),
    var_v = str_to_sentence(var_v),
    var_v = case_when(
      var_v == "Médico o de atención a la salud incluyendo pruebas de detección o inmunidad por coronavirus" ~ "Atención a la salud (incluyendo pruebas de detección por coronavirus)",
      var_v == "Trámites de construcción permisos de demolición y similares" ~ "Trámites de construcción, permisos de demolición y similares",
      var_v == "Seguridad pública" ~ "Seguridad pública",
      var_v == "Luz y suministro de energía eléctrica" ~ "Luz y suministro de energía eléctrica",
      var_v == "Servicios legales" ~ "Servicios legales",
      var_v == "Otro" ~ "Otro",
      var_v == "Nsnc" ~ "Ns/Nc"
    ),
    orden = case_when(
      var_v == "Atención a la salud (incluyendo pruebas de detección por coronavirus)" ~ 1,
      var_v == "Trámites de construcción, permisos de demolición y similares" ~ 4,
      var_v == "Seguridad pública" ~ 2,
      var_v == "Luz y suministro de energía eléctrica" ~ 5,
      var_v == "Servicios legales" ~ 3,
      var_v == "Otro" ~ 6,
      var_v == "Ns/Nc" ~ 7,
    )
  )


fiuf <- "¿Para qué tipo de servicio?"
ggplot(a, 
       aes(x = reorder(str_wrap(var_v,15), orden), 
           y = prop, 
           label = paste0(prop, "%"),
           fill = prop)) + 
  geom_bar(stat = "identity", width = .3) +
  geom_text(size = 7, vjust = -0.2) +
  scale_y_continuous(
    limits = c(0,42),
    breaks = seq(10,40,10),
    labels = paste0(
      as.character(seq(10,40,10)), "%"
    )
  ) + 
  scale_fill_gradient(high = '#000c2d', low = '#b2b6c0') +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", "mcci7.png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 7 ----
i <-  23
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      str_detect(tolower(var_v), "mejorado") ~ "Mejorado",
      str_detect(tolower(var_v), "igual") ~ "Permaneció igual",
      str_detect(tolower(var_v), "empeorado") ~ "Empeorado",
      T ~ "Ns/Nc"
    )
  ) %>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden = case_when(
      str_detect(tolower(var_v), "mejorado") ~ 1,
      str_detect(tolower(var_v), "igual") ~ 2,
      str_detect(tolower(var_v), "empeorado") ~ 3,
      T ~ 4
    )
  )
fiuf <- "En comparación con el año pasado, ¿diría Ud. que su situación económica personal ha mejorado, ha empeorado, o sigue igual?"
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[2],
                                   mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 8 ----
i <-  24
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      str_detect(tolower(var_v), "mejor") ~ "Mejorará",
      str_detect(tolower(var_v), "igual") ~ "Permanecerá igual",
      str_detect(tolower(var_v), "empeor") ~ "Empeorará",
      T ~ "Ns/Nc"
    )
  ) %>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden = case_when(
      str_detect(tolower(var_v), "mejor") ~ 1,
      str_detect(tolower(var_v), "igual") ~ 2,
      str_detect(tolower(var_v), "empeor") ~ 3,
      T ~ 4
    )
  )
fiuf <- "Ahora, pensando en el futuro, ¿diría Ud. que de aquí a un año su situación económica personal va a ser mejor, peor, o va a seguir igual?"
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[2],
                                   mcci_discrete[1],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 9 ----
i <-  25
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      str_starts(tolower(var_v), "aprueba") ~ "Aprueba",
      str_starts(tolower(var_v), "ni") ~ "Ni aprueba ni desaprueba",
      str_starts(tolower(var_v), "desaprueba") ~ "Desaprueba",
      T ~ "Ns/Nc"
    )
  ) %>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden = case_when(
      str_starts(tolower(var_v), "aprueba") ~ 1,
      str_starts(tolower(var_v), "ni") ~ 2,
      str_starts(tolower(var_v), "desaprueba") ~ 3,
      T ~ 4
    )
  )
fiuf <- "¿Usted aprueba o desaprueba el trabajo de Andrés Manuel López Obrador como presidente de México?"
ggplot(a, 
       aes(y = reorder(str_wrap(var_v,10), -orden), 
           x = prop, 
           label = paste0(prop, "%"),
           fill = reorder(str_wrap(var_v,10), -orden))) + 
  geom_col(width = 0.5) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[5],
                                   mcci_discrete[2],
                                   mcci_discrete[1])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")


# ** Gráfica 9.1 (comparado con reforma) ----
i = 25
a <- 
  filter(
    readxl::read_excel("MCCI/encuesta_2020/03_gráficas/03_serie/00_simples/00_props_serie.xlsx") %>% 
      mutate(prop = as.numeric(prop)), 
    v_id=="3_en_general_usted_aprueba_o_desaprueba_la_forma_como_andres_manuel_lopez_obrador_esta_haciendo_su_trabajo_como_presidente_de_la_republica"
  ) %>% 
  mutate(
    var_v = case_when(
      str_starts(tolower(var_v), "aprueba") ~ "Aprueba",
      str_starts(tolower(var_v), "ni") ~ "Ni aprueba ni desaprueba",
      str_starts(tolower(var_v), "desaprueba") ~ "Desaprueba",
      T ~ "Ns/Nc"
    )
  ) %>% 
  select(var_v, año, starts_with("prop")) %>% 
  group_by(var_v, año) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    prop = round(prop*100, 2),
    id = paste0("Encuesta nacional en vivienda\nMCCI-Reforma | marzo ", año),
    fecha = case_when(
      año == 2019 ~ as.Date("2019-03-01"),
      año == 2020 ~ as.Date("2020-03-01")
    ),
    orden = case_when(
      str_starts(tolower(var_v), "aprueba") ~ 1,
      str_starts(tolower(var_v), "ni") ~ 2,
      str_starts(tolower(var_v), "desaprueba") ~ 3,
      T ~ 4
    )
  ) %>% 
  select(var_v, prop, orden, id, fecha) %>% 
  bind_rows(
    prop_simples %>% 
      filter(v_id == v_names_loop_gg[i]) %>% 
      mutate(
        var_v = case_when(
          str_starts(tolower(var_v), "aprueba") ~ "Aprueba",
          str_starts(tolower(var_v), "ni") ~ "Ni aprueba ni desaprueba",
          str_starts(tolower(var_v), "desaprueba") ~ "Desaprueba",
          T ~ "Ns/Nc"
        )
      ) %>% 
      group_by(var_v, v_id) %>% 
      summarise_all(funs(sum(.,na.rm=T))) %>% 
      mutate(
        orden = case_when(
          str_starts(tolower(var_v), "aprueba") ~ 1,
          str_starts(tolower(var_v), "ni") ~ 2,
          str_starts(tolower(var_v), "desaprueba") ~ 3,
          T ~ 4
        ),
        id = "Encuesta nacional telefónica\nMCCI-Data OPM | julio 2020",
        fecha = as.Date("2020-07-01")
      ) %>% 
      select(var_v, prop, orden, id, fecha)
  )

fiuf <- vector_fiuf[i]

ggplot(a %>% filter(!str_detect(var_v, "Ni")), 
       aes(y = reorder(var_v, -as.numeric(orden)),
           x = prop,
           label = paste0(prop, "%"),
           fill = as.factor(id))) +
  geom_col(width = 0.5, position = position_dodge2()) + 
  geom_text(hjust = -0.1, size = 8, position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[1], 
                                   mcci_discrete[2], 
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 70),
       caption = "Fuente: Encuestas MCCI-Reforma y MCCI-Data OPM\nNota: la última medición no es comparable debido al diseño muestral.\nNo se toma en cuenta la categoría 'Ni aprueba ni desaprueba' de la última medición.") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 34, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.y = unit(1.0, 'cm'),
        legend.position = "right")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], "_comparada.png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 10 ----
i <-  26
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  dplyr::mutate(
    var_v = case_when(
      var_v == "0 Políticamente de izquierda" ~ "Izquierda",
      var_v == "1" ~ "Izquierda",
      var_v == "2" ~ "Izquierda",
      var_v == "3" ~ "Izquierda",
      var_v == "4" ~ "Izquierda",
      var_v == "5" ~ "Centro",
      var_v == "6" ~ "Derecha",
      var_v == "7" ~ "Derecha",
      var_v == "8" ~ "Derecha",
      var_v == "9" ~ "Derecha",
      var_v == "10 Políticamente de derecha" ~ "Derecha",
      T ~ "No se identifica\nNs/Nc"
    )
  ) %>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden =case_when(
      var_v == "Izquierda" ~ 1,
      var_v == "Centro" ~ 2,
      var_v == "Derecha" ~ 3,
      T ~ 4
    )
  )

fiuf <- vector_fiuf[i]
ggplot(a,
       aes(fill = reorder(var_v, -as.numeric(orden)),
           x = prop,
           label = paste0(prop, "%\n", var_v),
           y = v_id)) +
  geom_col(width = 0.8, position = position_stack()) + 
  geom_text(hjust = 0.5, size = 9, position = position_stack(vjust = .5), col = "#ffffff", fontface = "bold") +
  scale_x_continuous(
    limits = c(0,100),
    breaks = seq(0,100,25),
    labels = paste0(
      as.character(seq(0,100,25)), "%"
    )
  ) + 
  scale_fill_manual("", values = c(mcci_discrete[3],
                                   mcci_discrete[1],
                                   mcci_discrete[2],
                                   mcci_discrete[5])) +
  labs(title= str_wrap(fiuf, width = 75),
       caption = fiuffi,
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        panel.spacing.y = unit(1.0, 'cm'),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")

# ** Gráfica 11 ----
i <-  27
a <- prop_simples %>% 
  filter(v_id == v_names_loop_gg[i]) %>% 
  mutate(
    var_v = case_when(
      str_detect(tolower(var_v), "morena") ~ "MORENA",
      str_detect(tolower(var_v), "pri") ~ "PRI",
      str_detect(tolower(var_v), "pan") ~ "PAN",
      str_detect(tolower(var_v), "mc") ~ "Movimiento Ciudadano",
      T ~ var_v
    )
  )%>% 
  group_by(var_v, v_id) %>% 
  summarise_all(funs(sum(.,na.rm=T))) %>% 
  mutate(
    orden =case_when(
      str_detect(tolower(var_v), "morena") ~ 4,
      str_detect(tolower(var_v), "pri") ~ 2,
      str_detect(tolower(var_v), "pan") ~ 1,
      str_detect(tolower(var_v), "mov") ~ 3,
      str_detect(tolower(var_v), "otro") ~ 5,
      str_detect(tolower(var_v), "ning") ~ 6,
      T ~ 7
    )
  )

fiuf <- "Sin importar por quién ha votado en el pasado, ¿con cuál partido político simpatiza más?"
ggplot(a, aes(y = reorder(str_wrap(var_v, 25), -orden),
              x = prop,
              label = paste0(prop, "%"),
              fill = reorder(str_wrap(var_v, 25), -orden))) +
  geom_col(width = 0.8) + 
  geom_text(hjust = -0.1, size = 8) +
  scale_x_continuous(
    limits = c(0,40),
    breaks = seq(0,40,10),
    labels = paste0(
      as.character(seq(0,40,10)), "%"
    )
  ) + 
  scale_fill_manual("", values = rev(c(
    "#0079bc", # PAN
    "#e93d44", # PRI
    "#fd8204", # MC
    "#b85756", # MORENA
    "#8a8c91", # NS / NC
    "#606265", # LEYENDAS
    "#b8babd"
  ))) +
  labs(title= str_wrap(fiuf, width = 55),
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.caption = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 21),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "none")
ggsave(filename = paste0(
  out, "00_simples/", v_num[i], ".png"
), width = 17, height = 12, dpi = 100, bg = "transparent")
