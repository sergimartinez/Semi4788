# =============================================================================
# Seminario de investigación
# Muestreo e inferencia (descriptiva): dplyr, TLC, p-valor, t-test, Chi2,
# covarianza, correlación y regresión
#
# Script generado a partir de los chunks de R de L6_quant_II.qmd
# =============================================================================


# =============================================================================
# LIBRERÍAS
# =============================================================================

library(tidyverse)     # gramática de manipulación y visualización de datos (dplyr, ggplot2, etc.)
library(fontawesome)   # iconos para los materiales de clase (no usado en el análisis)
library(crayon)        # texto coloreado en consola (no usado en el análisis)
library(knitr)         # motor de generación de los documentos/slides
library(ggthemes)      # temas adicionales para ggplot2
library(kableExtra)    # tablas HTML/LaTeX con formato enriquecido
library(directlabels)  # etiquetas directas sobre curvas en gráficos
library(ggtext)        # texto enriquecido (markdown/HTML) dentro de ggplot2
library(plotly)        # gráficos interactivos
library(showtext)      # uso de fuentes personalizadas en gráficos
library(countrycode)   # conversión de nombres/códigos de país
library(readxl)        # lectura de archivos Excel
library(gt)             # tablas con formato
library(gtExtras)      # extensiones de formato para gt
library(countdown)     # temporizadores para las slides
library(here)          # rutas de archivo relativas al proyecto
library(haven)          # lectura de archivos .dta (Stata) y manejo de etiquetas
library(mosaic)        # funciones didácticas de estadística (xpt, etc.)
library(olsrr)         # diagnósticos de modelos de regresión OLS
library(janitor)       # limpieza de nombres de variables (clean_names)
library(doBy)          # utilidades de agregación por grupos
library(broom)         # convierte resultados de modelos/tests en data frames ordenados (tidy)
library(gapminder)     # dataset de ejemplo GDP/población por país y año

# hook para recortar automáticamente los PDFs generados (usado al renderizar las slides)
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)


# =============================================================================
# SECCIÓN 1: DPLYR
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 (Mítico) ejemplo usando gapminder
# -----------------------------------------------------------------------------

# cargamos el dataset de ejemplo gapminder (PIB y población en los últimos 50 años)
gapminder_df <- gapminder::gapminder_unfiltered

# inspeccionamos las primeras filas del dataset
head(gapminder_df)


# -----------------------------------------------------------------------------
# 1.2 Filtering: filtrar observaciones según condiciones
# -----------------------------------------------------------------------------

# filtramos por varias condiciones a la vez: rango de años, PIB per cápita mínimo y continente
gapminder_df %>%
  filter(year %in% c(1970:1974) &
           gdpPercap >= 10000 &
           continent == "Asia") %>%
  head(6) # mostramos solo las primeras 6 filas del resultado


# -----------------------------------------------------------------------------
# 1.3 Mutate: crear nuevas variables
# -----------------------------------------------------------------------------

## ¿Qué sucede en Europa entre 1993 y 2003?
## ¿Tuvo algún efecto en el PIB o la esperanza de vida?

gapminder_df %>%
  mutate(gdp = pop * gdpPercap, # calculamos el PIB total (población x PIB per cápita)
         dog_life_exp = lifeExp / 7) %>% # aquí multiplicamos la esperanza de vida de cada país equivalente a los perros, y el PIB.
  filter(country == "Austria" & year > 1993 & year < 2003) # nos quedamos solo con Austria en ese rango de años


# -----------------------------------------------------------------------------
# 1.4 Summarize y Group_By: resumir datos por grupos
# -----------------------------------------------------------------------------

# ¿Cómo podemos ver medias por grupo de observaciones?
gapminder_df %>%
  summarise(pop_mean = mean(pop)) # calculamos la media de población para todo el dataset (colapsa a una fila)

# por defecto, `summarize' colapsa la base de datos en una línea, pero si utilizamos `group_by' podemos obtener resultados más informativos como, en nuestro caso, el contniente o el país

## ¿Cuántos millones de personas, de media, tiene cada país en cada continente?

gapminder_df %>%
  group_by(continent) %>% # agrupamos por continente
  summarise(pop_mean = mean(pop / 1000000)) # media de población (en millones) por continente

# ¿Qué es FSU? ¿Florida State University?

gapminder_df %>%
  filter(continent == "FSU") %>% # filtramos las observaciones del continente "FSU"
  distinct(country) # obtenemos los países únicos de ese grupo

# también podemos obtener información de diversos puntos temporales

gapminder_df %>%
  group_by(continent) %>% # agrupamos por continente
  summarise(pop_mean = mean(pop / 1000000), # media de población en millones
            gdpPercap_mean = mean(gdpPercap)) # media de PIB per cápita


# -----------------------------------------------------------------------------
# 1.5 (Des)selecting variables: seleccionar o quitar columnas
# -----------------------------------------------------------------------------

# queremos ver sólo tres columnas/variables? Select():

gapminder_df %>%
  select(country, year, lifeExp) %>% # nos quedamos solo con estas tres columnas
  head(3)

# podemos realizar el mismo procedimiento de-seleccionando otras variables con el select(-)

gapminder_df %>%
  select(-c(continent, pop)) %>% # eliminamos las columnas continent y pop, nos quedamos con el resto
  head(3)


# -----------------------------------------------------------------------------
# 1.6 Renombrar (rename) y limpiar nombres de variables
# -----------------------------------------------------------------------------

# new_name = old_name
gapminder_df %>%
  rename(population = pop, # renombramos pop -> population
         life_expectancy = lifeExp) %>% # renombramos lifeExp -> life_expectancy
  head()

#install.packages("janitor")
# library(janitor) # ya cargada en la sección de librerías

# janitor package cambia automáticamente las variables a minusculas y formato xxx_xx
gapminder_df %>%
  janitor::clean_names() %>% # estandariza los nombres de columnas a snake_case
  head(2)


# -----------------------------------------------------------------------------
# 1.7 Joining (merge) & Binding (agregar filas)
# -----------------------------------------------------------------------------

# imagine que tenemos 2 datasets:

gapminder_df_pop <- gapminder_df %>%
  select(country, year, pop) # dataset solo con población

gapminder_df_life <- gapminder_df %>%
  select(country, year, lifeExp) # dataset solo con esperanza de vida

# cómo las juntamos? on _join, detallando las variables por las que esta unión podría ordenarse:

gapminder_df_pop %>%
  inner_join(gapminder_df_life, by = c("country", "year")) %>% # unimos ambos datasets por país y año
  head(5)

# There are different types of _join.

# ahora imaginen que tenemos dos datasets con las mismas columnas (y distintas filas). Por ejemplo:

gapminder_df_old <- gapminder_df %>%
  filter(year < 1965) # observaciones anteriores a 1965

gapminder_df_new <- gapminder_df %>%
  filter(year >= 1965) # observaciones de 1965 en adelante

gapminder_df_old %>%
  rbind(gapminder_df_new) %>% # apilamos (bind) ambos datasets por filas
  head()


# -----------------------------------------------------------------------------
# 1.8 Merging interesting data: ejemplo aplicado con datos de Medellín
# -----------------------------------------------------------------------------

# 1. Datos de nivel socioeconómico (SES) por comuna en Medellín

# library(doBy) # ya cargada en la sección de librerías

# cargamos el csv con la cantidad de hogares por barrio y estrato socioeconómico
hogares_barrio_estrato <- read.csv("cantidad_hogares_barrio_estrato_sisben_lll.csv")

hogares_barrio_estrato_2019 <- hogares_barrio_estrato %>%
  filter(Annio == 2019) %>% # nos quedamos solo con el año 2019
  mutate(total_hogares = Estrato_0 + Estrato_1 + Estrato_2 + Estrato_3 + Estrato_4 + Estrato_5 + Estrato_6) %>% # calculamos el total de hogares sumando todos los estratos
  collapse::collap(total_hogares + Estrato_0 + Estrato_1 + Estrato_2 + Estrato_3 + Estrato_4 + Estrato_5 + Estrato_6 ~ Comuna + Codigo_comuna, FUN = "fsum") # agregamos (sumamos) por comuna y código de comuna

# calculamos el ratio de hogares en los estratos más bajos (0,1,2) sobre el total de hogares
hogares_barrio_estrato_2019$ratio_estrato2 <- (hogares_barrio_estrato_2019$Estrato_0 + hogares_barrio_estrato_2019$Estrato_1 + hogares_barrio_estrato_2019$Estrato_2) / hogares_barrio_estrato_2019$total_hogares

hogares_barrio_estrato_2019 <- hogares_barrio_estrato_2019 %>%
  mutate(ratio_estrato2_v2 = (Estrato_0 + Estrato_1 + Estrato_2) / total_hogares, # recalculamos el mismo ratio dentro de un mutate
         Codigo_comuna = as.numeric(Codigo_comuna)) # convertimos el código de comuna a numérico para poder unir datasets más adelante

# graficamos el total de hogares por comuna, coloreado según el ratio de estratos bajos
hogares_barrio_estrato_2019 %>%
  ggplot() +
  geom_bar(aes(x = Codigo_comuna, y = total_hogares, fill = ratio_estrato2), stat = "identity") + # barras de total de hogares por comuna
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # rotamos las etiquetas del eje x para que se lean mejor


# 2. Número de Juntas de Acción Comunal

# cargamos el csv con las fichas diagnósticas de las juntas de acción comunal
jac_fichas <- read.csv("fichas_diagnosticas_juntas_de_accion_comunal.csv")

# convertimos la columna de dignatarios activos a numérico
jac_fichas$dignatarios <- as.numeric(jac_fichas$DIGNATARIOS.ACTIVOS)

jac_fichas_collapsed <- jac_fichas %>%
  collapse::collap(dignatarios ~ COMUNA, FUN = "fsum") %>% # sumamos los dignatarios activos por comuna
  rename(Codigo_comuna = COMUNA) # renombramos la columna para que coincida con el otro dataset

# unimos los datos de juntas de acción comunal con los datos de estratos socioeconómicos por comuna
data1 <- jac_fichas_collapsed %>%
  inner_join(hogares_barrio_estrato_2019, by = "Codigo_comuna")


# 3. ¿Hay una correlación?

# graficamos la relación entre el ratio de estratos bajos y el número de dignatarios (juntas de acción comunal)
data1 %>%
  ggplot(aes(x = ratio_estrato2_v2, y = dignatarios)) +
  geom_jitter() + # puntos con un poco de ruido aleatorio para evitar solapamientos
  geom_smooth(method = "lm", se = FALSE) + # línea de tendencia lineal (sin banda de error)
  theme_bw()


# =============================================================================
# SECCIÓN 2: STATS
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1 El teorema del límite central (TLC) y el p-valor
# -----------------------------------------------------------------------------

# ---- ¿Qué es una distribución normal? ----

set.seed(1) # fijamos la semilla aleatoria para que el resultado sea reproducible
norm <- rnorm(1000, mean = 0, sd = 1) # generamos 1000 observaciones de una distribución normal estándar

data.frame(norm) %>%
  summary(norm) # resumen estadístico (mínimo, cuartiles, media, máximo) de la variable generada

# marcamos las observaciones que caen fuera del intervalo [-1.95, 1.95] (aprox. el 95% central)
data.frame(norm) %>%
  mutate(margin95 = ifelse((norm < (-1.95)) | (norm > (1.95)), 1, 0)) %>% # 1 si está fuera del margen del 95%, 0 si está dentro
  ggplot(aes(x = norm, color = as.factor(margin95))) +
  geom_histogram(position = "identity") # histograma coloreado según si la observación está dentro o fuera del margen


# ---- Un sondeo es una sola realización: estimación del voto al PP con IC ----

library(tidyverse) # (ya cargada arriba, se repite tal como aparece en el chunk original)
library(haven)      # (ya cargada arriba, se repite tal como aparece en el chunk original)

# cargamos los microdatos del barómetro de 40dB de octubre de 2026
datos <- read_dta("03_Datos_octubre_2026.dta", encoding = "latin1")

# Códigos 1 a 18 de p3 son partidos. Se excluyen blanco, nulo,
# no votaría, no sabe y no contesta (códigos 19 a 23)
limpio <- datos |>
  filter(p3 %in% 1:18) |> # nos quedamos solo con quienes votarían a algún partido
  transmute(pp = as.integer(p3 == 2), ponde) # creamos una variable binaria (1 = votaría al PP) y mantenemos la ponderación

n     <- nrow(limpio) # número de observaciones (tamaño de muestra nominal)
n_ef  <- sum(limpio$ponde)^2 / sum(limpio$ponde^2)   # tamaño efectivo (ajustado por la ponderación)
p_hat <- weighted.mean(limpio$pp, limpio$ponde) # proporción estimada de voto al PP, ponderada
se    <- sqrt(p_hat * (1 - p_hat) / n_ef) # error estándar de la proporción estimada

# mostramos el tamaño de muestra, el tamaño efectivo, la estimación y su intervalo de confianza al 95%
tibble(n, n_ef, p_hat, se,
       inferior = p_hat - 1.96 * se,
       superior = p_hat + 1.96 * se)


# ---- Diez mil sondeos posibles, una sola campana: simulación del TLC ----

set.seed(2026) # fijamos la semilla para reproducibilidad
B <- 10000 # número de sondeos simulados

# simulamos B sondeos extrayendo una proporción binomial con el tamaño y probabilidad estimados
sondeos <- tibble(p_sim = rbinom(B, size = round(n_ef), prob = p_hat) / round(n_ef))

# graficamos la distribución de las B estimaciones simuladas junto a la normal teórica
ggplot(sondeos, aes(p_sim)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, # histograma de densidad de las estimaciones simuladas
                 fill = "grey75", colour = "white") +
  stat_function(fun = dnorm, args = list(mean = p_hat, sd = se), # curva normal teórica superpuesta
                colour = "firebrick", linewidth = 1) +
  geom_vline(xintercept = p_hat + c(-1.96, 1.96) * se, linetype = "dashed") + # líneas verticales marcando el intervalo del 95%
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Estimación de voto al PP en cada sondeo simulado", y = "Densidad") +
  theme_minimal(base_size = 14)

# comparamos la media y SD simuladas con el error estándar teórico calculado antes
c(media_sim = mean(sondeos$p_sim), sd_sim = sd(sondeos$p_sim), se_teorico = se)


# -----------------------------------------------------------------------------
# 2.2 T-test y Chi2: relaciones entre dos variables
# -----------------------------------------------------------------------------

# ---- Chi2: cálculo e interpretación (ejemplo gender gap Trump-Clinton) ----

# calculamos el estadístico Chi2 a mano: suma de ((observado-esperado)^2 / esperado) para cada celda
(((532 - 586.56)^2) / 586.56) + (((736 - 682.24)^2) / 682.24) +
  (((596 - 541.44)^2) / (541.44)) + (((575 - 629.76)^2) / 629.76)


# ---- T-test: ejemplo con tomates (cálculo manual del estadístico T) ----

# calculamos el estadístico t a mano: diferencia de medias dividida por el error estándar combinado
(1.3 - 1.6) / sqrt(((0.5^2) / 22) + ((0.3^2) / 24))

# buscamos la probabilidad asociada (p-valor) a ese valor t en la distribución t con 21 grados de libertad
xpt <- xpt(q = 2.44, df = 21, xlab = "t's")


# ---- T-test: ejemplo con los datos de 40dB (ERC/Bildu vs Sumar/Podemos) ----

library(broom) # (ya cargada arriba, se repite tal como aparece en el chunk original)

izq <- datos |>
  zap_labels() |> # quitamos las etiquetas de valor de Stata para trabajar con los códigos numéricos
  filter(p3 %in% c(4, 5, 7, 10)) |> # nos quedamos solo con quienes votarían a estos 4 partidos
  mutate(
    bloque = if_else(p3 %in% c(7, 10), "ERC y Bildu", "Sumar y Podemos"), # creamos la variable de bloque político
    # 5 es "No lo sé". Invertimos la escala para que más alto sea más acuerdo (1 a 4)
    across(starts_with("p22_"), ~ if_else(.x == 5, NA_real_, 5 - .x)) # recodificamos todos los ítems p22_ invirtiendo la escala y marcando NA los "no sabe"
  )

# Un ítem concreto (p22_3, ventajas fiscales a pequeños propietarios que alquilan a precios asequibles)
t.test(p22_3 ~ bloque, data = izq) # comparamos la media de acuerdo con este ítem entre los dos bloques

# Los 12 ítems a la vez, con corrección de Bonferroni por comparaciones múltiples
resultados <- izq |>
  pivot_longer(starts_with("p22_"), names_to = "item", values_to = "acuerdo") |> # pasamos de formato ancho a largo, un ítem por fila
  group_by(item) |> # agrupamos por cada ítem de política de vivienda
  group_modify(~ tidy(t.test(acuerdo ~ bloque, data = .x))) |> # aplicamos un t-test por cada ítem y convertimos el resultado en data frame ordenado
  ungroup() |>
  mutate(p_bonferroni = p.adjust(p.value, method = "bonferroni")) # corregimos los p-valores por comparaciones múltiples

# mostramos el resumen de resultados por ítem
resultados |> select(item, estimate, statistic, p.value, p_bonferroni)

# graficamos las diferencias de medias por ítem con su intervalo de confianza
ggplot(resultados, aes(x = estimate, y = reorder(item, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + # punto estimado y su intervalo de confianza
  geom_vline(xintercept = 0, linetype = "dashed") + # línea de referencia en el valor cero (sin diferencia)
  labs(x = "Diferencia de medias (ERC y Bildu menos Sumar y Podemos)", y = NULL) +
  theme_minimal(base_size = 14)

# De los 12 ítems solo uno separa a los dos bloques. En las ventajas fiscales a pequeños
# propietarios que alquilan a precios asequibles, ERC y Bildu muestran más acuerdo
# (3,57 frente a 3,16 en una escala de 1 a 4). La diferencia es de 0,41 puntos, con un
# intervalo del 95% de 0,19 a 0,63 [t = 3,61, p < 0,001]. Sigue siendo significativa tras
# Bonferroni. En los otros once ítems no hay diferencias significativas.


# -----------------------------------------------------------------------------
# 2.3 Covarianza y correlación
# -----------------------------------------------------------------------------

# ---- Covarianza entre ideología y preocupación por la IA ----

ia_ideologia <- datos |>
  zap_labels() |> # quitamos las etiquetas de valor de Stata
  select(ideologia = p7, ia = p8_8) |> # seleccionamos y renombramos las dos variables de interés
  mutate(
    ideologia = if_else(ideologia == 99, NA_real_, ideologia),  # 99 es No lo sé, lo recodificamos como NA
    # 5 es No lo sé. Invertimos la escala para que más alto sea más preocupación (1 a 4)
    ia = if_else(ia == 5, NA_real_, 5 - ia) # recodificamos "no sabe" como NA e invertimos la escala
  ) |>
  drop_na() # eliminamos las filas con valores perdidos en cualquiera de las dos variables

nrow(ia_ideologia) # número de observaciones válidas tras limpiar los datos
cov(as.numeric(ia_ideologia$ideologia), as.numeric(ia_ideologia$ia)) # calculamos la covarianza entre ambas variables

# La covarianza es negativa (alrededor de -0,19 con 1.818 casos). Quienes se sitúan más a
# la derecha tienden a mostrar algo menos de preocupación por la IA. Su magnitud depende
# de las unidades de ambas escalas, así que por sí sola es difícil de interpretar y de
# comparar con otras relaciones.


# ---- De la covarianza a la correlación ----

ia_ideologia <- datos |>
  zap_labels() |> # quitamos las etiquetas de valor de Stata
  select(ideologia = p7, ia = p8_8) |> # seleccionamos y renombramos las variables
  mutate(
    ideologia = if_else(ideologia == 99, NA_real_, ideologia),  # 99 es No lo sé
    # 5 es No lo sé. Invertimos la escala para que más alto sea más preocupación (1 a 4)
    ia = if_else(ia == 5, NA_real_, 5 - ia)
  ) |>
  drop_na() # eliminamos filas con NA

# test de correlación de Pearson entre ideología y preocupación por la IA (coeficiente, IC y p-valor)
stats::cor.test(as.numeric(ia_ideologia$ideologia), as.numeric(ia_ideologia$ia))


# -----------------------------------------------------------------------------
# 2.4 Regresión
# -----------------------------------------------------------------------------

# ---- Propiedades de la regresión: mismo signo en covarianza, correlación y beta ----

cov(ia_ideologia$ideologia, ia_ideologia$ia) # covarianza entre ambas variables

cor(ia_ideologia$ideologia, ia_ideologia$ia) # correlación entre ambas variables

summary(lm(ia_ideologia$ideologia ~ ia_ideologia$ia)) # modelo de regresión lineal simple y su resumen


# ---- Interpretando y prediciendo: ajuste del modelo de regresión ----

# ajustamos el modelo de regresión lineal de preocupación por la IA en función de la ideología
modelo_ia_ideo <- lm(ia ~ ideologia, data = ia_ideologia)
summary(modelo_ia_ideo) # resumen del modelo: coeficientes, errores estándar, p-valores y R²


# ---- Asunciones de la regresión: diagnósticos del modelo ----

# comprobamos la homocedasticidad (varianza constante de los residuos) con un gráfico de residuos frente a valores ajustados
olsrr::ols_plot_resid_fit(modelo_ia_ideo)

# comprobamos la normalidad de los residuos con un histograma
olsrr::ols_plot_resid_hist(modelo_ia_ideo)
