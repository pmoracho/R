library("tidyverse")
library("ggrepel")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
  base_familiy = "Raleway"
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
  base_familiy = ""
}

dias_proyectados <- 10
###############################################################
# Obtenemos las cantidades actualizadas de infectados por fecha
# Los datos arrancan del 5/3 la función de D. Penazzi del 3/3
###############################################################
url <- "https://raw.githubusercontent.com/SistemasMapache/Covid19arData/master/CSV/Covid19arData%20-%20historico.csv"
file <- file.path(".", basename(url))
download.file(url, file)
covid <- read.csv(file, stringsAsFactors = FALSE, dec = ",")
covid$tot_casosconf <- as.integer(sub("\\.","", covid$tot_casosconf) )

###############################################################
# Agrupamos por día y obtenemos el valor máximo, ya que en 
# estos datos acumulan por provincia
###############################################################
covid %>% 
  select(dia_inicio, tot_casosconf) %>%
  # Si tenemos los datos del día antes, podemos agregarlos 
  #union(data.frame(dia_inicio = max(covid$dia_inicio) + 1,tot_casosconf = 1133)) %>% 
  group_by(dia_inicio) %>% 
  summarize(tot_casosconf = max(tot_casosconf)) ->  df

# df %>%  View()
proximo_dia <- max(df$dia_inicio) + 1
###############################################################
# Generamos el Df final con todos los datos para el gráfico
###############################################################
df %>% 
  mutate(casos_modelo = NA_integer_) %>% 
  union(data_frame( dia_inicio = proximo_dia:(proximo_dia + dias_proyectados),
                    tot_casosconf = NA_integer_,
                    casos_modelo = NA_integer_)
  ) %>% 
  mutate(casos_modelo = round(3.3327 * 1.2405^ (dia_inicio + 2)),
         consolidado = ifelse(is.na(tot_casosconf), casos_modelo, tot_casosconf), 
         label = ifelse(!is.na(tot_casosconf), paste0(casos_modelo ,"/",tot_casosconf), paste(casos_modelo)), 
         dia = 4 + dia_inicio
         ) -> df


labels_x <- function (x, n = 15) 
{
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n)
    names(breaks) <- format(as.Date("2020-03-04") + breaks, "%d/%m")
    breaks
  }
}


###############################################################
# El Plot final
###############################################################
zoom <- list(x=NULL, y=NULL)
# Para ajustar el gráfico a un área en paticular
zoom <- list(x=c(15, 30), y=c(150,2500))
beaks <- ifelse(!is.null(zoom$x), diff(zoom$x), 30)

df %>% 
  ggplot(mapping=aes(x = dia_inicio, label = label)) +
  geom_point(mapping = aes(y = tot_casosconf), color = "blue", size = 2) +
  geom_point(mapping = aes(y = casos_modelo), color = "red", alpha = .2, size = 3) +
  stat_function(fun = function(x) 3.3327 * 1.2405^ (x + 2), color = "red", alpha = .3) +
  geom_smooth(mapping = aes(y = tot_casosconf), method = 'loess', 
              color = "blue", alpha=.1, size = .5, se=FALSE) +
  scale_x_continuous(breaks = labels_x(n = beaks),
                     limits=zoom$x) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15),
                     limits=zoom$y) +
  geom_label_repel(mapping = aes(y = consolidado),
                   nudge_y = 0.15,
                   angle = 90,
                   vjust = 5,
                   segment.size = 0.2,
                   size = 4,
                   family = base_familiy,
                   colour = "#666666"
  ) +
    labs(title = "Crecimiento de los casos de COVID-19 en Argentina", 
         subtitle = "Casos reales sobre la curva de Daniel Penazzi  3.3327 x 1.2405 ^ (dia_inicio+2)", 
         caption = "Datos reales: https://github.com/SistemasMapache/Covid19arData", 
         y = "Casos", 
         x = "Días (desde el 5/3)",
         color = NULL)  + 
  theme_elegante_std(base_family = base_familiy) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        panel.grid.minor = element_blank()
        )
