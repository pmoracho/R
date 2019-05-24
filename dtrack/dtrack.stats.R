library(readr)
library(lubridate)
library(tidyverse)


windowsFonts(Raleway=windowsFont("Raleway"))

dd <- read_delim("dtrack.data.csv",
                 ";", escape_double = FALSE, col_types = cols(FechaRecepcion = col_character()),
                 trim_ws = TRUE)


dd$FechaRecepcion <- parse_date_time(dd$FechaRecepcion, 'dmy HMS')
dd$period <- year(dd$FechaRecepcion) * 100 + month(dd$FechaRecepcion)
dd$mes <- month(dd$FechaRecepcion)

dd %>%
    filter(ResponsableEvento == 'PM') %>%
    group_by(period, mes) %>%
    summarise(n=n()) %>%
    arrange(period) %>%

dd %>%
    filter(ResponsableEvento=="PM") %>%
    group_by(period, mes) %>%
    summarise(n=n()) %>%
    ggplot(aes(x = factor(mes), y=n, fill=factor(mes))) +
    geom_boxplot() +
    labs(title="Eventos PM",
         subtitle="Cantidad total por período",
         caption="fuente: dtrack",
         y="Cantidad",
         x="Periodo") +
    theme_elegante() +
    theme(legend.position = "none")

dd %>%
    filter(ResponsableEvento!="PM") %>%
    group_by(period, mes) %>%
    summarise(n=n()) %>%
    ggplot(aes(x = factor(mes), y=n, fill=factor(mes))) +
    geom_boxplot() +
    labs(title="Eventos Otros",
         subtitle="Cantidad total por período",
         caption="fuente: dtrack",
         y="Cantidad",
         x="Periodo") +
    theme_elegante() +
    theme(legend.position = "none")


