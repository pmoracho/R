# 30díasdegráficos con R - día 15 - Dendograma
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggdendro + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 15: Dendograma para identificar y segmentar en 5 grupos las provincias
# segun la cantidad de casos de COVID-19, basado en este excelente artículo: https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia15.R

library("tidyverse")
library("ggdendro")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}

set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1,
                          nbreaks     = 10) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <- pretty(segment(hcdata)$y, n = nbreaks)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks, labels = scales::comma)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks, labels = scales::comma)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks, labels = scales::comma)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p
}

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')
# Dependiendo del horario, la última fila puede ser un placeholder si datos
# covid.data <- covid.data[covid.data$dia_inicio != max(covid.data$dia_inicio),]

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))
first_date <- min(as.Date(covid.data$fecha,"%d/%m/%Y"))
ndias <- max(covid.data$dia_inicio)
dias <- expand.grid(distrito = unique(covid.data$osm_admin_level_4),
                    dia = 1:ndias,
                    stringsAsFactors = FALSE)

dias %>% 
  left_join(covid.data, by=c("distrito" = "osm_admin_level_4", "dia" = "dia_inicio")) %>% 
  mutate(casos = replace_na(nue_casosconf_diff, 0)) %>% 
  select(distrito, casos)  %>% 
  filter(distrito != 'Indeterminado') %>% 
  group_by(distrito) %>% 
  summarise(casos = sum(casos)) %>% 
  arrange(-casos) %>% 
  mutate(porc = cumsum(casos/sum(casos))) -> data

data_d <- as.data.frame(data[, 2])
rownames(data_d) <- data$distrito

D   <- dist(data_d)
hc  <- hclust(D)
hcdata <- dendro_data_k(hc, 5)

plot_ggdendro(hcdata,
              direction   = "rl",
              label.size  = 3,
              branch.size = .8,
              nudge.label = 0.02,
              expand.y    = 0.2,
              nbreaks     = 10) +
  # scale_y_continuous(labels = scales::comma) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Agrupación de provincias por cantidad de casos en 5 grupos principales (al: ", last_date, ")"), 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData",
       y = "Cantidad de casos",
       x = "")
