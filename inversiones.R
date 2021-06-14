ipc <- read_csv("https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.9/download/indice-precios-al-consumidor-apertura-por-categorias-base-diciembre-2016-mensual.csv")
us <- read_csv("https://infra.datos.gob.ar/catalog/sspm/dataset/168/distribution/168.1/download/datos-tipo-cambio-usd-futuro-dolar-frecuencia-diaria.csv",
               col_types = cols(
                 indice_tiempo = col_date(format = ""),
                 tipo_cambio_bna_vendedor = col_number(),
                 tipo_cambio_a3500 = col_skip(),
                 tipo_cambio_mae = col_skip(),
                 volumen_mae = col_skip(),
                 tipo_cambio_implicito_en_adrs = col_skip(),
                 futuro_rofex_usd1m = col_skip(),
                 interes_abierto_1m = col_skip(),
                 futuro_rofex_usd2m = col_skip(),
                 interes_abierto_2m = col_skip(),
                 futuro_rofex_usd3m = col_skip(),
                 interes_abierto_3m = col_skip(),
                 futuro_rofex_usd4m = col_skip(),
                 interes_abierto_4m = col_skip(),
                 futuro_rofex_usd5m = col_skip(),
                 interes_abierto_5m = col_skip(),
                 futuro_rofex_usd6m = col_skip(),
                 interes_abierto_6m = col_skip()
               )) %>% 
  filter(indice_tiempo >= '2016-12-01')

  #
