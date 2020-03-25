IDENTHOGAR <- c(1011,1021,1041,1051,1071,1091,1234,1235,1236) # codigo del hogar del adulto seleccionado
A7_2a <- c(1,4,2,1,1,1,1,2,1) # numero asignado al adulto seleccionado para la encuesta de adultos
niv_est <- c(1,4,4,2,3,4,4,4,2) # nivel de estudios del adulto seleccionado
edad <- c(84,26,56,70,61,35,55,38,39) # edad del adulto seleccionado
datos_adultos<-data.frame(IDENTHOGAR, A7_2a, niv_est, edad)

IDENTHOGAR <- c(1011,1021,1021,1021,1021,1021,1021,1041,1041) # codigo del hogar de la persona que hace la encuesta de hogares
NORDEN_A <- c(1,1,2,3,4,5,NA,1,2) # numero de orden del adulto que hace la encuesta de hogar
A7_2a <- c(1,4,4,4,4,4,4,2,2) # numero asignado al adulto seleccionado para la encuesta de adultos
m2 <- c(63,85,85,85,85,85,85,91,91) # metros cuadrados del hogar
uds_consumo<- c(1,4.3,4.3,4.3,4.3,4.3,4.3,1.7,1.7) # unidades de consumo del hogar
datos_hogares<-data.frame(IDENTHOGAR, NORDEN_A, A7_2a, m2,uds_consumo)

do.call(cbind, lapply(datos_adultos, function(x) as.factor(as.character(x))))

datos_adultos$IDENTHOGAR <- as.factor(as.character(datos_adultos$IDENTHOGAR))
datos_hogares$IDENTHOGAR <- as.factor(as.character(datos_adultos$IDENTHOGAR))
str(datos_adultos)
merge(
  datos_adultos,
  datos_hogares,
  by.x = c("IDENTHOGAR", "A7_2a"),
  by.y = c("IDENTHOGAR", "NORDEN_A"),
  all.x = TRUE
)

# Variables de la Encuesta Adultos con respuesta de NS/NC
vars_na_89_A <- c("enf_cron","SM_estres","tipo_act_fis","frec_act_fis","fuma")

vars_na_8_A <- c("sit_lab")
vars_na_9_A <- c("clase_pr")
vars_na_9899_A <- c("niv_est")
vars_na_998999_A <- c("altura","peso")

# Variables de la Encuesta Hogares con respuesta de NS/NC
vars_na_89_H <- c("ruido","malos","agua","limpieza", "cont_indus","cont_otras",
                  "escasez_verde","molest_animal" ,"delincuencia")
vars_na_9899_H <- c("n_dormitorios", "ingreso")
vars_na_m2_H <-c("m2")

set.seed(2020)
df <- as.data.frame(matrix(sample(c(1:9,98,99,998,999), 100, replace = TRUE), ncol=10))
df

lista_reemplazo <- list(
  list(cols=c("V1", "V2"), na_vals=c(9,8)),
  list(cols=c("V3"), na_vals=c(998)),
  list(cols=c("V4", "V5", "V6"), na_vals=c(998, 999)),
  list(cols=c("V7", "V8"), na_vals=c(9)),
  list(cols=c("V9", "V10"), na_vals=c(8, 9, 998, 999))
)

for (reemplazo in lista_reemplazo) {
  for (col in reemplazo$cols) {
    df[df[, col] %in% reemplazo$na_vals, col] <- NA
  }
}
df

casos <- df == reemplazo$na_vals

casos[, reemplazo$cols]

df[, reemplazo$cols]


datos_adultos <- read.fwf(
  file="~/Tmp/MICRODAT.CA.txt",
  widths=c(2, 8, 2, 1, 3, 1, 1, 1, 1, 2, 1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 3, 3,
           1, 1, 1, 2, 1, 1, 1, 1, 2, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 1, 1, 2, 1, 1, 3, 1, 1, 3, 1, 1, 1, 2, 2, 2, 2,
           2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1,
           1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2,
           2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2,
           2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 11, 1, 1, 5, 5, 5))

colnames(datos_adultos) <- c("CCAA", "IDENTHOGAR", "A7_2a", "SEXOa", "EDADa", "ACTIVa", "PROXY_0",
                             "PROXY_1", "PROXY_2", "PROXY_2b", "PROXY_3b", "PROXY_4", "PROXY_5", "E1_1",
                             "E2_1a", "E2_1b", "E2_1c", "E2_1d", "E3", "E4", "E4b", "NIVEST", "F6", "F7",
                             "F8_2", "F9_2", "F10", "F11", "F12", "F13", "F14a", "F14b", "F15", "F16",
                             "F17", "F18a_2", "F18b_2", "F19a_2", "F19b_2", "F20", "G21", "G22", "G23",
                             "G24", "G25a_1", "G25b_1", "G25c_1", "G25a_2", "G25b_2", "G25c_2", "G25a_3",
                             "G25b_3", "G25c_3", "G25a_4", "G25b_4", "G25c_4", "G25a_5", "G25b_5", "G25c_5",
                             "G25a_6", "G25b_6", "G25c_6", "G25a_7", "G25b_7", "G25c_7", "G25a_8", "G25b_8",
                             "G25c_8", "G25a_9", "G25b_9", "G25c_9", "G25a_10", "G25b_10", "G25c_10",
                             "G25a_11", "G25b_11", "G25c_11", "G25a_12", "G25b_12", "G25c_12", "G25a_13",
                             "G25b_13", "G25c_13", "G25a_14", "G25b_14", "G25c_14", "G25a_15", "G25b_15",
                             "G25c_15", "G25a_16", "G25b_16", "G25c_16", "G25a_17", "G25b_17", "G25c_17",
                             "G25a_18", "G25b_18", "G25c_18", "G25a_19", "G25b_19", "G25c_19", "G25a_20",
                             "G25b_20", "G25c_20", "G25a_21", "G25b_21", "G25c_21", "G25a_22", "G25b_22",
                             "G25c_22", "G25a_23", "G25b_23", "G25c_23", "G25a_24", "G25b_24", "G25c_24",
                             "G25a_25", "G25b_25", "G25c_25", "G25a_26", "G25b_26", "G25c_26", "G25a_27",
                             "G25b_27", "G25c_27", "G25a_28", "G25b_28", "G25c_28", "G25a_29", "G25b_29",
                             "G25c_29", "G25a_30", "G25b_30", "G25c_30", "G25a_31", "G25b_31", "G25c_31",
                             "G25a_32", "G25b_32", "G25c_32", "H26_1", "H26_2", "H26_3", "H27", "I28_1",
                             "I28_2", "I29_1", "I29_2", "K32", "K33", "K34", "K35", "K36", "K37", "K38",
                             "K38a", "L39_1", "L39_2", "L39_3", "L39_4", "L39_5", "L40", "L41", "L42_1",
                             "L42_2", "L42_3", "L42_4", "L42_5", "L42_6", "L42_7", "L43", "L44", "L45",
                             "L46", "M47_1", "M47_2", "M47_3", "M47_4", "M47_5", "M47_6", "M47_7", "M47_8",
                             "M47_9", "M47_10", "M47_11", "M47_12", "M47a", "M47b", "N48", "N49", "N50",
                             "N51", "N52", "N53", "N54", "N55_1", "N55_2", "N55_3", "N56_1", "N56_2",
                             "N56_3", "N57", "N58_1", "N58_2", "N58_3", "N59", "N60_1", "N60_2", "N60_3",
                             "N60_4", "N60a_1", "N60a_2", "N60a_3", "N60a_4", "N61_1", "N61_2", "N61_3",
                             "N61_4", "N61_5", "N62", "N62b", "N63_1", "N63_2", "N63_3", "N63_4", "N63_5",
                             "N63_6", "N63_7", "N63_8", "N63_9", "N63_10", "N64", "N65_1", "N65_2", "N65_3",
                             "N65_4", "N65_5", "N65_6", "N65_7", "N65_8", "O66", "O67", "O69", "O70", "O71",
                             "O72", "O73", "O74", "O75", "O76", "O77", "O78", "O79", "O80_1", "O80_2",
                             "O80_3", "O81_1", "O81_2", "O81_3", "O82_1", "O82_2", "O83", "O84_1", "O84_2",
                             "O84_3", "O84_4", "O84_5", "O84_6", "O84_7", "O84_8", "O84_9", "P85", "P86",
                             "P87_1a", "P87_1b", "P87_2a", "P87_2b", "P87_3a", "P87_3b", "P87_4a", "P87_4b",
                             "P87_5a", "P87_5b", "P87_6a", "P87_6b", "P87_7a", "P87_7b", "P87_8a", "P87_8b",
                             "P87_9a", "P87_9b", "P87_10a", "P87_10b", "P87_11a", "P87_11b", "P87_12a",
                             "P87_12b", "P87_13a", "P87_13b", "P87_14a", "P87_14b", "P87_15a", "P87_15b",
                             "P87_16a", "P87_16b", "P87_17a", "P87_17b", "P87_18a", "P87_18b", "P87_19a",
                             "P87_19b", "P87_20a", "P87_20b", "P87_21a", "P87_21b", "P87_22a", "P87_22b",
                             "P87_23a", "P87_23b", "Q88", "Q89", "Q90", "Q91", "Q92", "Q93", "Q94", "Q95",
                             "Q96", "Q97", "Q98", "Q99", "Q100", "Q101", "Q102", "Q103", "Q104", "Q105",
                             "R106", "R107", "R108_1", "R108_2", "R108_3", "R108_4", "S109", "S110", "T111",
                             "T112", "T113", "T114_1", "T114_2", "T115", "T116_1", "T116_2", "T117",
                             "T118_1", "T118_2", "T119_1", "T119_2", "U120_1", "U120_1a", "U120_2",
                             "U120_3", "U120_4", "U120_5", "U120_6", "U120_7", "U120_7a", "U120_8",
                             "U120_9", "U120_10", "U120_11", "U120_12", "U120_13", "U120_14", "U120_15",
                             "U120_15a", "U120FZ", "U120CANTFZ", "U2_120F", "V121", "V122", "V123", "V124",
                             "V125", "V126", "W127", "W128Cer", "W128Cer_1", "W128Cer_2", "W128Cer_3",
                             "W128Cer_4", "W128Cer_5", "W128Cer_6", "W128Cer_7", "W128Vin", "W128Vin_1",
                             "W128Vin_2", "W128Vin_3", "W128Vin_4", "W128Vin_5", "W128Vin_6", "W128Vin_7",
                             "W128Vermut", "W128Vermut_1", "W128Vermut_2", "W128Vermut_3", "W128Vermut_4",
                             "W128Vermut_5", "W128Vermut_6", "W128Vermut_7", "W128Lic", "W128Lic_1",
                             "W128Lic_2", "W128Lic_3", "W128Lic_4", "W128Lic_5", "W128Lic_6", "W128Lic_7",
                             "W128Comb", "W128Comb_1", "W128Comb_2", "W128Comb_3", "W128Comb_4",
                             "W128Comb_5", "W128Comb_6", "W128Comb_7", "W128Sidra", "W128Sidra_1",
                             "W128Sidra_2", "W128Sidra_3", "W128Sidra_4", "W128Sidra_5", "W128Sidra_6",
                             "W128Sidra_7", "W129", "X130_1", "X130_2", "X130_3", "X130_4", "X130_5",
                             "X130_6", "X130_7", "X130_8", "X130_9", "X130_10", "X130_11", "Y133", "Y134",
                             "Y135", "FACTORADULTO", "CLASE_PR", "IMCa", "CMD1", "CMD2", "CMD3")

datos_hogares <- read.fwf(
  file="~/Tmp/MICRODAT.CH.txt",
  widths = c(2, 8 ,1, 1, 3, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 1, 1, 1, 1, 3, 3, 1, 1, 3,
             3, 3, 3, 1, 1, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 2, 11, 1))

colnames(datos_hogares) <- c("CCAA","IDENTHOGAR", "ESTRATO", "SEXO_i", "EDAD_i", "NORDEN_Ai", "NORDEN_Mi",
                             "NADULTOS", "NMENORES", "A7_1_i", "NORDEN_Pref", "A7_2a", "A7_2m", "A8_1_i",
                             "A8_2_i", "NORINF", "A9_otra", "A10_i", "A11_i", "A12", "B13", "B14", "B15_2",
                             "B16_2", "B17", "B18", "B19a_2", "B19b_2", "B20a_2", "B20b_2", "B21a", "B21b",
                             "C22", "C23", "C24_1", "C24_2", "C24_3", "C24_4", "C24_5", "C24_6", "C24_7",
                             "C24_8", "C24_9", "D26_1", "D26_2", "D26_3", "D26_4", "D26_5", "D26_6",
                             "D26_7", "D26_8", "D26_9", "D26_10", "D26_11", "D27", "D29", "FACTORHOGAR",
                             "CLASE_PRp"
)


datos_def <- merge(datos_adultos,
                   datos_hogares,
                   by.x = c("IDENTHOGAR", "A7_2a"),
                   by.y = c("IDENTHOGAR", "NORDEN_Ai"),
                   all.x = TRUE)

library(tidyverse)
glimpse(datos_def)

datos_def[datos_def$IDENTHOGAR == 1011, ] %>%
  View()

datos_def <- datos_def[order(datos_def$IDENTHOGAR),]
datos_def %>% 
  View()

datos_adultos %>% 
  left_join(datos_hogares, by = c("IDENTHOGAR", "A7_2a" = "NORDEN_Ai")) %>% 
  View()

datos_adultos %>% 
  View()

library("pyramid")

pyramid()

ages <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-')
males <- c(34,19,11,11,8,7,5)
females <- c(26,25,16,11,7,5,1)
data <- data.frame(males,females,ages)
pyramid(data)


ob_2005 <- read.csv("censo 2005.csv", header = TRUE)
Pob_t2005 <- data.frame(Hombres = Pob_2005$Hombres[-ncol(Pob_2005)], Mujeres = Pob_2005$Mujeres[-ncol(Pob_2005)], Edades = Pob_$Edades[-nrow(Pob_2005)])

pyramid(data, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edades", AxisFM = "d", AxisBM = ",", Lcol = "Cyan", Rcol = "Pink", main = "Tlaxcala 2005" )
