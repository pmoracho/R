run_cmd <- function(cmd, wait=TRUE, echo=FALSE) {
  
  if (.Platform$OS.type == "windows") {
    shell(cmd, wait=wait)
  } else {
    system(cmd, wait=wait)
  }
  
  if (echo) {
    cat(cmd, "\n")
  }
}

get_loudness_data_from_audio_file <- function(file, from=NA, duration=NA) {
  
  assert <- function (expr, error) {
    if (!expr) stop(error, call. = FALSE)
  }
  
  assert(file.exists(file), paste(file, 'does wnot exist!'))
  
  tmpfile <-tempfile("get_loudness_ts_from_file_out")
  
  paste('-ss',
        ifelse(is.na(from), '00:00:00', from),
        ifelse(is.na(duration), '', paste('-t', duration))
  ) -> cut_param
  
  ff_cmd <- paste(Sys.which("ffmpeg"), '-i', file, cut_param, "-af ebur128 -f null - 2>", tmpfile)
  cat(paste0("Get loudness from ", file, " using ffmpeg..."), collapse = "\n")
  # cat(ff_cmd, collapse = "\n")

  run_cmd(ff_cmd)
  assert(file.exists(tmpfile), paste('working file:', tmpfile, 'does wnot exist!'))
  cat("Parsinng data...", collapse = "\n")
  parse_ffmpeg_out(tmpfile, n = 10000)

}

parse_ffmpeg_out <- function(filepath, n = 1) {
  
  pattern <- "t:\\s*(\\d*\\.?\\d*)\\s*M:\\s*(-\\d*\\.?\\d*)\\s*S:\\s*(-\\d*\\.?\\d*)\\s*I:\\s*(-\\d*\\.?\\d*)\\s*LUFS\\s*LRA:\\s*(\\d*\\.?\\d*)\\s*LU"
  con = file(filepath, "r")
  dflist <-  list()
  e <- 1
  while ( TRUE ) {
    lines = readLines(con, n)
    if ( length(lines) == 0 ) {
      break
    } else {
      for (i in 1:length(lines)){
        capture <- gsub(pattern, "\\1,\\2,\\3,\\4,\\5", regmatches(lines[i], gregexpr(pattern,lines[i])))
        data <- strsplit(capture, ",")[[1]]
        if (length(data) == 5) {
          data <- as.numeric(data)
          dflist[[e]] <- data.frame(t=data[1], M=data[2], S=data[3], I=data[4], LRA=data[5])
          e <- e + 1
        }
      }
    }
  }
  close(con)
  m <- do.call('rbind', dflist)
}


df <- get_loudness_data_from_audio_file('~/Descargas/R_MIC_091018-164437.mp3', from='00:00:00', duration='01:00:00')
df <- get_loudness_data_from_audio_file('~/Descargas/R_MIC_091018-164437.mp3')
summary(df)

View(df)


filepath <- '~/Descargas/R_MIC_091018-164437.mp3'
filename <- basename(filepath)
pattern <- "R_MIC_(\\d{2})(\\d{2})(\\d{2})-(\\d{2})(\\d{2})(\\d{2})[.].*"
gsub(pattern, "\\1,\\2,\\3,\\4,\\5,\\6", regmatches(filename, gregexpr(pattern,filename)))


library(tidyverse)
library(ggelegant)
library(zoo)

df$dt <- as.POSIXct("2018-10-09 16:44:37") + df$t

df$M_sd <- rollapplyr(df$M, 6000, sd, fill = 0)

df %>% 
  ggplot(aes(x=dt, y=M_sd)) +
  geom_line(color="grey50") +
  theme_elegante_std()

df %>% 
  filter(t>1) %>% 
  ggplot(aes(x=M)) +
  # geom_density() +
  geom_histogram()+
  theme_elegante_std()


df %>% 
  filter(t>1) %>% 
  summarise(quantile = scales::percent(seq(0, 1, length = 101)),
            M = quantile(M, prob=seq(0, 1, length = 101))) -> percentiles


df %>% 
  filter(t>1) %>% 
  mutate(M_su= ifelse(M > percentiles[100, "M"], M, percentiles[100, "M"])) %>% 
  ggplot(aes(x=dt, y=M_su)) +
  geom_line(color="grey50") +
  theme_elegante_std()
