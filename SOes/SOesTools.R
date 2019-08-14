CreateSOesData <- function(url, path, path7Z, force.download = FALSE) {
  
  fileext <- basename(url)
  zipfile <- file.path(path, fileext)
  datapath <- file.path(path, tools::file_path_sans_ext(fileext))
  
  if (!file.exists(datapath) || force.download) {
    if (!file.exists(zipfile) || force.download) {
      download.file(url, zipfile)
    }
  }
  system(paste(path7Z, " e -o", path, zipfile))

}

url = "https://archive.org/download/stackexchange/spanish.stackexchange.com.7z"
data.path <- file.path(getwd(),"data")

CreateSOesData(url, data.path, path7Z="E:\pm\bin\7-Zip\7z.exe")
