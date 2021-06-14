library(magick)
library(image.binarization)

types <- c("otsu", "bernsen", "niblack", "sauvola", "wolf", "nick", "gatos", "su", "trsingh",
           "bataineh", "wan", "isauvola")

types <- c("otsu")


path <- "~/Tmp/TO/"
for (f in list.files(path, "*.tiff")) {
  name <- tools::file_path_sans_ext(f)
  img <- image_read(paste0(path, f))
  img <- image_convert(img, format = "PGM", colorspace = "Gray")
  out <- image_binarization(img, type = type)
  image_write(out, paste0(path, name, '.jpg'))
}


for (type in types) {
  img <- image_convert(img, format = "PGM", colorspace = "Gray")
  out <- image_binarization(img, type = type)
  image_write(out, paste0("~/Tmp/TO/8-",type, '.jpg'))
}
