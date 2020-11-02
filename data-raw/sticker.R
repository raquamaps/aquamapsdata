library(hexSticker)
library(here)

imgurl_src <-
  "https://www.aquamaps.org/pic/aquamaps.jpg"

browseURL(imgurl_src)

assets <- file.path(here(), "data-raw", "assets")
if (!dir.exists(assets)) dir.create(assets)

logo <- file.path(assets, "logo.jpg")
download.file(imgurl_src, logo)

circle <- file.path(assets, "circle.png")
cmd <- sprintf("convert -size 1186x1186 xc:none -fill %s -draw 'translate 593,593 circle 0,0 593,0' %s", logo, circle)

system(cmd)

imgurl_src <- circle
imgurl_dest <- file.path(assets, "sticker.png")

# requires imagemagick ie sudo apt install imagemagick-6.q16

sticker(filename = imgurl_dest,
  p_color = "#1954a6",
  h_fill = "#ffffff",
  h_color = RColorBrewer::brewer.pal(7, "Greys")[5],
  subplot = imgurl_src, package = "aquamapsdata",
  p_size = 20, s_x = 1, s_y = .75, s_width = .6
)

# dest location inspired by https://stackoverflow.com/questions/44113759
fig <- file.path(here(), "man", "figures")
if (!dir.exists(fig)) dir.create(fig, recursive = TRUE)

library(magick)
image_read(imgurl_dest) %>%
  image_resize("120x120") %>%
  image_write(file.path(here(), "man", "figures", "sticker.png"))


