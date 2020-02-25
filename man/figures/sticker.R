
# RStudio: https://github.com/rstudio/hex-stickers
# hexbin: http://hexb.in/
# hexbin github: https://github.com/maxogden/hexbin
# make stickers with: https://github.com/GuangchuangYu/hexSticker


# Note the hexbin github readme says to use
# 181x209 as a png (preview on mac can easily resize)
# and also provide an svg
# I used this to convert png to svg: https://www.aconvert.com/image/png-to-svg/


# https://coolors.co/193763-f4f4f4-00173d-ff5a5f-c81d25




library(hexSticker)
sticker(

  # image
  "man/figures/ff4.png",
  s_x=1.07, # slightly to right to appear centered
  s_y=0.9,
  s_width=.75,
  s_height=.75,

  # package name
  package="collidr",
  p_size=9.5,
  p_color = "#383838", # 00030A 010101
  p_y = 1.475,

  # Output file
  filename="man/figures/collidr.png",

  # Background colour
  h_fill = "#F0F0F0", # #F0F0F0


  # Border
  # Grey colours: https://www.w3schools.com/colors/colors_shades.asp
  h_color = "#30287F",   # 3F4243 7F2B94 3B2691 4238AF
  # h_size = 1.5,

  dpi = 1000 # otherwise the final fantasy image quality is not good
);system("open man/figures/collidr.png")

