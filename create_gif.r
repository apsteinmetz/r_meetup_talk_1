
library(rayshader)
library(magick)
library(tidyverse)
library(sf)
library(raster)
library(fs)

load("data/base_map.rdata")
load("data//kherson_elev.rdata")
load("data/road_overlay.rdata")

kh_elmat <- raster_to_matrix(
  kherson_elev)

plot_rising_water <- function(water_level = 0, bottom_level = -2, save = "FALSE") {
  flood_elmat <- round(kh_elmat)
  flood_elmat <- ifelse(flood_elmat <= water_level & flood_elmat >= bottom_level, -2, flood_elmat)
  full_map <-  base_map |>
    add_overlay("img/esri_world_imagery.png") |> 
    add_water(detect_water(flood_elmat), color = "desert") |>
    add_overlay(roads)
  if (save) {
    save_png(
      filename = paste0("img/frames/flood_", 
                        formatC(water_level, width = 3, flag = "0"), ".png"),
      title_text = paste0(
        "Flood Inundation of the Dnipro\nAfter Kakhovka Dam Destruction\n
        Water Level:",
        formatC(water_level, width = 3, flag = " ")," Meters"),
      title_size = 60, title_bar_color = "white"
    )
  } else{
    plot_map(full_map)
  }
}

draw_inset <- function(water_level){
  df <- enframe(water_level)
  gg <- ggplot(df,aes(1,water_level)) + geom_col(fill="blue") +
    ylim(0,10) + 
    theme_void() + 
    theme(panel.grid.major.y = element_line(),
          panel.background = element_rect(fill="forestgreen")) + 
    annotate("text",x=1,y=9,
             label = "Flood",  size = 40) +
    annotate("text",x=1,y=4,
             label = paste0(water_level," Meters"), size = 40) 
  ggsave(paste0("insets/inset_",water_level,".png"),gg,device = "png",path="img")
}

0:8 |> walk(plot_rising_water)
0:8 |> walk(draw_inset)

man <- image_read("img/man.png")
new_man <- image_scale(man, "x200")
fnames <- paste0("img\\frames\\", dir("img/frames"))
fnames2 <- paste0("img\\frames2\\", dir("img/frames"))
frames <- image_read(fnames[1:9])
inames <- paste0("img\\insets\\", dir("img/insets"))
insets <- image_read(inames[1:9])
new_insets <- image_scale(insets, "600x1000!")
new_frames <- frames

# Add plot to annotated graphic
# image_composite is not vectorized properly
for (n in 1:9){
new_frames[n] <- image_composite(frames[n],new_insets[n], 
                                 gravity = "southeast",offset = "+100+100") |> 
  image_composite(new_man, gravity = "southeast",offset = "+600+140")
  image_write(image=new_frames[n],path=fnames2[n])
}


fnames2 <- paste0("img\\frames2\\", dir("img/frames"))
image_read(fnames2[1:9])  |> 
  image_resize("1000x") |> # make size manageable
  image_morph() |>         # smooth transitions. default is 3 frames per frame.
  image_animate() |>       # default is 10 fps
  image_write(path = "img/flood.gif",format = "gif")

