# osm streets in MA

library(tidyverse)
# library(osmdata)
library(osmextract)

# [out:csv(highway,name)];
# area
# [name="Brinches"];
# way(area)[highway][name];
# out;

street_types = c("primary","secondary","tertiary")
oe_match("us/new jersey")
ma_all <- oe_get("us/massachusetts",force_vectortranslate = TRUE)
nj_all <- oe_get("us/new-jersey",force_vectortranslate = TRUE)

# ma_bounds <- oe_get_boundary("us/massachusetts")

nj_streets <- nj_all |> 
  as_tibble() |> 
  select(name,highway) |> 
  filter(highway %in% street_types) |> 
  select(name) |> 
  na.omit() |> 
  # distinct() |> 
  filter(!str_detect(name, "West ")) |> 
  mutate(start_letter = str_sub(name,1,1)) |> 
  #group_by(start_letter,name) |> 
  count(start_letter)
