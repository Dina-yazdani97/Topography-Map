pacman::p_load(sf, rnaturalearth, tidyverse, patchwork, 
               extrafont, ncdf4, stars, terra,
               ncmeta, lwgeom)

nc_file <- file.choose()

nc_info <- ncmeta::nc_meta(nc_file)
print(nc_info$variable)

nc_data <- stars::read_ncdf(nc_file, var = "z")
print(st_crs(nc_data))

if(is.na(st_crs(nc_data))) {
  coords <- st_coordinates(nc_data)
  lon_range <- range(coords[,1], na.rm = TRUE)
  lat_range <- range(coords[,2], na.rm = TRUE)
  
  if(lon_range[1] >= -180 & lon_range[2] <= 180 & 
     lat_range[1] >= -90 & lat_range[2] <= 90) {
    st_crs(nc_data) <- 4326
    print("CRS set to WGS84 (EPSG:4326)")
  }
}

target_crs_robinson <- "ESRI:54030"

world_countries <- rnaturalearth::ne_countries(
  scale = 'medium', returnclass = 'sf')

world_oceans <- rnaturalearth::ne_download(
  scale = 'medium', 
  type = 'ocean', 
  category = 'physical',
  returnclass = 'sf')

print("Data loaded into 'world_countries' and 'world_oceans'.")

world_countries_robinson <- world_countries |>
  sf::st_transform(crs = target_crs_robinson)

world_oceans_robinson <- world_oceans |>
  sf::st_transform(crs = target_crs_robinson)


countries_union <- st_union(world_countries_robinson) |>
  st_buffer(dist = 0.1) 

nc_raster <- terra::rast(nc_file)

if(crs(nc_raster) == "") {
  ext_vec <- as.vector(ext(nc_raster))
  if(ext_vec[1] >= -180 & ext_vec[2] <= 180 & 
     ext_vec[3] >= -90 & ext_vec[4] <= 90) {
    crs(nc_raster) <- "EPSG:4326"
    print("Raster CRS set to WGS84")
  }
}


nc_raster_robinson <- terra::project(nc_raster, target_crs_robinson, method = "bilinear")


nc_raster_masked <- terra::mask(nc_raster_robinson, vect(countries_union))

nc_data_masked <- st_as_stars(nc_raster_masked)

print("Data masked to show only land areas.")

graticules <- st_graticule(
  lat = seq(-90, 90, by = 15), 
  lon = seq(-180, 180, by = 15), 
  crs = st_crs(4326)           
) |>
  st_transform(crs = target_crs_robinson) 

print("Graticules created and transformed.")


custom_palette <- c(
  rgb(84, 48, 5, maxColorValue = 255),    
  rgb(140, 81, 10, maxColorValue = 255),  
  rgb(191, 129, 45, maxColorValue = 255), 
  rgb(223, 194, 125, maxColorValue = 255),
  rgb(246, 232, 195, maxColorValue = 255),
  rgb(245, 245, 245, maxColorValue = 255),
  rgb(199, 234, 229, maxColorValue = 255),
  rgb(128, 205, 193, maxColorValue = 255),
  rgb(53, 151, 143, maxColorValue = 255), 
  rgb(1, 102, 95, maxColorValue = 255),  
  rgb(0, 60, 48, maxColorValue = 255) 
)

plot_transformed <- ggplot() + 
  geom_sf(
    data = world_oceans_robinson,
    fill = "lightskyblue",      
    color = NA,                
    linewidth = 0,
    alpha = 0.8
  ) +
  geom_stars(data = nc_data_masked, aes(fill = z), alpha = 0.9) +
  geom_sf(
    data = graticules,
    color = "white",           
    linewidth = 0.2,           
    linetype = "solid",
    alpha = 0.6
  ) +
  geom_sf(
    data = world_countries_robinson,
    fill = NA,         
    color = "black",            
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colours = custom_palette,
    na.value = NA,
    name = "Elevation (m)",
    guide = guide_colorbar(
      barwidth = 15,
      barheight = 0.8,
      title.position = "top",
      title.hjust = 0.5,
      face = "bold"
    )
  ) +
  ggtitle("Topography Map") + 
  labs(caption = paste("File:", basename(nc_file))) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.caption = element_text(size = 10, color = "gray50"),
    panel.grid = element_blank(),    
    axis.text = element_blank(),      
    axis.title = element_blank(),   
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  coord_sf(crs = target_crs_robinson, expand = FALSE)

print(plot_transformed)
print("NetCDF file information:")
print(paste("Variable:", names(nc_data)))
print(paste("Dimensions:", paste(dim(nc_data), collapse = " x ")))

ggsave(
  filename = "topography_map_high_quality.png", plot = plot_transformed, device = "png",
 path = getwd(), width = 16,height = 9, units = "in", dpi = 300, bg = "white")