# Data sources:
# UKCS
# https://opendata-nstauthority.hub.arcgis.com/maps/NSTAUTHORITY::nsta-wells-wgs84/about
# FAROE ISLANDS
# https://jf.fo/en/datur/boridatur/
# NORWAY
# https://factpages.npd.no
# DENMARK
# https://eng.geus.dk/products-services-facilities/archives/the-subsurface-archive/well-data
# NETHERLANDS
# https://www.nlog.nl/en/boreholes



# PULL FONT FROM GOOGLE FONTS ---------------------------------------------

sysfonts::font_add_google(name = "Ubuntu", family = "ubuntu")
showtext::showtext_auto()



# SET CRS FOR OUR PROJECTION ----------------------------------------------

# https://epsg.io/3035
projection_crs <- 3035



# CREATE BACKGROUND POLYGON FOR NORWAY ------------------------------------

# Change scale value to 10 to get high resolution coastline - you'll need to install the "rnaturalearthhires" package for this
# devtools::install_github("ropensci/rnaturalearthhires")

background_polygon <-
  rnaturalearth::ne_countries(scale = 110,
                              continent = c("Europe", "North America"),
                              returnclass = "sf")



# READ IN SAVED WELL DATA -------------------------------------------------

# UK
wells_uk <-
  readr::read_csv(file = "data/UK.csv",
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)

# FAROE ISLANDS
wells_faroe_islands <-
  sf::st_read("data/faroe_islands_shp/2015.08.24_FO_Offshore_Wells.shp")

# NORWAY
wells_norway_exploration <-
  readr::read_csv(file = "data/norway_exploration.csv",
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)

wells_norway_development <-
  readr::read_csv(file = "data/norway_development.csv",
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)

# DENMARK
wells_denmark <-
  sf::st_read("data/denmark_shp/samba_wellbores.shp")

# NETHERLANDS
wells_netherlands <-
  readxl::read_excel(path = "data/netherlands.xls",
                     guess_max = 200000)



# SELECT COLUMNS AND CONVERT TO 4326 SPATIAL DATAFRAMES -------------------

# UK
wells_uk <-
  wells_uk |>
  dplyr::select(wellbore_id = WELLBOREID,
                latitude = Y,
                longitude = X) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = sf::st_crs(4326),
               remove = FALSE)

# FAROE ISLANDS
wells_faroe_islands <-
  wells_faroe_islands |>
  dplyr::select(longitude = X,
                latitude = Y) |>
  tibble::rowid_to_column(var = "wellbore_id")

# NORWAY
wells_norway_exploration <-
  wells_norway_exploration |>
  dplyr::select(wellbore_id = wlbNpdidWellbore,
                latitude = wlbNsDecDeg,
                longitude = wlbEwDesDeg)

wells_norway_development <-
  wells_norway_development |>
  dplyr::select(wellbore_id = wlbNpdidWellbore,
                latitude = wlbNsDecDeg,
                longitude = wlbEwDesDeg)

wells_norway <-
  dplyr::bind_rows(wells_norway_exploration,
                   wells_norway_development) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = sf::st_crs(4230),
               remove = FALSE) |>
  sf::st_transform(4326)

# DENMARK
wells_denmark <-
  wells_denmark |>
  dplyr::select(wellbore_id = id_hidden) |>
  dplyr::mutate(wellbore_id = as.numeric(wellbore_id)) |>
  sf::st_transform(4326) |>
  sf::st_difference(background_polygon |> dplyr::filter(geounit == "Denmark")) |>
  dplyr::select(wellbore_id)

# NETHERLANDS
wells_netherlands <-
  wells_netherlands |>
  dplyr::filter(`On offshore` == "OFF") |>
  dplyr::select(wellbore_id = UWI,
                longitude = `Longitude ED50`,
                latitude = `Latitude ED50`) |>
  dplyr::mutate(wellbore_id = as.numeric(wellbore_id)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = sf::st_crs(4230),
               remove = FALSE) |>
  sf::st_transform(4326)



# BIND ALL SPATIAL DATAFRAMES TOGETHER -------------------------------------

wells_all <-
  dplyr::bind_rows(wells_norway,
                   wells_uk,
                   wells_netherlands,
                   wells_faroe_islands,
                   wells_denmark)



# CONVERT TO EQUAL AREA CRS FOR CALCULATION AND PROJECTION ----------------

wells_all <-
  wells_all |>
  sf::st_transform(projection_crs)



# DEFINE BOUNDING BOX FOR MAP IN PROJECTION COORDINATES -------------------

x_min <- 3450000
x_max <- 4250000
y_min <- 3200000
y_max <- 4500000

display_box_proj_bbox <-
  sf::st_sfc(sf::st_point(c(x_min, y_min)),
             sf::st_point(c(x_max, y_max)),
             crs = projection_crs)

display_box_proj_coords <-
  display_box_proj_bbox |>
  sf::st_coordinates()



# CALCULATE ASPECT RATIO FOR SAVING PNG -----------------------------------

display_box_aspect_ratio <-
  (display_box_proj_coords[2, 2] - display_box_proj_coords[1, 2]) / (display_box_proj_coords[2, 1] - display_box_proj_coords[1, 1])



# CALCULATE OTHER XY LOCATIONS FOR ANNOTATIONS ----------------------------

x_centre <- (x_min + x_max) / 2

shunt_from_corner_by <- 0.01
x_bottom_right_corner <- x_max - (x_max - x_min) * shunt_from_corner_by
y_bottom_right_corner <- y_min + ((y_max - y_min) * shunt_from_corner_by)

shunt_from_corner_by_line_top <- 0.017
y_bottom_right_corner_line_top <- y_min + ((y_max - y_min) * shunt_from_corner_by_line_top)

shunt_from_edge_by_title <- 0.02
shunt_from_edge_by_subtitle <- 0.047
y_top_title <- y_max - (y_max - y_min) * shunt_from_edge_by_title
y_top_subtitle <- y_max - (y_max - y_min) * shunt_from_edge_by_subtitle



# CALCULATE VORONOI POLYGONS AND AREAS ------------------------------------

wells_voronoi <-
  wells_all |>
  dplyr::distinct(geometry, .keep_all = TRUE) |>
  sf::st_union() |>
  sf::st_voronoi() |>
  sf::st_collection_extract()

wells_voronoi <-
  wells_voronoi |>
  sf::st_crop(display_box_proj_bbox)

wells_voronoi_areas <-
  wells_voronoi |>
  sf::st_area() |>
  units::drop_units() |>
  tibble::as_tibble_col(column_name = "area_km2")

wells_voronoi <-
  wells_voronoi |>
  sf::st_as_sf() |>
  dplyr::bind_cols(wells_voronoi_areas)



# MAKE VORONOI MAP --------------------------------------------------------

# set to 1 for 1200 dpi, 2 for 600 dpi, or 4 for 300 dpi
dev_text_scale_factor <- 1

plot_voronoi <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = wells_voronoi,
                   mapping = ggplot2::aes(fill = area_km2),
                   colour = "#f5f5f2",
                   size = 0.1) +
  ggplot2::geom_sf(data = background_polygon,
                   colour = NA) +
  ggplot2::coord_sf(xlim = display_box_proj_coords[,'X'], ylim = display_box_proj_coords[,'Y'],
                    crs = projection_crs,
                    expand = FALSE,
                    datum = NA) +
  ggplot2::scale_fill_distiller(palette = "YlGnBu",
                                direction = 1,
                                trans = "log10",
                                guide = NULL) +
  ggplot2::annotate(geom = "text",
                    label = "Spatial density of oil & gas wells in the North Sea",
                    x = x_centre,
                    y = y_top_title,
                    hjust = 0.5,
                    vjust = 1,
                    size = (19 * 5) / dev_text_scale_factor,
                    colour = "#E5E5E5") +
  
  ggplot2::annotate(geom = "text",
                    label = paste0("Voronoi polygons calculated from ", format(nrow(wells_voronoi), big.mark = ","), " wellhead coordinates"),
                    x = x_centre,
                    y = y_top_subtitle,
                    hjust = 0.5,
                    vjust = 1,
                    size = (10 * 5) / dev_text_scale_factor,
                    colour = "#E5E5E5") +
  
  ggplot2::annotate(geom = "text",
                    label = "Map author: Sam Fielding, 2022; Map licence: CC BY-SA 4.0; Projection: EPSG:3035; Country outline: Natural Earth;",
                    x = x_bottom_right_corner,
                    y = y_bottom_right_corner_line_top,
                    hjust = 1,
                    vjust = 1,
                    size = (18) / dev_text_scale_factor,
                    colour = "#939184") +
  
  ggplot2::annotate(geom = "text",
                    label = "Data: UK data.nstauthority.co.uk, Faroe Islands jf.fo, Norway factpages.npd.no, Denmark data.geus.dk, Netherlands nlog.nl",
                    x = x_bottom_right_corner,
                    y = y_bottom_right_corner,
                    hjust = 1,
                    vjust = 1,
                    size = (18) / dev_text_scale_factor,
                    colour = "#939184") +
  
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "ubuntu",
                                              color = "#4e4d47",
                                              size = (20 * 10) / dev_text_scale_factor),
                 
                 plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.key = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 axis.title = ggplot2::element_blank())


ggplot2::ggsave(filename = "north_sea_wells_voronoi.png",
                plot = plot_voronoi,
                units = "mm",
                width = 200,
                height = 200 * display_box_aspect_ratio * 0.995,
                dpi = 1200 / dev_text_scale_factor)
