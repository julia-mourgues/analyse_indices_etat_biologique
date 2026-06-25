## Projection Lambert 93
crs_lambert93 <- sf::st_crs(2154)

## pour fond de carte
background <-
  rnaturalearth::ne_countries(country = "France", scale = 50) %>%
  st_as_sf() %>%
  st_transform(crs = crs_lambert93)


## pour les limites des régions/départements
regions_FR <-
  rnaturalearth::ne_states(country = "France", returnclass = "sf") %>%
  sf::st_transform(crs = crs_lambert93) %>%
  sf::st_make_valid() %>%
  dplyr::filter(type != "Overseas département")

## la normandie
region_normandie <-
  regions_FR %>%
  dplyr::filter(region == 'Normandie')

## bbox de la normandie pour zoomer
bbox_normandie <-
  region_normandie %>%
  sf::st_buffer(dist = 5000) %>%
  sf::st_bbox()

## données hydro bassins
district_limits <- suppressWarnings(
  capture.output(
    tmp <- tod::wfs_sandre(
      url_wfs = "https://services.sandre.eaufrance.fr/geo/topage?",
      couche = "BassinHydrographique_FXX"
    ),
    file = NULL
  )
)

district_limits <- sf::st_transform(tmp, crs = crs_lambert93)

cours_eau <-
  sf::read_sf(
    "\\\\ad.intra\\dfs\\COMMUNS\\REGIONS\\nor\\DR\\OFB\\SIG\\DR\\REFERENTIEL\\BD CARTHAGE\\NOR_COURS_D_EAU.gpkg"
  ) %>% 
  dplyr::filter(CLASSE <= 4)


## couche SIG des station sur le réseau
stations_all <-
  sf::read_sf(
    "\\\\ad.intra\\dfs\\COMMUNS\\REGIONS\\nor\\DR\\OFB\\SIG\\DR\\IG_METIER\\EAU\\PECHE\\Stations_reseaux.gpkg"
  ) %>%
  sf::st_transform(crs = crs_lambert93) %>%
  dplyr::filter(Etat == "actif") %>%
  dplyr::select(station=`Code Station SANDRE`, nom = `LIBELLE SANDRE`) 


carte_station <- function (id_station){
  
  stations <- stations_all%>% 
    dplyr::filter(station==id_station)
  
  coords <- st_coordinates(stations)
  stations_lab <- cbind(stations, coords)

  
  dept_station <-
    regions_FR[st_intersects(regions_FR, stations, sparse = FALSE), ]

  ## fond de carte
  base_map <-
    ggplot2::ggplot(background) +
    ggplot2::geom_sf(fill = "grey70") +
    ggplot2::geom_sf(
      data = background %>%  dplyr::filter(admin == 'France'),
      fill = 'grey95'
    ) +
    ggplot2::geom_sf(
      data = regions_FR,
      fill = "white",
      size = 0.1,
      col = "black"
    ) +
    ggplot2::geom_sf(
      data = cours_eau,
      fill = NA,
      size = 0.4,
      col = "lightblue"
    ) +
    ggplot2::geom_sf(
      data = region_normandie,
      fill = NA,
      size = 0.5,
      col = "black"
    ) +
    ggplot2::geom_sf(
      data = dept_station,
      fill = NA,
      color = "#CA3C66",
      size = 1
    )+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue")) +
    ggspatial::annotation_north_arrow(
      location = "topright",
      which_north = "true",
      style = ggspatial::north_arrow_fancy_orienteering,
      rotation = 0
    )+
    ggspatial::annotation_scale(
      location = "br",
      width_hint = 0.2
    )
  
  
  map_stations <-
    base_map +
    ggplot2::geom_sf(
      data = stations,
      col = 'darkgreen',
      inherit.aes = F,
      size=4,
      alpha = 0.9,
      shape=19
    ) +  
    ggrepel::geom_label_repel(
      data = stations_lab,
      ggplot2::aes(
        X, Y,
        label = station   
      ),
      size = 4,
      fill="lightgreen",
      color="black",
      min.segment.length = 2,
      segment.color = "black",
      nudge_x = 5000,   
      nudge_y = 5000
    )+
    ggplot2::coord_sf(
      xlim = bbox_normandie[c(1, 3)],
      ylim = bbox_normandie[c(2, 4)],
      expand = F
    )+
    ggplot2::labs(
                  x=NULL,
                  y=NULL)+
    ggplot2::theme(
                   axis.text = element_blank(),     
                   axis.ticks = element_blank()     
    )
  
    return(map_stations)
}