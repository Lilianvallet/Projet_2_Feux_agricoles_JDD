library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(readxl)
library(openxlsx)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(ggalt)
library(ggbeeswarm)

# Diffrents type of database (Promethee, Aquitaine and MidiPyr) ---------------------------------
## Chargement et nettoyage de base de données ----------------------
commune_gps <- read_csv2("data/commune_gps_tidied.csv")
Promethee <-
  read_delim("data/liste_incendies_du_16_12_2021_located.csv",
             delim = ";")
MidiPyr <- read_delim(
  "data/feuxmidpyr3_located.csv",
  delim = ";",
  locale = locale(decimal_mark = ".")
)
Aquitaine <- read_delim("data/feux_aquitaine_GIPATGRI.txt",
                        delim = ";")
Atlas <-
  read_xlsx("data/Atlas/MissingAndPossibleProblems_17Dec.xlsx",
            sheet = 3) %>%
  select(Year:Filename)
Atlas_area <- read_csv("data/Atlas/Atlas_Database.csv")
BDIFF <-
  read_delim(
    "data/export_BDIFF_incendies_20220111_located.csv",
    delim = ";",
    locale = locale(decimal_mark = ".")
  )
clean_names(Promethee) -> Promethee
clean_names(MidiPyr) -> MidiPyr
clean_names(Aquitaine) -> Aquitaine
clean_names(Atlas) -> Atlas
clean_names(Atlas_area) -> Atlas_area
clean_names(BDIFF) -> BDIFF

# Final Merging option : Promethee + Aquitaine +MidiPyr + BDIFF(where and when not in MidiPyr or Aquitaine) =====
# BDIFF %>%
#   mutate(
#     long = as.numeric(long),
#     lat = as.numeric(lat),
#     database_origin = "BDIFF",
#     date=as_date(dmy_hms(date_de_premiere_alerte))
#   ) %>%
#   select(!statut) %>%
#   filter(long < 20)->BDIFF
# anti_join(Aquitaine,BDIFF)
#
# Aquitaine%>%
#   mutate(date=as_date(date))%>%
#   select(date)

##Data Tydying====
Promethee %>%
  rename(burned_area_km2 = surface_parcourue_m2) %>%
  mutate(
    database_origin = "liste_incendies_du_16_12_2021_located",
    burned_area_km2 = burned_area_km2 / 1000000,
    type_de_feu = as.character(type_de_feu),
    date = as_date(alerte)
  ) %>%
  filter(long > 0) -> Promethee_tidy

MidiPyr %>%
  filter(!is.na(annee)) %>%
  rename(
    code_insee = commun_code_insee,
    burned_area_km2 = surtot,
    burned_area_forest_km2 = surfor,
    burned_area_lan_km2 = surlan
  ) %>%
  mutate(
    burned_area_km2 = burned_area_km2/10000,
    burned_area_forest_km2 = burned_area_forest_km2/10000,
    burned_area_lan_km2 = burned_area_lan_km2/10000,
    date = if_else(str_length(datdeb) == 5, paste0(0, datdeb), datdeb),
    date = dmy(date),
    database_origin = "feuxmidpyr3_located",
    long = as.numeric(long),
    numero = as.numeric(numero),
    code_insee = as.character(code_insee),
    origin = as.character(origin)
  ) -> MidiPyr_tidy

Aquitaine %>%
  rename(
    code_insee = commune,
    long = longitude,
    lat = latitude,
    burned_area_km2 = surface_totale_m2,
    date = date_de_premiere_alerte,
    origin = origine,
    burned_area_forest_km2 = surface_foret,
    burned_area_lan_km2 = surfaces_non_boisees_m2
  ) %>%
  mutate(
    departement = as.character(departement),
    code_insee = as.character(code_insee),
    burned_area_km2 = burned_area_km2 / 1000000,
    burned_area_forest_km2 = burned_area_forest_km2 / 1000000,
    burned_area_lan_km2 = burned_area_lan_km2 / 1000000,
    date = as_date(date),
    database_origin = "feux_aquitaine_GIPATGRI"
  ) -> Aquitaine_tidy

BDIFF %>%
  rename(burned_area_km2=surface_brulee_m2,
         burned_area_forest_km2 = surface_foret_m2,
         burned_area_lan_km2 = surfaces_non_boisees_m2)%>%
  mutate(
    burned_area_km2 = burned_area_km2 / 1000000,
    burned_area_forest_km2 = burned_area_forest_km2 / 1000000,
    burned_area_lan_km2 = burned_area_lan_km2 / 1000000,
    long = as.numeric(long),
    lat = as.numeric(lat),
    database_origin = "BDIFF",
    date = dmy_hm(date_de_premiere_alerte),
    date = as_date(date)
  ) %>%
  select(!statut) %>%
  filter(long < 20) -> BDIFF_tidy

Atlas <- left_join(Atlas, Atlas_area, by = "id")
Atlas %>%
  rename(burned_area_largefire_km2 = area_ha) %>%
  mutate(
    burned_area_largefire_km2 = burned_area_largefire_km2 * 10,
    date = as_date(alerte),
    large_fire = "YES"
  ) -> Atlas_tidy


## Merge database====
PromMidiPyr <- bind_rows(Promethee_tidy, MidiPyr_tidy)
Fire_database <- bind_rows(PromMidiPyr, Aquitaine_tidy)
Fire_database <-
  left_join(Fire_database,
            Atlas_tidy,
            by = c("numero", "code_du_carreau_dfci", "date"))
BDIFF_reduced <-
  anti_join(BDIFF_tidy, Fire_database, by = c("code_insee", "date"))
Fire_database <- bind_rows(Fire_database, BDIFF_reduced)

## Map ====
Fire_database %>%
  filter(!is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) -> Fire_database_sf

tmap_mode("plot")
osm_fire_sf <- read_osm(Fire_database_sf, ext = 1.1)
tm_shape(osm_fire_sf) +
  tm_rgb() +
  tm_shape(Fire_database_sf) +
  tm_dots(col = "database_origin",
          palette = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  tm_legend(legend.bg.color = "white") -> map_to_be_saved
tmap_save(map_to_be_saved, "Merged_Fire_Database_version2.png")

Fire_database_sf%>%
  ggplot() +
  aes(col = database_origin) +
  borders("france") +
  geom_sf(cex = 0.1) +
  scale_color_discrete(labels = c("BDIFF", "Aquitaine", "MidiPyr", "Promethée"))->map_to_be_saved
ggsave("output_data/Merged_Fire_Database.jpg",map_to_be_saved)
## Graph ====
Fire_database %>%
  group_by(database_origin) %>%
  summarise(min = min(date), max = max(date)) -> Period
ggplot() +
  geom_dumbbell(data = Period,
                aes(
                  col = database_origin,
                  y = database_origin,
                  x = min,
                  xend = max
                )) +
  geom_violin(data = Fire_database, aes(col = database_origin, y = database_origin, x =
                                          date)) +
  scale_y_discrete(labels = c("BDIFF", "Aquitaine", "MidiPyr", "Promethée")) +
  scale_x_date(name = "Date") +
  theme(legend.position = "none")->graph_to_be_saved
ggsave("output_data/Database_distribution.jpg",graph_to_be_saved)

  ## Output ====
Fire_database %>%
  mutate(centroid_origin=if_else(database_origin=="feux_aquitaine_GIPATGRI","fire","administrative_unit"))%>%
  rename(LONG = long,
         LAT = lat)%>%
  select(
    database_origin,
    centroid_origin,
    date,
    code_insee,
    burned_area_km2,
    burned_area_largefire_km2,
    LONG,
    LAT,
    burned_area_forest_km2,
    burned_area_lan_km2,
    everything()
  ) -> Fire_database
write.table(
  Fire_database,
  "Merged_Fire_Database.csv",
  sep = ";",
  fileEncoding = "UTF-8",
  row.names = F
)

write.xlsx(
  Fire_database,
  "Merged_Fire_Database.xlsx",
  keepNA=T,
  na.string="NA"
)

write_excel_csv2(
  Fire_database,
  "Merged_Fire_Database.csv",
)

Fire_database%>%
  group_by(database_origin)%>%
  count()
# Same type of database (Atlas) -------------------------------------------------------------------
seq_names <- c(
  "AtlasCorse",
  "AtlasDept66",
  "Dept4Atlas31",
  "Dept4Atlas32",
  "Dept5Atlas",
  "Dept6Atlas",
  "Dept7Atlas",
  "Dept11Atlas",
  "Dept26Atlas",
  "Dept30Atlas",
  "Dept48Atlas",
  "Dept83Atlas31",
  "Dept83Atlas32",
  "Dept84Atlas"
)
seq_departement <-
  c("2A2B",
    "66",
    "4",
    "4",
    "5",
    "6",
    "7",
    "11",
    "26",
    "30",
    "48",
    "83",
    "83",
    "84")
Atlas_Database <- list()
for (i in 1:14) {
  name <- seq_names[i]
  data <- st_read(paste0("data/Atlas/", name, ".shp"))
  Atlas_Database[[i]] <- st_transform(data, crs = 4326)
  Atlas_Database[[i]]["departement"] <- seq_departement[i]
}
Atlas_database <- bind_rows(Atlas_Database)
plot(Atlas_database$geometry)
tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(Atlas_database) +
  tm_polygons("AreaHa",
              lwd = 0,
              palette = get_brewer_pal("OrRd", n = 15, contrast = c(0.48, 1))) -> map
tmap_save(map, "output_data/Atlas_Database.html")
st_write(Atlas_database, "data/Atlas/Atlas_Database.shp")

tab <- as_tibble(Atlas_database)
write.xlsx2(tab, "Atlas_Database.xlsx",)
st_write(Atlas_database, "data/Atlas/Atlas_Database.csv")




data <- read_csv2("Merged_Fire_Database.csv")
data %>% count(numero)
plot(data$numero)