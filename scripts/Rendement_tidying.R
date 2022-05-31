fire%>%
  group_by(groups,type_vegetation)%>%
  summarise(mean(surface_ha),count)->VEGET
#
# write_excel_csv2(VEGET,"Vegetation_type.csv")
library(stringi)
library(janitor)
library(tidyverse)
library(snakecase)

#Get table----------------------------------------------------------------------
Rendement <-
  read.csv(
    "data/Rendement_agri/SAANR_DEVELOPPE_2_Rendement.csv",#Or "data/Rendement_agri/SAANR_FOURRAGE_2_Rendement.csv"
    header = T,
    sep = ';'#,encoding = "UTF-8"
  )
Rendement <- as.tibble(Rendement)
Rendement<-rename(.data = Rendement,"Cultures.développées.1"="X.U.FEFF.Cultures.développées.1")

#Pivot longer Location----------------------------------------------------------
Rendement %>%
  pivot_longer(
    cols = "FR.métro...France.métropolitaine":"X2B...Haute.Corse",
    names_to = "location",
    values_to = "rendement"
  ) -> Rendement

#Clean columns names -----------------------------------------------------------
Rendement %>%
  rename_with(
    .fn = function(x) {
      to_snake_case(x)
    }
  ) %>%
  clean_names() -> Rendement

#Clean location name and culture developpe, and keep original names-------------
Rendement %>%
  mutate(
    location_or = location,
    cultures_developpees_1_or = cultures_developpees_1,
    cultures_developpees_2_or = cultures_developpees_2,
    cultures_developpees_3_or = cultures_developpees_3
  )->Rendement

Rendement %>%
  mutate(across(
    c(
      cultures_developpees_1,
      cultures_developpees_2,
      cultures_developpees_3,
      location
    ),
    ~ stri_trans_general(to_snake_case(.x), id = "Latin-ASCII")
  )) -> Rendement

#Split Location into number and Name--------------------------------------------
Rendement %>%
  mutate(
    departement_number = str_extract(location, str_extract(location, pattern = "[:digit:][:digit:]")),
    departement_number = if_else(
      is.na(departement_number),
      str_extract(location, pattern = "[:digit:][:punct:][:alpha:]"),
      departement_number
    ),
    departement_number = if_else(str_detect(location, "fr[:punct:]"), "00", departement_number),
    departement_number = str_remove(departement_number, "_")
  ) -> Rendement

fwrite(Rendement,"data/Rendement_agri/Annual_Rendement_Departement_DEVELOPPE.csv",sep = ";")
write_delim(Rendement,"data/Rendement_agri/Annual_Rendement_Departement_DEVELOPPE.csv",";") #OR "data/Rendement_agri/Annual_Rendement_Departement_FOURAGE.csv"

test<-read_delim("data/Rendement_agri/Annual_Rendement_Departement_FOURAGE.csv") #OR "data/Rendement_agri/Annual_Rendement_Departement_FOURAGE.csv"

test%>%
  group_by(cultures_developpees_3)%>%
  count()->a2
tab<-rbind(a1,a2)
write_csv2(tab,"KEYWORDS.csv")
