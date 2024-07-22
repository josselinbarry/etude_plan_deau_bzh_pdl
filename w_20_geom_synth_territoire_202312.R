# VERSION FINALISEE AU 20231207
## En cours de création

## CREER LES COUCHES ME - SAGE - COMMUNE - BV IPR par agglomérations des données issues de la couche PE

# Library ----
#library(plyr)
library(tidyverse)
# library(terra)
# library(lubridate)
# library(RcppRoll)
# library(DT)
# library(readxl)
# library(dbplyr)
# library(RPostgreSQL)
# library(rsdmx)
library(sf)
#library(stringi)
library(units)

source(file = "R/compter_sommer_simple_surfaces_dans_polygone.R")
source(file = "R/compter_sommer_surfaces_dans_polygone.R")
source(file = "R/sommer_volume_points_ds_polygone.R")


# Import des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20240220.gpkg")%>%
  st_transform(crs = 2154)

interstect_test <- sf::read_sf(dsn = "data/testA.gpkg")

sages <- sf::read_sf(dsn = "data/outputs/sages_20231202.gpkg") %>%
  st_transform(crs = 2154)

  mutate(hors_prelevement = hors_prelevements) 

bv_me_decoup <- sf::read_sf(dsn = "data/outputs/bv_me_qualifies_20240216.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_me = st_area(geom)) 
  
communes <- sf::read_sf(dsn = "data/outputs/communes_qualifiees_20240216.gpkg") %>%
  st_transform(crs = 2154)

  mutate(surface_com = st_area(geom)) %>%
  select(code_insee,nom_officiel, code_insee_du_departement, code_insee_de_la_region, surface_com) %>%
  rename(nom_com = nom_officiel,
         insee_dep = code_insee_du_departement,
         insee_reg = code_insee_de_la_region)

ce_topage <- sf::read_sf(dsn = "data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.gpkg") %>% 
  rename(cdoh_ce = CdOH) %>%
  select(cdoh_ce, StreamOrde) %>%
  st_transform(crs = 2154)

cehm_topage <- sf::read_sf(dsn = "data/TronconHydrographique_hors_marais_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.gpkg") %>% 
  rename(cdoh_ce = CdOH) %>%
  select(cdoh_ce, StreamOrde) %>%
  st_transform(crs = 2154)

bv_ipr <- sf::read_sf(dsn = "data/outputs/bv_ipr_20240625.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_ipr = st_area(geom)) %>%
  mutate(sta_id = id)

lineaire_topage_pe <- sf::read_sf(dsn = "data/outputs/lineaires_topage_pe.gpkg")

ce_topage_me <- sf::read_sf(dsn = "data/outputs/intersection_topage_me_20231128.gpkg")

ce_topage_ipr <- sf::read_sf(dsn = "data/outputs/ce_topage_ipr_20240625.gpkg")

ce_topage_sage <- sf::read_sf(dsn = "data/outputs/intersection_topage_sage_20231128.gpkg")

ce_topage_com <- sf::read_sf(dsn = "data/outputs/intersection_topage_commune_20231128.gpkg")

pe_decoup_me <- sf::read_sf(dsn = "data/outputs/pe_decoup_me_20240215.gpkg")
pe_decoup_com <- sf::read_sf(dsn = "data/outputs/pe_decoup_com_20240216.gpkg")

qa_me <- sf::read_sf(dsn = "data/outputs/qa_me_20231200.gpkg")
qa_sage <- sf::read_sf(dsn = "data/outputs/qa_sage_20231200.gpkg")
qa_com <- sf::read_sf(dsn = "data/outputs/qa_com_20231200.gpkg")
qa_ipr <- sf::read_sf(dsn = "data/outputs/qa_ipr_20231200.gpkg")
q5_me <- sf::read_sf(dsn = "data/outputs/q5_me_20231200.gpkg")
q5_sage <- sf::read_sf(dsn = "data/outputs/q5_sage_20231200.gpkg")
q5_com <- sf::read_sf(dsn = "data/outputs/q5_com_20231200.gpkg")
q5_ipr <- sf::read_sf(dsn = "data/outputs/q5_ipr_20231200.gpkg")

qa <- sf::read_sf(dsn = "data/qa_zone_etude.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN) %>%
  st_transform(crs = 2154)

q5 <- sf::read_sf(dsn = "data/q5_zone_etude.gpkg") %>%
  select(ID_BDCARTH, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>%
  st_transform(crs = 2154)

departements <- sf::read_sf(dsn = "data/DEPARTEMENT.shp") %>%
  st_transform(crs = 2154)

regions <- sf::read_sf(dsn = "data/regions.gpkg") %>%
  select(insee_reg, nom_region) %>%
  st_transform(crs = 2154)

zhp24 <- sf::read_sf(dsn = "data/zhp_24_vect.shp") %>%
  st_transform(crs = 2154)

zhp28 <- sf::read_sf(dsn = "data/zhp_28_vect.shp") %>%
  st_transform(crs = 2154)

zhp52 <- sf::read_sf(dsn = "data/zhp_52_vect.shp") %>%
  st_transform(crs = 2154)

zhp53 <- sf::read_sf(dsn = "data/zhp_53_vect.shp") %>%
  st_transform(crs = 2154)

zhp75 <- sf::read_sf(dsn = "data/zhp_75_vect.shp") %>%
  st_transform(crs = 2154)

zhp24_hm <- sf::read_sf(dsn = "data/zhp_hm_24_vect.shp") %>%
  st_transform(crs = 2154)

zhp28_hm <- sf::read_sf(dsn = "data/zhp_hm_28_vect.shp") %>%
  st_transform(crs = 2154)

zhp52_hm <- sf::read_sf(dsn = "data/zhp_hm_52_vect.shp") %>%
  st_transform(crs = 2154)

zhphm_53 <- sf::read_sf(dsn = "data/zhp_hm_53_vect.shp") %>%
  st_transform(crs = 2154)

zhphm_75 <- sf::read_sf(dsn = "data/zhp_hm_75_vect.shp") %>%
  st_transform(crs = 2154)

marais <- sf::read_sf(dsn = "data/Marais.shp") %>%
  st_transform(crs = 2154)


# Calcul des linéaires topages et strahler max ----

## Calcul des linéaires topages et strahler max par me ----

ce_topage_me <- ce_topage %>% 
  st_intersection(bv_me_decoup) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) # longueur des intersects

sf::write_sf(obj = ce_topage_me, dsn = "data/outputs/intersection_topage_me_20231128.gpkg")

ce_decoup_me <- ce_topage_me %>%
  st_drop_geometry() %>%
  select(cdoh_ce, cdeumassed, longueur_intersect, StreamOrde) %>%
  group_by(cdeumassed) %>%
  summarise(strahler_max = max (StreamOrde),
            longueur_ce_topage = sum(longueur_intersect)) %>%
  select(cdeumassed, strahler_max,longueur_ce_topage)

ce_tdbv_decoup_me <- ce_topage_me %>% 
  st_drop_geometry() %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  select(cdoh_ce, cdeumassed, longueur_intersect) %>%
  group_by(cdeumassed) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_ce_tdbv_topage)

cehm_topage_me <- cehm_topage %>% 
  st_intersection(bv_me_decoup) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) %>% # longueur des intersects
  st_drop_geometry()
  
sf::write_sf(obj = cehm_topage_me, dsn = "data/outputs/intersection_topage_hm_me_20240220.gpkg")

long_cehm_me <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_me %>% 
      units::drop_units(),
    var_id_polygone = cdeumassed,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm,
    var_somme_surfaces = longueur_cehm_topage) %>%
  select(-nb_cehm)

long_cehm_tdbv_me <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_me %>% 
      filter(StreamOrde== 1 | StreamOrde == 2) %>%
      units::drop_units(),
    var_id_polygone = cdeumassed,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm_tdbv,
    var_somme_surfaces = longueur_cehm_tdbv_topage) %>%
  select(-nb_cehm_tdbv)

## Calcul des linéaires topages et strahler max par sage ----

ce_topage_sage <- ce_topage %>% 
  st_intersection(sages) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.))  # longueur des intersects

sf::write_sf(obj = ce_topage_sage, dsn = "data/outputs/intersection_topage_sage_20231128.gpkg")

ce_decoup_sage <- ce_topage_sage %>%
  st_drop_geometry() %>%
  select(cdoh_ce, nom_sage, longueur_intersect, StreamOrde) %>%
  group_by(nom_sage) %>%
  summarise(strahler_max = max (StreamOrde),
            longueur_ce_topage = sum(longueur_intersect)) %>%
  select(nom_sage, strahler_max, longueur_ce_topage)

ce_tdbv_decoup_sage <- ce_topage_sage %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  st_drop_geometry() %>%
  select(cdoh_ce, nom_sage, longueur_intersect) %>%
  group_by(nom_sage) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_ce_tdbv_topage)

## Calcul des linéaires topages et strahler max par commune ----

ce_topage_com <- ce_topage %>% 
  st_intersection(communes) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) # longueur des intersects

sf::write_sf(obj = ce_topage_com, dsn = "data/outputs/intersection_topage_commune_20231128.gpkg")

ce_decoup_com <- ce_topage_com %>%
  st_drop_geometry() %>%
  select(cdoh_ce, code_insee, longueur_intersect, StreamOrde) %>%
  group_by(code_insee) %>%
  summarise(strahler_max = max (StreamOrde),
            longueur_ce_topage = sum(longueur_intersect)) %>%
  select(code_insee, strahler_max, longueur_ce_topage)

ce_tdbv_decoup_com <- ce_topage_com %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  st_drop_geometry() %>%
  select(cdoh_ce, code_insee, longueur_intersect) %>%
  group_by(code_insee) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(code_insee, longueur_ce_tdbv_topage)

cehm_topage_com <- cehm_topage %>% 
  st_intersection(communes) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) %>% # longueur des intersects
  st_drop_geometry()

sf::write_sf(obj = cehm_topage_com, dsn = "data/outputs/intersection_topage_hm_com_20240220.gpkg")

long_cehm_com <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_com %>% 
      units::drop_units(),
    var_id_polygone = code_insee,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm,
    var_somme_surfaces = longueur_cehm_topage) %>%
  select(-nb_cehm)

long_cehm_tdbv_com <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_com %>% 
      filter(StreamOrde== 1 | StreamOrde == 2) %>%
      units::drop_units(),
    var_id_polygone = code_insee,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm_tdbv,
    var_somme_surfaces = longueur_cehm_tdbv_topage) %>%
  select(-nb_cehm_tdbv)


## Calcul des linéaires topages et strahler max par bv ipr ----

ce_topage_ipr <- ce_topage %>% 
  st_intersection(bv_ipr) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) 

sf::write_sf(obj = ce_topage_ipr, dsn = "data/outputs/ce_topage_ipr_20240625.gpkg")

ce_topage_ipr <- ce_topage_ipr %>%
  mutate(sta_id = id) %>%
  units::drop_units()

cehm_topage_ipr <- cehm_topage_ipr %>%
  mutate(sta_id = id) %>%
  units::drop_units()

ce_decoup_ipr <- ce_topage_ipr %>%
  select(cdoh_ce, sta_id, longueur_intersect, StreamOrde) %>%
  group_by(sta_id) %>%
  summarise(strahler_max = max (StreamOrde),
            longueur_ce_topage = sum(longueur_intersect)) %>%
  select(sta_id, strahler_max, longueur_ce_topage)

ce_tdbv_decoup_ipr <- ce_topage_ipr %>%
  filter(StreamOrde == 1 | StreamOrde == 2) %>%
  select(cdoh_ce, sta_id, longueur_intersect) %>%
  group_by(sta_id) %>%
  summarise(longueur_ce_tdbv_topage = sum(longueur_intersect)) %>%
  select(sta_id, longueur_ce_tdbv_topage)

cehm_topage_ipr <- cehm_topage %>% 
  st_intersection(bv_ipr) %>% # découpage des ce selon les masses d'eau
  mutate(longueur_intersect = st_length(.)) %>% # longueur des intersects
  st_drop_geometry()

long_cehm_ipr <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_ipr %>% 
      units::drop_units(),
    var_id_polygone = sta_id,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm,
    var_somme_surfaces = longueur_cehm_topage) %>%
  select(-nb_cehm)

long_cehm_tdbv_ipr <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = cehm_topage_ipr %>% 
      filter(StreamOrde== 1 | StreamOrde == 2) %>%
      units::drop_units(),
    var_id_polygone = sta_id,
    var_a_sommer = longueur_intersect,
    var_nb_objets = nb_cehm_tdbv,
    var_somme_surfaces = longueur_cehm_tdbv_topage) %>%
  select(-nb_cehm_tdbv)

## Jointure des linéaires topages par objet ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(ce_decoup_me) %>%
  dplyr::left_join(ce_tdbv_decoup_me) %>%
  dplyr::left_join(long_cehm_me) %>%
  dplyr::left_join(long_cehm_tdbv_me)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240225.gpkg")

sages <- sages %>% 
  dplyr::left_join(ce_decoup_sage) %>%
  dplyr::left_join(ce_tdbv_decoup_sage) 

communes <- communes %>%
  dplyr::left_join(ce_decoup_com) %>%
  dplyr::left_join(ce_tdbv_decoup_com) %>%
  dplyr::left_join(long_cehm_com) %>%
  dplyr::left_join(long_cehm_tdbv_com)

sf::write_sf(obj = communes, dsn = "data/outputs/communes_qualifiees_20240225.gpkg")

bv_ipr <- bv_ipr %>%
  dplyr::left_join(ce_decoup_ipr %>%
                     st_drop_geometry()) %>%
  dplyr::left_join(ce_tdbv_decoup_ipr %>%
                     st_drop_geometry()) %>%
  dplyr::left_join(long_cehm_ipr %>%
                     st_drop_geometry()) %>%
  dplyr::left_join(long_cehm_tdbv_ipr%>%
                     st_drop_geometry())

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_stat_ipr_20240625.gpkg")

# Calcul des linéaires topages intersectés ----

## Calcul des linéaires topages intersectés par me ----

lineaire_topage_pe_me <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_me_decoup) %>% 
  mutate(longueur_intersect = st_length(.))  

lineaire_topage_pe_tot_me <- lineaire_topage_pe_me %>% 
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pehm_tdbv_tot_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0 & StreamOrde < 3) %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_tdbv_tot)

lineaire_topage_pe_perm_me <- lineaire_topage_pe_me %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_perm)

lineaire_topage_pehm_tdbv_perm_me <- lineaire_topage_pe_me %>% 
  filter(zone_marais == 0 & Persistanc == "permanent" & StreamOrde < 3) %>%  
  st_drop_geometry() %>%
  group_by(cdeumassed) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_intersect)) %>%
  select(cdeumassed, longueur_topage_intersecte_pehm_tdbv_perm)

## Jointure des linéaires topages intersectés par me ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(lineaire_topage_pe_tot_me) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_me) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_tot_me) %>%
  dplyr::left_join(lineaire_topage_pe_perm_me) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_me) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_perm_me)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240216.gpkg")

## Calcul des linéaires topages intersectés par sage ----

lineaire_topage_pe_sage <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(sages) %>% 
  mutate(longueur_intersect = st_length(.)) 

lineaire_topage_pe_tot_sage <- lineaire_topage_pe_sage %>% 
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pehm_tdbv_tot_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0 & StreamOrde < 3) %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_tdbv_tot)

lineaire_topage_pe_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_perm)

lineaire_topage_pehm_tdbv_perm_sage <- lineaire_topage_pe_sage %>% 
  filter(zone_marais == 0 & Persistanc == "permanent" & StreamOrde <3) %>%  
  st_drop_geometry() %>%
  group_by(nom_sage) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_intersect)) %>%
  select(nom_sage, longueur_topage_intersecte_pehm_tdbv_perm)

## Jointure des linéaires topages intersectés par sage ----

sages <- sages %>%
  dplyr::left_join(lineaire_topage_pe_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_tot_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pe_perm_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_sage, join_by(nom_sage == nom_sage)) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_perm_sage, join_by(nom_sage == nom_sage))

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231201.gpkg")

## Calcul des linéaires topages intersectés par commune ----

lineaire_topage_pe_com <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(communes) %>% 
  mutate(longueur_intersect = st_length(.)) 

lineaire_topage_pe_tot_com <- lineaire_topage_pe_com %>% 
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pehm_tdbv_tot_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0 & StreamOrde <3) %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_tdbv_tot)

lineaire_topage_pe_perm_com <- lineaire_topage_pe_com %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_perm)

lineaire_topage_pehm_tdbv_perm_com <- lineaire_topage_pe_com %>% 
  filter(zone_marais == 0 & Persistanc == "permanent" & StreamOrde <3) %>%  
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_intersect)) %>%
  select(code_insee, longueur_topage_intersecte_pehm_tdbv_perm)

## Jointure des linéaires topages intersectés par commune ----

communes <- communes %>%
  dplyr::left_join(lineaire_topage_pe_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_tot_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pe_perm_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_com, join_by(code_insee == code_insee)) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_perm_com, join_by(code_insee == code_insee))

sf::write_sf(obj = communes, dsn = "data/outputs/communes_qualifiees_20240216.gpkg")

## Calcul des linéaires topages intersectés par bv ipr ----

lineaire_topage_pe_ipr <- lineaire_topage_pe %>% 
  filter(mare == 0) %>%
  st_intersection(bv_ipr) %>% 
  mutate(longueur_intersect = st_length(.)) 

lineaire_topage_pe_tot_ipr <- lineaire_topage_pe_ipr %>% 
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pe_tot = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pe_tot)

lineaire_topage_pehm_tot_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0) %>%  
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pehm_tot = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pehm_tot)

lineaire_topage_pehm_tdbv_tot_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0 & StreamOrde <3) %>%  
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pehm_tdbv_tot)

lineaire_topage_pe_perm_ipr <- lineaire_topage_pe_ipr %>% 
  filter(Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pe_perm = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pe_perm)

lineaire_topage_pehm_perm_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0 & Persistanc == "permanent") %>%  
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pehm_perm = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pehm_perm)

lineaire_topage_pehm_tdbv_perm_ipr <- lineaire_topage_pe_ipr %>% 
  filter(zone_marais == 0 & Persistanc == "permanent" & StreamOrde <3) %>%  
  st_drop_geometry() %>%
  group_by(sta_id) %>%
  summarise(longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_intersect)) %>%
  select(sta_id, longueur_topage_intersecte_pehm_tdbv_perm)

## Jointure des linéaires topages intersectés par bv ipr ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(lineaire_topage_pe_tot_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_tot_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_tot_ipr) %>%
  dplyr::left_join(lineaire_topage_pe_perm_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_perm_ipr) %>%
  dplyr::left_join(lineaire_topage_pehm_tdbv_perm_ipr)

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Calcul des surfaces cumulées de ZHP ----

## Calcul des surfaces cumulées de ZHP par BV ME ----

zhp_tot <-
  dplyr::bind_rows(
    zhp24, zhp28, zhp53, zhp52, zhp75)

zhp_decoup_me <- zhp_tot %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) #%>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = zhp_decoup_me, dsn = "data/outputs/zhp_decoup_me_20240220.gpkg")

surf_zhp_me <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhp_decoup_me %>% 
      units::drop_units(),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhp,
    var_somme_surfaces = surf_zhp) %>%
  select(-nb_zhp)

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_zhp_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20240117.gpkg")

## Calcul des surfaces cumulées de ZHP hors marais par me ---- 

zhphm_tot <-
  dplyr::bind_rows(
    zhp24_hm, zhp28_hm, zhphm_53, zhp52_hm, zhphm_75)

sf::write_sf(obj = zhphm_tot, dsn = "data/outputs/zhphm_tot_20240625.gpkg")

zhphm_decoup_me <- zhphm_tot %>% 
  st_intersection(bv_me_decoup) %>% 
  mutate(surface_intersect = st_area(.)) 

sf::write_sf(obj = zhphm_decoup_me, dsn = "data/outputs/zhphm_decoup_me_20240220.gpkg")

surf_zhphm_me <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhphm_decoup_me %>% 
      units::drop_units(),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhphm,
    var_somme_surfaces = surf_zhphm) %>%
  select(-nb_zhphm)

surf_zhphm_me <- surf_zhphm_me %>%
  st_drop_geometry()

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_zhphm_me, join_by(cdeumassed == cdeumassed)) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240220.gpkg")

## Calcul des surfaces cumulées de ZHP par commune ----

zhp_decoup_com <- zhp_tot %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) #%>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = zhp_decoup_com, dsn = "data/outputs/zhp_decoup_com_20240220.gpkg")

surf_zhp_com <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhp_decoup_com %>% 
      units::drop_units(),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhp,
    var_somme_surfaces = surf_zhp) %>%
  select(-nb_zhp)



communes <- communes %>%
  dplyr::left_join(surf_zhp_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20240117.gpkg")

## Calcul des surfaces cumulées de ZHP hors marais par commune ----

zhphm_decoup_com <- zhphm_tot %>% 
  st_intersection(communes) %>% 
  mutate(surface_intersect = st_area(.)) %>%
  st_drop_geometry()

sf::write_sf(obj = zhphm_decoup_com, dsn = "data/outputs/zhphm_decoup_com_20240220.gpkg")

surf_zhphm_com <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhphm_decoup_com %>% 
      units::drop_units(),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhphm,
    var_somme_surfaces = surf_zhphm) %>%
  select(-nb_zhphm) %>%
  st_drop_geometry()

communes <- communes %>%
  dplyr::left_join(surf_zhphm_com, join_by(code_insee == code_insee)) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_qualifiees_20240220.gpkg")

## Calcul des surfaces cumulées de ZHP par SAGE ----

zhp_decoup_sage <- zhp_tot %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = zhp_decoup_sage, dsn = "data/outputs/zhp_decoup_sage_20240117.gpkg")

surf_zhp_sage <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhp_decoup_sage %>% 
      units::drop_units(),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhp,
    var_somme_surfaces = surf_zhp) %>%
  select(-nb_zhp)

sages <- sages %>%
  dplyr::left_join(surf_zhp_sage) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20240117.gpkg")

## Calcul des surfaces cumulées de ZHP par BV IPR ----

zhp_decoup_ipr <- zhp_tot %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = zhp_decoup_ipr, dsn = "data/outputs/zhp_decoup_ipr_20240605.gpkg")

surf_zhp_ipr <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhp_decoup_ipr %>% 
      units::drop_units(),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhp,
    var_somme_surfaces = surf_zhp) %>%
  select(-nb_zhp)

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_zhp_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240605.gpkg")

## Calcul des surfaces cumulées de ZHP hors marais par BV IPR ----

zhphm_decoup_ipr <- zhphm_tot %>% 
  st_intersection(bv_ipr) %>% 
  mutate(surface_intersect = st_area(.)) %>%
  st_drop_geometry()

#ICI

sf::write_sf(obj = zhphm_decoup_ipr, dsn = "data/outputs/zhphm_decoup_ipr_20240605.gpkg")

surf_zhphm_ipr <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = zhphm_decoup_ipr %>% 
      units::drop_units(),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_zhphm,
    var_somme_surfaces = surf_zhphm) %>%
  select(-nb_zhphm) %>%
  st_drop_geometry()

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_zhphm_ipr, join_by(sta_id == sta_id)) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240605.gpkg")


# Calcul des surfaces de zones de marais ----

## Calcul des surfaces cumulées zone de marais par BV ME ----

marais_decoup_me <- marais %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = marais_decoup_me, dsn = "data/outputs/marais_decoup_me_20240110.gpkg")

surf_marais_me <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = marais_decoup_me %>% 
      units::drop_units(),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_marais,
    var_somme_surfaces = surf_marais) %>%
  select(-nb_marais)

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_marais_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_surf_marais.gpkg")

## Calcul des surfaces cumulées de marais par commune ----

marais_decoup_com <- marais %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = marais_decoup_com, dsn = "data/outputs/marais_decoup_com_20240117.gpkg")

surf_marais_com <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = marais_decoup_com %>% 
      units::drop_units(),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_marais,
    var_somme_surfaces = surf_marais) %>%
  select(-nb_marais)

communes <- communes %>%
  dplyr::left_join(surf_marais_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_surf_marais.gpkg")

## Calcul des surfaces cumulées de marais par bv ipr ----

marais_decoup_ipr <- marais %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = marais_decoup_ipr, dsn = "data/outputs/marais_decoup_ipr_20240625.gpkg")

surf_marais_ipr <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = marais_decoup_ipr %>% 
      units::drop_units(),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_marais,
    var_somme_surfaces = surf_marais) %>%
  select(-nb_marais)

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_marais_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Décompte et calcul des surfaces cumulées de PE et de mares ----

## Décompte et calcul des surfaces cumulées de PE par BV ME ----

pe_decoup_me <- pe %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/pe_decoup_me_20240215.gpkg")

surf_pe_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )
  
surf_pehm_tdbv_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )
  
surf_pehm_connecte_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_cours_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_source_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0 & connecte_source == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_source_tot,
    var_somme_surfaces = surf_pehm_sur_source_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_source_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 0 & connecte_source == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_source_perm,
    var_somme_surfaces = surf_pehm_sur_source_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de PE par BV ME ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_pe_tot_me) %>%
  dplyr::left_join(surf_pehm_tot_me) %>%
  dplyr::left_join(surf_pe_perm_me) %>%
  dplyr::left_join(surf_pehm_perm_me) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_me) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_me) %>%
  dplyr::left_join(surf_pehm_connecte_tot_me) %>%
  dplyr::left_join(surf_pehm_connecte_perm_me) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_me) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_me) %>%
  dplyr::left_join(surf_pehm_sur_source_tot_me) %>%
  dplyr::left_join(surf_pehm_sur_source_perm_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifie_20240715.gpkg")

## Décompte et calcul des surfaces cumulées de mares par BV ME ----

pe_decoup_me <- pe %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/pe_decoup_me_20231200.gpkg")

surf_mares_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_tot,
    var_somme_surfaces = surf_mares_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_tot,
    var_somme_surfaces = surf_mareshm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm,
    var_somme_surfaces = surf_mares_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm,
    var_somme_surfaces = surf_mareshm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE)

## Jointure des surfaces cumulées de mares par BV ME ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_mares_tot_me) %>%
  dplyr::left_join(surf_mareshm_tot_me) %>%
  dplyr::left_join(surf_mares_perm_me) %>%
  dplyr::left_join(surf_mareshm_perm_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240216.gpkg")

## Décompte et calcul des surfaces cumulées de PE par sage ----

pe_decoup_sage <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/pe_decoup_sage_20231200.gpkg")

surf_pe_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_cours_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par SAGE ----

sages <- sages %>%
  dplyr::left_join(surf_pe_tot_sage) %>%
  dplyr::left_join(surf_pehm_tot_sage) %>%
  dplyr::left_join(surf_pe_perm_sage) %>%
  dplyr::left_join(surf_pehm_perm_sage) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_sage) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_sage) %>%
  dplyr::left_join(surf_pehm_connecte_tot_sage) %>%
  dplyr::left_join(surf_pehm_connecte_perm_sage) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_sage) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_sage) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231201.gpkg")

## Décompte et calcul des surfaces cumulées de mares par sage ----

pe_decoup_sage <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/pe_decoup_sage_20231200.gpkg")

surf_mares_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_tot,
    var_somme_surfaces = surf_mares_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_tot,
    var_somme_surfaces = surf_mareshm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm,
    var_somme_surfaces = surf_mares_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm,
    var_somme_surfaces = surf_mareshm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de mares par sage ----

sages <- sages %>%
  dplyr::left_join(surf_mares_tot_sage) %>%
  dplyr::left_join(surf_mareshm_tot_sage) %>%
  dplyr::left_join(surf_mares_perm_sage) %>%
  dplyr::left_join(surf_mareshm_perm_sage) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231202.gpkg")

## Décompte et calcul des surfaces cumulées de PE par commune ----

pe_decoup_com <- pe %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/pe_decoup_com_20240216.gpkg")

surf_pe_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_cours_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

surf_pehm_sur_source_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0 & connecte_source == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_source_tot,
    var_somme_surfaces = surf_pehm_sur_source_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_source_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 0 & connecte_source == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_source_perm,
    var_somme_surfaces = surf_pehm_sur_source_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )


## Jointure des surfaces cumulées de PE par communes ----

communes <- communes %>%
  dplyr::left_join(surf_pe_tot_com) %>%
  dplyr::left_join(surf_pehm_tot_com) %>%
  dplyr::left_join(surf_pe_perm_com) %>%
  dplyr::left_join(surf_pehm_perm_com) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_com) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_com) %>%
  dplyr::left_join(surf_pehm_connecte_tot_com) %>%
  dplyr::left_join(surf_pehm_connecte_perm_com) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_com) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_com) %>%
  dplyr::left_join(surf_pehm_sur_source_tot_com) %>%
  dplyr::left_join(surf_pehm_sur_source_perm_com)%>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20240715.gpkg")

## Décompte et calcul des surfaces cumulées de mares par communes ----

pe_decoup_com <- pe %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/pe_decoup_com_20231200.gpkg")

surf_mares_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_tot,
    var_somme_surfaces = surf_mares_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_tot,
    var_somme_surfaces = surf_mareshm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm,
    var_somme_surfaces = surf_mares_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm,
    var_somme_surfaces = surf_mareshm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de PE par communes ----

communes <- communes %>%
  dplyr::left_join(surf_mares_tot_com) %>%
  dplyr::left_join(surf_mareshm_tot_com) %>%
  dplyr::left_join(surf_mares_perm_com) %>%
  dplyr::left_join(surf_mareshm_perm_com) %>%
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20240216.gpkg")


## Décompte et calcul des surfaces cumulées de PE par BV IPR ----

pe_decoup_ipr <- pe %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_ipr, dsn = "data/outputs/pe_decoup_ipr_20240610.gpkg")

surf_pe_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm,
    var_somme_surfaces = surf_pe_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm,
    var_somme_surfaces = surf_pehm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_tot,
    var_somme_surfaces = surf_pehm_tdbv_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_tdbv_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_tdbv_perm,
    var_somme_surfaces = surf_pehm_tdbv_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = TRUE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_tot,
    var_somme_surfaces = surf_pehm_connecte_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_connecte_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_connecte_perm,
    var_somme_surfaces = surf_pehm_connecte_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = TRUE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_sur_cours_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_tot,
    var_somme_surfaces = surf_pehm_sur_cours_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )
surf_pehm_sur_cours_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 0),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_sur_cours_perm,
    var_somme_surfaces = surf_pehm_sur_cours_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = TRUE
  )

## Jointure des surfaces cumulées de PE par BV IPR ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_pe_tot_ipr) %>%
  dplyr::left_join(surf_pehm_tot_ipr) %>%
  dplyr::left_join(surf_pe_perm_ipr) %>%
  dplyr::left_join(surf_pehm_perm_ipr) %>%
  dplyr::left_join(surf_pehm_tdbv_tot_ipr) %>%
  dplyr::left_join(surf_pehm_tdbv_perm_ipr) %>%
  dplyr::left_join(surf_pehm_connecte_tot_ipr) %>%
  dplyr::left_join(surf_pehm_connecte_perm_ipr) %>%
  dplyr::left_join(surf_pehm_sur_cours_tot_ipr) %>%
  dplyr::left_join(surf_pehm_sur_cours_perm_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

## Décompte et calcul des surfaces cumulées de mares par BV IPR ----

pe_decoup_ipr <- pe %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_ipr, dsn = "data/outputs/pe_decoup_ipr_20231200.gpkg")

surf_mares_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_tot,
    var_somme_surfaces = surf_mares_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_tot,
    var_somme_surfaces = surf_mareshm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm,
    var_somme_surfaces = surf_mares_perm,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% units::drop_units() %>% filter(mare == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm,
    var_somme_surfaces = surf_mareshm_perm,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de mares par BV IPR ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_mares_tot_ipr) %>%
  dplyr::left_join(surf_mareshm_tot_ipr) %>%
  dplyr::left_join(surf_mares_perm_ipr) %>%
  dplyr::left_join(surf_mareshm_perm_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Ajout du décompte et calcul des surfaces cumulées de PE et mares en ZHP ----

## Ajout du décompte et calcul des surfaces cumulées de PE en ZHP par BV ME ----

pe_decoup_me <- pe %>% 
  st_intersection(bv_me_decoup) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_me, dsn = "data/outputs/pe_decoup_ipr_20231200.gpkg")

surf_pe_zhp_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_zhp_tot,
    var_somme_surfaces = surf_pe_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_zhp_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_zhp_tot,
    var_somme_surfaces = surf_pehm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_zhp_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm_zhp,
    var_somme_surfaces = surf_pe_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_zhp_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm_zhp,
    var_somme_surfaces = surf_pehm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Ajout du décompte et calcul des surfaces cumulées de mares en ZHP par BV ME ----

surf_mares_zhp_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_zhp_tot,
    var_somme_surfaces = surf_mares_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_zhp_tot_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_zhp_tot,
    var_somme_surfaces = surf_mareshm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_zhp_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm_zhp,
    var_somme_surfaces = surf_mares_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_zhp_me <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_me %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = cdeumassed,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm_zhp,
    var_somme_surfaces = surf_mareshm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées ZHP par BV ME ----

bv_me_decoup <- bv_me_decoup %>%
  dplyr::left_join(surf_pe_zhp_tot_me) %>%
  dplyr::left_join(surf_pehm_zhp_tot_me) %>%
  dplyr::left_join(surf_pe_perm_zhp_me) %>%
  dplyr::left_join(surf_pehm_perm_zhp_me) %>%
  dplyr::left_join(surf_mares_zhp_tot_me) %>%
  dplyr::left_join(surf_mareshm_zhp_tot_me) %>%
  dplyr::left_join(surf_mares_perm_zhp_me) %>%
  dplyr::left_join(surf_mareshm_perm_zhp_me) %>%
  units::drop_units()

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240216.gpkg")

## Ajout du décompte et calcul des surfaces cumulées de PE en ZHP par COMMUNE ----

pe_decoup_com <- pe %>% 
  st_intersection(communes) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_com, dsn = "data/outputs/pe_decoup_com_20231200.gpkg")

surf_pe_zhp_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_zhp_tot,
    var_somme_surfaces = surf_pe_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_zhp_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_zhp_tot,
    var_somme_surfaces = surf_pehm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_zhp_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm_zhp,
    var_somme_surfaces = surf_pe_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_zhp_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm_zhp,
    var_somme_surfaces = surf_pehm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Ajout du décompte et calcul des surfaces cumulées de mares en ZHP par COMMUNE ----

surf_mares_zhp_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_zhp_tot,
    var_somme_surfaces = surf_mares_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_zhp_tot_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_zhp_tot,
    var_somme_surfaces = surf_mareshm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_zhp_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm_zhp,
    var_somme_surfaces = surf_mares_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_zhp_com <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_com %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = code_insee,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm_zhp,
    var_somme_surfaces = surf_mareshm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées ZHP par COMMUNE ----

communes <- communes %>%
  dplyr::left_join(surf_pe_zhp_tot_com) %>%
  dplyr::left_join(surf_pehm_zhp_tot_com) %>%
  dplyr::left_join(surf_pe_perm_zhp_com) %>%
  dplyr::left_join(surf_pehm_perm_zhp_com) %>%
  dplyr::left_join(surf_mares_zhp_tot_com) %>%
  dplyr::left_join(surf_mareshm_zhp_tot_com) %>%
  dplyr::left_join(surf_mares_perm_zhp_com) %>%
  dplyr::left_join(surf_mareshm_perm_zhp_com) %>%  
  units::drop_units()

sf::write_sf(obj = communes, dsn = "data/outputs/communes_qualifiees_20240216.gpkg")

## Ajout du décompte et calcul des surfaces cumulées de PE en ZHP par SAGE ----

pe_decoup_sage <- pe %>% 
  st_intersection(sages) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_sage, dsn = "data/outputs/pe_decoup_sage_20231200.gpkg")

surf_pe_zhp_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_zhp_tot,
    var_somme_surfaces = surf_pe_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_zhp_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_zhp_tot,
    var_somme_surfaces = surf_pehm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_zhp_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm_zhp,
    var_somme_surfaces = surf_pe_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_zhp_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm_zhp,
    var_somme_surfaces = surf_pehm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )
## Ajout du décompte et calcul des surfaces cumulées de mares en ZHP par SAGE ----

surf_mares_zhp_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_zhp_tot,
    var_somme_surfaces = surf_mares_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_zhp_tot_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_zhp_tot,
    var_somme_surfaces = surf_mareshm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_zhp_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm_zhp,
    var_somme_surfaces = surf_mares_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_zhp_sage <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_sage %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = nom_sage,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm_zhp,
    var_somme_surfaces = surf_mareshm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de PE ZHP par SAGE ----

sages <- sages %>%
  dplyr::left_join(surf_pe_zhp_tot_sage) %>%
  dplyr::left_join(surf_pehm_zhp_tot_sage) %>%
  dplyr::left_join(surf_pe_perm_zhp_sage) %>%
  dplyr::left_join(surf_pehm_perm_zhp_sage) %>%
  dplyr::left_join(surf_mares_zhp_tot_sage) %>%
  dplyr::left_join(surf_mareshm_zhp_tot_sage) %>%
  dplyr::left_join(surf_mares_perm_zhp_sage) %>%
  dplyr::left_join(surf_mareshm_perm_zhp_sage) %>%
  units::drop_units()

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20240110.gpkg")

## Ajout du décompte et calcul des surfaces cumulées de PE en ZHP par BV_IPR ----

pe_decoup_ipr <- pe %>% 
  st_intersection(bv_ipr) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = pe_decoup_ipr, dsn = "data/outputs/pe_decoup_ipr_20231200.gpkg")

surf_pe_zhp_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_zhp_tot,
    var_somme_surfaces = surf_pe_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_zhp_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_zhp_tot,
    var_somme_surfaces = surf_pehm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pe_perm_zhp_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe_perm_zhp,
    var_somme_surfaces = surf_pe_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_perm_zhp_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 0) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pehm_perm_zhp,
    var_somme_surfaces = surf_pehm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )
## Ajout du décompte et calcul des surfaces cumulées de mares en ZHP par BV_IPR ----

surf_mares_zhp_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_zhp_tot,
    var_somme_surfaces = surf_mares_zhp_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_zhp_tot_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_zhp_tot,
    var_somme_surfaces = surf_mareshm_zhp_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mares_perm_zhp_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mares_perm_zhp,
    var_somme_surfaces = surf_mares_perm_zhp,
    zone_marais_incluse = TRUE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_mareshm_perm_zhp_ipr <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_decoup_ipr %>% 
      units::drop_units() %>% 
      filter(mare == 1) %>%
      filter(zhp == 1),
    var_id_polygone = sta_id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_mareshm_perm_zhp,
    var_somme_surfaces = surf_mareshm_perm_zhp,
    zone_marais_incluse = FALSE,
    seulement_permanent = TRUE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

## Jointure des surfaces cumulées de PE ZHP par BV IPR ----

bv_ipr <- bv_ipr %>%
  dplyr::left_join(surf_pe_zhp_tot_ipr) %>%
  dplyr::left_join(surf_pehm_zhp_tot_ipr) %>%
  dplyr::left_join(surf_pe_perm_zhp_ipr) %>%
  dplyr::left_join(surf_pehm_perm_zhp_ipr) %>%
  dplyr::left_join(surf_mares_zhp_tot_ipr) %>%
  dplyr::left_join(surf_mareshm_zhp_tot_ipr) %>%
  dplyr::left_join(surf_mares_perm_zhp_ipr) %>%
  dplyr::left_join(surf_mareshm_perm_zhp_ipr) %>%
  units::drop_units()

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Ajout de la proportion de surface en ZHP ----

## Ajout de la proportion de surface en ZHP ----

NON

# Calcul et jointure de la surface moyenne des PE permanents ----

## Calcul et jointure de la surface moyenne des PE permanents par ME ----

surface_moyenne_pe_me <- pe_decoup_me %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(cdeumassed) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(cdeumassed, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_me <- pe_decoup_me %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(cdeumassed) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(cdeumassed, surface_moy_pe_perm_tdbv)

bv_me_decoup <- bv_me_decoup %>%
  left_join(surface_moyenne_pe_me) %>%
  left_join(surface_moyenne_pe_tdbv_me)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240216.gpkg")

## Calcul et jointure de la surface moyenne des PE permanents par sage ----

surface_moyenne_pe_sage <- pe_decoup_sage %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(nom_sage) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(nom_sage, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_sage <- pe_decoup_sage %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(nom_sage) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(nom_sage, surface_moy_pe_perm_tdbv)

sages <- sages %>%
  left_join(surface_moyenne_pe_sage) %>%
  left_join(surface_moyenne_pe_tdbv_sage)

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231201.gpkg")

## Calcul et jointure de la surface moyenne des PE permanents par commune ----

surface_moyenne_pe_com <- pe_decoup_com %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(code_insee) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(code_insee, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_com <- pe_decoup_com %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(code_insee) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(code_insee, surface_moy_pe_perm_tdbv)

communes <- communes %>%
  left_join(surface_moyenne_pe_com) %>%
  left_join(surface_moyenne_pe_tdbv_com)

sf::write_sf(obj = communes, dsn = "data/outputs/communes_qualifiees_20240216.gpkg")

## Calcul et jointure de la surface moyenne des PE permanents par bv_ipr ----

surface_moyenne_pe_ipr <- pe_decoup_ipr %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  group_by(sta_id) %>%
  summarise(surface_moy_pe_perm = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(sta_id, surface_moy_pe_perm)

surface_moyenne_pe_tdbv_ipr <- pe_decoup_ipr %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent' & StreamOrde < 3) %>%
  group_by(sta_id) %>%
  summarise(surface_moy_pe_perm_tdbv = mean(surface_m2)) %>%
  st_drop_geometry() %>%
  select(sta_id, surface_moy_pe_perm_tdbv)

bv_ipr <- bv_ipr %>%
  left_join(surface_moyenne_pe_ipr) %>%
  left_join(surface_moyenne_pe_tdbv_ipr)

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Ajouts et qualification de l'attribut "departements" ----

me_decoup_dprt <- bv_me_decoup %>% 
  st_intersection(departements) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

dprt_par_me <- me_decoup_dprt %>% 
  group_by(cdeumassed) %>%
  summarise(cd_dprt = paste(unique(INSEE_DEP), collapse = ', ')) %>%
  select(cdeumassed, cd_dprt)

bv_me_decoup <- bv_me_decoup %>%
  left_join(dprt_par_me) 

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_qualifies_20240217.gpkg")

# Calcul des débits max ----

## Calcul des débits max par ME ----

qa_me <- qa %>% 
  st_intersection(bv_me_decoup) %>%
  st_drop_geometry() 

sf::write_sf(obj = qa_me, dsn = "data/outputs/qa_me_20231200.gpkg")

qa_max_me <- qa_me %>%
  select(cdeumassed, QABASN, QAMOY_MN, QAHAUN) %>%
  group_by(cdeumassed) %>%
  summarise(QAMOY_max = max (QAMOY_MN)) %>%
  select(cdeumassed, QAMOY_max)

q5_me <- q5 %>% 
  st_intersection(bv_me_decoup) %>%
  st_drop_geometry() 

sf::write_sf(obj = q5_me, dsn = "data/outputs/q5_me_20231200.gpkg")

q5_max_me <- q5_me %>%
  select(cdeumassed, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  group_by(cdeumassed) %>%
  summarise(Q5MOY_max = max (Q5MOY_MN)) %>%
  select(cdeumassed, Q5MOY_max)

bv_me_decoup <- bv_me_decoup %>%
  left_join(qa_max_me) %>%
  left_join(q5_max_me)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231202.gpkg")

## Calcul des débits max par sage ----

qa_sage <- qa %>% 
  st_intersection(sages) %>%
  st_drop_geometry() 

sf::write_sf(obj = qa_sage, dsn = "data/outputs/qa_sage_20231200.gpkg")

qa_max_sage <- qa_sage %>%
  select(nom_sage, QABASN, QAMOY_MN, QAHAUN) %>%
  group_by(nom_sage) %>%
  summarise(QAMOY_max = max (QAMOY_MN)) %>%
  select(nom_sage, QAMOY_max)

q5_sage <- q5 %>% 
  st_intersection(sages) %>%
  st_drop_geometry() 

sf::write_sf(obj = q5_sage, dsn = "data/outputs/q5_sage_20231200.gpkg")

q5_max_sage <- q5_sage %>%
  select(nom_sage, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  group_by(nom_sage) %>%
  summarise(Q5MOY_max = max (Q5MOY_MN)) %>%
  select(nom_sage, Q5MOY_max)

sages <- sages %>%
  left_join(qa_max_sage) %>%
  left_join(q5_max_sage)

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231202.gpkg")

## Calcul des débits max par commune ----

qa_com <- qa %>% 
  st_intersection(communes) %>%
  st_drop_geometry() 

sf::write_sf(obj = qa_com, dsn = "data/outputs/qa_com_20231200.gpkg")

qa_max_com <- qa_com %>%
  select(code_insee, QABASN, QAMOY_MN, QAHAUN) %>%
  group_by(code_insee) %>%
  summarise(QAMOY_max = max (QAMOY_MN)) %>%
  select(code_insee, QAMOY_max)

q5_com <- q5 %>% 
  st_intersection(communes) %>%
  st_drop_geometry() 

sf::write_sf(obj = q5_com, dsn = "data/outputs/q5_com_20231200.gpkg")

q5_max_com <- q5_com %>%
  select(code_insee, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  group_by(code_insee) %>%
  summarise(Q5MOY_max = max (Q5MOY_MN)) %>%
  select(code_insee, Q5MOY_max)

communes <- communes %>%
  left_join(qa_max_com) %>%
  left_join(q5_max_com)

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231201.gpkg")

## Calcul des débits max par bv ipr ----

qa_ipr <- qa %>% 
  st_intersection(bv_ipr) %>%
  st_drop_geometry() 

sf::write_sf(obj = qa_ipr, dsn = "data/outputs/qa_ipr_20240625.gpkg")

qa_max_ipr <- qa_ipr %>%
  select(sta_id, QABASN, QAMOY_MN, QAHAUN) %>%
  group_by(sta_id) %>%
  summarise(QAMOY_max = max (QAMOY_MN)) %>%
  select(sta_id, QAMOY_max)

q5_ipr <- q5 %>% 
  st_intersection(bv_ipr) %>%
  st_drop_geometry() 

sf::write_sf(obj = q5_ipr, dsn = "data/outputs/q5_ipr_20240625.gpkg")

q5_max_ipr <- q5_ipr %>%
  select(sta_id, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  group_by(sta_id) %>%
  summarise(Q5MOY_max = max (Q5MOY_MN)) %>%
  select(sta_id, Q5MOY_max)

bv_ipr <- bv_ipr %>%
  left_join(qa_max_ipr) %>%
  left_join(q5_max_ipr)

sf::write_sf(obj = bv_ipr, dsn = "data/outputs/bv_ipr_20240625.gpkg")

# Synthèse des prélèvements en retenue ----

## Synthèse des prélèvements en retenue par ME ----

prelevements_me <- prelevements %>%
  st_join(bv_me_decoup, 
          largest = T) %>%
  distinct()

me_prelevements <- prelevements_me %>%
  group_by(cdeumassed) %>%
  summarise(prel_2008_ret = sum(X2008_1),
            prel_2009_ret = sum(X2009_1),
            prel_2010_ret = sum(X2010_1),
            prel_2011_ret = sum(X2011_1),
            prel_2012_ret = sum(X2012_1),
            prel_2013_ret = sum(X2013_1),
            prel_2014_ret = sum(X2014_1),
            prel_2015_ret = sum(X2015_1),
            prel_2016_ret = sum(X2016_1)) %>%
  select(cdeumassed,
         prel_2008_ret, 
         prel_2009_ret,
         prel_2010_ret,
         prel_2011_ret,
         prel_2012_ret,
         prel_2013_ret,
         prel_2014_ret,
         prel_2015_ret,
         prel_2016_ret) %>%
  sf::st_drop_geometry() 

bv_me_decoup <- bv_me_decoup %>%
  left_join(y = me_prelevements)

## Synthèse des prélèvements 2019 en retenue par ME ----

prelevements_me_2019 <- prelevements_2019 %>%
  st_join(bv_me_decoup %>% select(cdeumassed), 
          largest = T) %>%
  distinct()

me_prelevements_2019 <- prelevements_me_2019 %>%
  filter(sur_retenue == 1) %>%
  group_by(cdeumassed) %>%
  summarise(prel_2019_ret = sum(VOLUME)) %>%
  select(cdeumassed,
         prel_2019_ret) %>%
  sf::st_drop_geometry() 

bv_me_decoup <- bv_me_decoup %>%
  left_join(y = me_prelevements_2019)

## Synthèse des prélèvements par ME ----

prelevements_totaux_me <- prelevements_totaux %>%
  st_join(bv_me_decoup, 
          largest = T) %>%
  distinct()

me_prelevements_totaux <- prelevements_totaux_me %>%
  group_by(cdeumassed) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(cdeumassed,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

bv_me_decoup <- bv_me_decoup %>%
  left_join(y = me_prelevements_totaux)

## Synthèse des prélèvements 2019 par ME ----

prelevements_me_tot_2019 <- prelevements_2019 %>%
  st_join(bv_me_decoup %>% select(cdeumassed), 
          largest = T) %>%
  distinct()

me_prelevements_tot_2019 <- prelevements_me_tot_2019  %>%
  group_by(cdeumassed) %>%
  summarise(prel_2019_tot = sum(VOLUME)) %>%
  select(cdeumassed,
         prel_2019_tot) %>%
  sf::st_drop_geometry() 

bv_me_decoup <- bv_me_decoup %>%
  left_join(y = me_prelevements_tot_2019)

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231202.gpkg")

## Synthèse des prélèvements en retenue par sage ----

prelevements_sage <- prelevements %>%
  st_join(sages, 
          largest = T) %>%
  distinct()

sage_prelevements <- prelevements_sage %>%
  group_by(nom_sage) %>%
  summarise(prel_2008_ret = sum(X2008_1),
            prel_2009_ret = sum(X2009_1),
            prel_2010_ret = sum(X2010_1),
            prel_2011_ret = sum(X2011_1),
            prel_2012_ret = sum(X2012_1),
            prel_2013_ret = sum(X2013_1),
            prel_2014_ret = sum(X2014_1),
            prel_2015_ret = sum(X2015_1),
            prel_2016_ret = sum(X2016_1)) %>%
  select(nom_sage,
         prel_2008_ret, 
         prel_2009_ret,
         prel_2010_ret,
         prel_2011_ret,
         prel_2012_ret,
         prel_2013_ret,
         prel_2014_ret,
         prel_2015_ret,
         prel_2016_ret) %>%
  sf::st_drop_geometry() 

sages <- sages %>%
  left_join(y = sage_prelevements)

## Synthèse des prélèvements 2019 en retenue par sage ----

prelevements_sage_2019 <- prelevements_2019 %>%
  st_join(sages %>% select(nom_sage), 
          largest = T) %>%
  distinct()

sage_prelevements_2019 <- prelevements_sage_2019 %>%
  filter(sur_retenue == 1) %>%
  group_by(nom_sage) %>%
  summarise(prel_2019_ret = sum(VOLUME)) %>%
  select(nom_sage,
         prel_2019_ret) %>%
  sf::st_drop_geometry() 

sages <- sages %>%
  left_join(y = sage_prelevements_2019)

## Synthèse des prélèvements par sage ----

prelevements_totaux_sage <- prelevements_totaux %>%
  st_join(sages, 
          largest = T) %>%
  distinct()

sage_prelevements_totaux <- prelevements_totaux_sage %>%
  group_by(nom_sage) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(nom_sage,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

sages <- sages %>%
  left_join(y = sage_prelevements_totaux)

## Synthèse des prélèvements totaux 2019 par sage ----

prelevements_sage_tot_2019 <- prelevements_2019 %>%
  st_join(sages %>% select(nom_sage), 
          largest = T) %>%
  distinct()

sage_prelevements_tot_2019 <- prelevements_sage_tot_2019  %>%
  group_by(nom_sage) %>%
  summarise(prel_2019_tot = sum(VOLUME)) %>%
  select(nom_sage,
         prel_2019_tot) %>%
  sf::st_drop_geometry() 

sages <- sages %>%
  left_join(y = sage_prelevements_tot_2019)

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231202.gpkg")

## Synthèse des prélèvements sur retenue par commune ----

prelevements_com <- prelevements %>%
  st_join(communes, 
          largest = T) %>%
  distinct()

com_prelevements <- prelevements_com %>%
  group_by(code_insee) %>%
  summarise(prel_2008_ret = sum(X2008_1),
            prel_2009_ret = sum(X2009_1),
            prel_2010_ret = sum(X2010_1),
            prel_2011_ret = sum(X2011_1),
            prel_2012_ret = sum(X2012_1),
            prel_2013_ret = sum(X2013_1),
            prel_2014_ret = sum(X2014_1),
            prel_2015_ret = sum(X2015_1),
            prel_2016_ret = sum(X2016_1)) %>%
  select(code_insee,
         prel_2008_ret, 
         prel_2009_ret,
         prel_2010_ret,
         prel_2011_ret,
         prel_2012_ret,
         prel_2013_ret,
         prel_2014_ret,
         prel_2015_ret,
         prel_2016_ret) %>%
  sf::st_drop_geometry() 

communes <- communes %>%
  left_join(y = com_prelevements)

## Synthèse des prélèvements 2019 sur retenue par commune ----

prelevements_tot_com_2019 <- prelevements_2019 %>%
  st_join(communes %>% select(code_insee), 
          largest = T) %>%
  distinct()

com_prelevements_tot_2019 <- prelevements_tot_com_2019 %>%
  filter(sur_retenue == 1) %>%
  group_by(code_insee) %>%
  summarise(prel_2019_ret = sum(VOLUME)) %>%
  select(code_insee,
         prel_2019_ret) %>%
  sf::st_drop_geometry() 

communes <- communes %>%
  left_join(y = com_prelevements_tot_2019)

## Synthèse des prélèvements par commune ----

prelevements_totaux_com <- prelevements_totaux %>%
  st_join(communes, 
          largest = T) %>%
  distinct()

com_prelevements_totaux <- prelevements_totaux_com %>%
  group_by(code_insee) %>%
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1)) %>%
  select(code_insee,
         prel_2008_tot, 
         prel_2009_tot,
         prel_2010_tot,
         prel_2011_tot,
         prel_2012_tot,
         prel_2013_tot,
         prel_2014_tot,
         prel_2015_tot,
         prel_2016_tot) %>%
  sf::st_drop_geometry() 

communes <- communes %>%
  left_join(y = com_prelevements_totaux)

## Synthèse des prélèvements totaux 2019 par communes ----

prelevements_com_tot_2019 <- prelevements_2019 %>%
  st_join(communes %>% select(code_insee), 
          largest = T) %>%
  distinct()

com_prelevements_tot_2019 <- prelevements_com_tot_2019  %>%
  group_by(code_insee) %>%
  summarise(prel_2019_tot = sum(VOLUME)) %>%
  select(code_insee,
         prel_2019_tot) %>%
  sf::st_drop_geometry() 

communes <- communes %>%
  left_join(y = com_prelevements_tot_2019)

sf::write_sf(obj = communes, dsn = "data/outputs/communes_20231202.gpkg")

# Suppression des valeurs de prélevements des territoires partiellement hors DR ----

bv_me_decoup <- bv_me_decoup %>%
  mutate(prel_2008_ret = ifelse(hors_prelevement == 0, prel_2008_ret, NA),
         prel_2009_ret = ifelse(hors_prelevement == 0, prel_2009_ret, NA),
         prel_2010_ret = ifelse(hors_prelevement == 0, prel_2010_ret, NA),
         prel_2011_ret = ifelse(hors_prelevement == 0, prel_2011_ret, NA),
         prel_2012_ret = ifelse(hors_prelevement == 0, prel_2012_ret, NA),
         prel_2013_ret = ifelse(hors_prelevement == 0, prel_2013_ret, NA),
         prel_2014_ret = ifelse(hors_prelevement == 0, prel_2014_ret, NA),
         prel_2015_ret = ifelse(hors_prelevement == 0, prel_2015_ret, NA),
         prel_2016_ret = ifelse(hors_prelevement == 0, prel_2016_ret, NA),
         prel_2019_ret = ifelse(hors_prelevement == 0, prel_2019_ret, NA),
         prel_2008_tot = ifelse(hors_prelevement == 0, prel_2008_tot, NA),
         prel_2009_tot = ifelse(hors_prelevement == 0, prel_2009_tot, NA),
         prel_2010_tot = ifelse(hors_prelevement == 0, prel_2010_tot, NA),
         prel_2011_tot = ifelse(hors_prelevement == 0, prel_2011_tot, NA),
         prel_2012_tot = ifelse(hors_prelevement == 0, prel_2012_tot, NA),
         prel_2013_tot = ifelse(hors_prelevement == 0, prel_2013_tot, NA),
         prel_2014_tot = ifelse(hors_prelevement == 0, prel_2014_tot, NA),
         prel_2015_tot = ifelse(hors_prelevement == 0, prel_2015_tot, NA),
         prel_2016_tot = ifelse(hors_prelevement == 0, prel_2016_tot, NA),
         prel_2019_tot = ifelse(hors_prelevement == 0, prel_2019_tot, NA))

sf::write_sf(obj = bv_me_decoup, dsn = "data/outputs/bv_me_decoup_20231202.gpkg")

sages <- sages %>%
  mutate(prel_2008_ret = ifelse(hors_prelevement == 0, prel_2008_ret, NA),
         prel_2009_ret = ifelse(hors_prelevement == 0, prel_2009_ret, NA),
         prel_2010_ret = ifelse(hors_prelevement == 0, prel_2010_ret, NA),
         prel_2011_ret = ifelse(hors_prelevement == 0, prel_2011_ret, NA),
         prel_2012_ret = ifelse(hors_prelevement == 0, prel_2012_ret, NA),
         prel_2013_ret = ifelse(hors_prelevement == 0, prel_2013_ret, NA),
         prel_2014_ret = ifelse(hors_prelevement == 0, prel_2014_ret, NA),
         prel_2015_ret = ifelse(hors_prelevement == 0, prel_2015_ret, NA),
         prel_2016_ret = ifelse(hors_prelevement == 0, prel_2016_ret, NA),
         prel_2019_ret = ifelse(hors_prelevement == 0, prel_2019_ret, NA),
         prel_2008_tot = ifelse(hors_prelevement == 0, prel_2008_tot, NA),
         prel_2009_tot = ifelse(hors_prelevement == 0, prel_2009_tot, NA),
         prel_2010_tot = ifelse(hors_prelevement == 0, prel_2010_tot, NA),
         prel_2011_tot = ifelse(hors_prelevement == 0, prel_2011_tot, NA),
         prel_2012_tot = ifelse(hors_prelevement == 0, prel_2012_tot, NA),
         prel_2013_tot = ifelse(hors_prelevement == 0, prel_2013_tot, NA),
         prel_2014_tot = ifelse(hors_prelevement == 0, prel_2014_tot, NA),
         prel_2015_tot = ifelse(hors_prelevement == 0, prel_2015_tot, NA),
         prel_2016_tot = ifelse(hors_prelevement == 0, prel_2016_tot, NA),
         prel_2019_tot = ifelse(hors_prelevement == 0, prel_2019_tot, NA))

sf::write_sf(obj = sages, dsn = "data/outputs/sages_20231202.gpkg")

# Synthèse des résultats de la couche communes aux couches départements et régions ----

dprt <- communes %>%
  group_by(insee_dep) %>%
  summarise(
    longueur_ce_topage = sum(longueur_ce_topage, na.rm = TRUE),
    longueur_ce_tdbv_topage = sum(longueur_ce_tdbv_topage, na.rm = TRUE),
    strahler_max = max(strahler_max, na.rm = TRUE),
    longueur_topage_intersecte_pe_tot = sum(longueur_topage_intersecte_pe_tot, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tot = sum(longueur_topage_intersecte_pehm_tot, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_topage_intersecte_pehm_tdbv_tot, na.rm = TRUE),
    longueur_topage_intersecte_pe_perm = sum(longueur_topage_intersecte_pe_perm, na.rm = TRUE),
    longueur_topage_intersecte_pehm_perm = sum(longueur_topage_intersecte_pehm_perm, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_topage_intersecte_pehm_tdbv_perm, na.rm = TRUE),
    nb_pe_tot = sum(nb_pe_tot, na.rm = TRUE),
    surf_pe_tot = sum(surf_pe_tot, na.rm = TRUE),
    nb_pehm_tot = sum(nb_pehm_tot, na.rm = TRUE),
    surf_pehm_tot= sum(surf_pehm_tot, na.rm = TRUE),
    nb_pe_perm = sum(nb_pe_perm, na.rm = TRUE),
    surf_pe_perm = sum(surf_pe_perm, na.rm = TRUE),
    nb_pehm_perm = sum(nb_pehm_perm, na.rm = TRUE),
    surf_pehm_perm = sum(surf_pehm_perm, na.rm = TRUE),
    nb_pehm_tdbv_tot = sum(nb_pehm_tdbv_tot, na.rm = TRUE),
    surf_pehm_tdbv_tot = sum(surf_pehm_tdbv_tot, na.rm = TRUE),
    nb_pehm_tdbv_perm = sum(nb_pehm_tdbv_perm, na.rm = TRUE),
    surf_pehm_tdbv_perm = sum(surf_pehm_tdbv_perm, na.rm = TRUE),
    nb_pehm_connecte_tot = sum(nb_pehm_connecte_tot, na.rm = TRUE),
    surf_pehm_connecte_tot = sum(surf_pehm_connecte_tot, na.rm = TRUE),
    nb_pehm_connecte_perm = sum(nb_pehm_connecte_perm, na.rm = TRUE),
    surf_pehm_connecte_perm = sum(surf_pehm_connecte_perm, na.rm = TRUE),
    nb_pehm_sur_cours_tot = sum(nb_pehm_sur_cours_tot, na.rm = TRUE),
    surf_pehm_sur_cours_tot = sum(surf_pehm_sur_cours_tot, na.rm = TRUE),
    nb_pehm_sur_cours_perm = sum(nb_pehm_sur_cours_perm, na.rm = TRUE),
    surf_pehm_sur_cours_perm = sum(surf_pehm_sur_cours_perm, na.rm = TRUE),
    nb_pehm_sur_source_tot = sum(nb_pehm_sur_source_tot, na.rm = TRUE),
    surf_pehm_sur_source_tot = sum(surf_pehm_sur_source_tot, na.rm = TRUE),
    nb_pehm_sur_source_perm = sum(nb_pehm_sur_source_perm, na.rm = TRUE),
    surf_pehm_sur_source_perm = sum(surf_pehm_sur_source_perm, na.rm = TRUE),
    nb_pehm_zhp_tot = sum(nb_pehm_zhp_tot, na.rm = TRUE),
    surf_pehm_zhp_tot = sum(surf_pehm_zhp_tot, na.rm = TRUE),
    nb_mares_tot = sum(nb_mares_tot, na.rm = TRUE),
    surf_mares_tot =  sum(surf_mares_tot, na.rm = TRUE),
    QAMOY_max = max(QAMOY_max, na.rm = TRUE),
    Q5MOY_max = max(Q5MOY_max, na.rm = TRUE)) %>%
  filter(insee_dep %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85))%>%
  st_drop_geometry()

departements <- departements %>%
  mutate(surface_dprt = st_area(geometry)) %>%
  left_join(dprt, by = c("INSEE_DEP" = "insee_dep")) %>%
  units::drop_units()

sf::write_sf(obj = departements, dsn = "data/outputs/departements_20231202.gpkg")

region <- communes %>%
  group_by(insee_reg) %>%
  summarise(
    longueur_ce_topage = sum(longueur_ce_topage, na.rm = TRUE),
    longueur_ce_tdbv_topage = sum(longueur_ce_tdbv_topage, na.rm = TRUE),
    strahler_max = max(strahler_max, na.rm = TRUE),
    longueur_topage_intersecte_pe_tot = sum(longueur_topage_intersecte_pe_tot, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tot = sum(longueur_topage_intersecte_pehm_tot, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tdbv_tot = sum(longueur_topage_intersecte_pehm_tdbv_tot, na.rm = TRUE),
    longueur_topage_intersecte_pe_perm = sum(longueur_topage_intersecte_pe_perm, na.rm = TRUE),
    longueur_topage_intersecte_pehm_perm = sum(longueur_topage_intersecte_pehm_perm, na.rm = TRUE),
    longueur_topage_intersecte_pehm_tdbv_perm = sum(longueur_topage_intersecte_pehm_tdbv_perm, na.rm = TRUE),
    nb_pe_tot = sum(nb_pe_tot, na.rm = TRUE),
    surf_pe_tot = sum(surf_pe_tot, na.rm = TRUE),
    nb_pehm_tot = sum(nb_pehm_tot, na.rm = TRUE),
    surf_pehm_tot= sum(surf_pehm_tot, na.rm = TRUE),
    nb_pe_perm = sum(nb_pe_perm, na.rm = TRUE),
    surf_pe_perm = sum(surf_pe_perm, na.rm = TRUE),
    nb_pehm_perm = sum(nb_pehm_perm, na.rm = TRUE),
    surf_pehm_perm = sum(surf_pehm_perm, na.rm = TRUE),
    nb_pehm_tdbv_tot = sum(nb_pehm_tdbv_tot, na.rm = TRUE),
    surf_pehm_tdbv_tot = sum(surf_pehm_tdbv_tot, na.rm = TRUE),
    nb_pehm_tdbv_perm = sum(nb_pehm_tdbv_perm, na.rm = TRUE),
    surf_pehm_tdbv_perm = sum(surf_pehm_tdbv_perm, na.rm = TRUE),
    nb_pehm_connecte_tot = sum(nb_pehm_connecte_tot, na.rm = TRUE),
    surf_pehm_connecte_tot = sum(surf_pehm_connecte_tot, na.rm = TRUE),
    nb_pehm_connecte_perm = sum(nb_pehm_connecte_perm, na.rm = TRUE),
    surf_pehm_connecte_perm = sum(surf_pehm_connecte_perm, na.rm = TRUE),
    nb_pehm_sur_cours_tot = sum(nb_pehm_sur_cours_tot, na.rm = TRUE),
    surf_pehm_sur_cours_tot = sum(surf_pehm_sur_cours_tot, na.rm = TRUE),
    nb_pehm_sur_cours_perm = sum(nb_pehm_sur_cours_perm, na.rm = TRUE),
    surf_pehm_sur_cours_perm = sum(surf_pehm_sur_cours_perm, na.rm = TRUE),
    QAMOY_max = max(QAMOY_max, na.rm = TRUE),
    Q5MOY_max = max(Q5MOY_max, na.rm = TRUE),
    nb_mares_tot = sum(nb_mares_tot, na.rm = TRUE),
    surf_mares_tot = sum(surf_mares_tot, na.rm = TRUE),
    nb_mareshm_tot = sum(nb_mareshm_tot, na.rm = TRUE),
    surf_mareshm_tot= sum(surf_mareshm_tot, na.rm = TRUE),
    nb_mares_perm = sum(nb_mares_perm, na.rm = TRUE),
    surf_mares_perm = sum(surf_mares_perm, na.rm = TRUE),
    nb_mareshm_perm = sum(nb_mareshm_perm, na.rm = TRUE),
    surf_mareshm_perm = sum(surf_mareshm_perm, na.rm = TRUE),
    prel_2008_ret = sum(prel_2008_ret, na.rm = TRUE),
    prel_2009_ret = sum(prel_2009_ret, na.rm = TRUE),
    prel_2010_ret = sum(prel_2010_ret, na.rm = TRUE),
    prel_2011_ret = sum(prel_2011_ret, na.rm = TRUE),
    prel_2012_ret = sum(prel_2012_ret, na.rm = TRUE),
    prel_2013_ret = sum(prel_2013_ret, na.rm = TRUE),
    prel_2014_ret = sum(prel_2014_ret, na.rm = TRUE),
    prel_2015_ret = sum(prel_2015_ret, na.rm = TRUE),
    prel_2016_ret = sum(prel_2016_ret, na.rm = TRUE),
    prel_2019_ret = sum(prel_2019_ret, na.rm = TRUE),
    prel_2008_tot = sum(prel_2008_tot, na.rm = TRUE),
    prel_2009_tot = sum(prel_2009_tot, na.rm = TRUE),
    prel_2010_tot = sum(prel_2010_tot, na.rm = TRUE),
    prel_2011_tot = sum(prel_2011_tot, na.rm = TRUE),
    prel_2012_tot = sum(prel_2012_tot, na.rm = TRUE),
    prel_2013_tot = sum(prel_2013_tot, na.rm = TRUE),
    prel_2014_tot = sum(prel_2014_tot, na.rm = TRUE),
    prel_2015_tot = sum(prel_2015_tot, na.rm = TRUE),
    prel_2016_tot = sum(prel_2016_tot, na.rm = TRUE),
    prel_2019_tot = sum(prel_2019_tot, na.rm = TRUE)) %>%
  filter(insee_reg %in% c(52, 53)) %>%
  st_drop_geometry()

regions <- regions %>%
  left_join(region, by = c("insee_reg" = "insee_reg"))

sf::write_sf(obj = regions, dsn = "data/outputs/regions_20231202.gpkg")

# Sauvegarde des résultats

save(bv_ipr,
     ce_decoup_ipr,
     ce_tdbv_decoup_ipr,
     ce_topage,
     ce_topage_ipr,
     cehm_topage,
     lineaire_topage_pe,
     lineaire_topage_pe_ipr,
     pe,
     zhp_decoup_ipr,
     zhphm_decoup_ipr,
     file = "data/outputs/w_ipr.RData")

## Charger ---- 
load(file = "data/outputs/w_ipr.RData") 


save(ce_topage_me,
     ce_topage_sage,
     ce_topage_com,
     lineaire_topage_pe_me,
     lineaire_topage_pe_sage,
     lineaire_topage_pe_com,
     pe_decoup_me,
     pe_decoup_sage,
     pe_decoup_com,
     sages,
     bv_me_decoup,
     communes,
     file = "data/outputs/w_territoires.RData")

save(lineaire_topage_pe_me,
     lineaire_topage_pe_sage,
     lineaire_topage_pe_com,
     lineaire_topage_pe_ipr,
     pe_decoup_me,
     pe_decoup_sage,
     pe_decoup_com,
     pe_decoup_ipr,
     pe,
     ce_topage,
     ce_topage_me,
     ce_topage_sage,
     ce_topage_com,
     ce_topage_ipr,
     bv_me_decoup,
     sages, 
     communes,
     bv_ipr,
     departements, 
     regions,
     file = "data/outputs/w_territoires2.RData")

save(zhp_tot,
     zhp_decoup_me,
     zhp_decoup_sage,
     zhp_decoup_ipr,
     zhp_decoup_com,
  file = "data/outputs/w_territoires3.RData")

# chargement des résultats

load(file = "data/outputs/w_territoires.RData")
load(file = "data/outputs/w_territoires2.RData")
load(file = "data/outputs/w_territoires3.RData")
load(file = "data/outputs/w_plando1.RData")
load(file = "data/outputs/w_plando2.RData")

troncons_topage_plus_proches <-
  sf::read_sf(dsn = "data/outputs/troncons_topage_plus_proches.gpkg")

# Synthèse volume prelevements par ME (INCLUS DS REGION) SAGE, dprt, region ----



# brouillon ----

bv_me_decoup <- bv_me_decoup %>%
  select(-prel_2018_ret, 
         -prel_2008_tot, 
         -prel_2009_tot, 
         -prel_2010_tot, 
         -prel_2011_tot, 
         -prel_2012_tot, 
         -prel_2013_tot, 
         -prel_2014_tot,
         -prel_2015_tot, 
         -prel_2016_tot, 
         -prel_2019_tot)

bv_ipr <- bv_ipr %>%
  select(-starts_with("prel_20"), 
         -starts_with("Q"), 
         -starts_with("pourcentage_"), 
         -starts_with("nb_mares"),
         -starts_with("surf_mares"))

sages <- sages %>%
  select(-starts_with("nb_"), -starts_with("surf_"), -starts_with("longueur_"), -starts_with("surface_moy"))

sages <- sages %>%
  select(-starts_with("nb_"), -starts_with("surf_"), -starts_with("longueur_"), -starts_with("surface_moy"))

bv_ipr <- bv_ipr %>%
  select(-starts_with("nb_"), -starts_with("surf_"), -starts_with("longueur_"), -starts_with("surface_moy"))

communes <- communes %>%
  select(-starts_with("nb_"), -starts_with("surf_"), -starts_with("longueur_"), -starts_with("surface_moy"))

rm(dprt)
