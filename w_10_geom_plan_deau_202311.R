# VERSION FINALISEE AU 20240715

## CREER LA COUCHE Plan D'Eau (PE) 

#La couche surfaces_elementaires importée dans R a été préalablement préparée de la manière suivante : 
#- Téléchargement de la couche surfaces_elementaires de la BD Topage
#- Sélection des surfaces élémentaires de la zone d'étude
#- Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objet les plus gros 
#- Suppression des invalidité de géométries (extension nettoyeur de polygones)
#- Création et calcul d'un champ coef_gravelius = perimeter()/(2*sqrt(pi()*area())) 
#- Création et affectation d'un code 0/1 pour les attributs :
# o ecoulement_naturel (NatureSE canal, écoulement canalisé ou écoulement naturel + corrections manuelles)
# o zone_marais (attribution manuelle à partir des zones de marais du Scan25)
# o marais_uhc1 (intersection UHC1 and "Persistanc" != 'permanent' and  "NatureSE" not in ( 'Plan d''eau - mare' ,  'Plan d''eau - retenue' ,  'Plan d''eau - réservoir', 'PE-réservoir-bassinorage', 'Ecoulement naturel' ) + corrections manuelles loire et vendée)

# Library ----
library(tidyverse)
library(sf)
library(units)

source(file = "R/functions.R")

# Import des données ----

surface_elementaire <-
  sf::read_sf(dsn = "data/surfaces_elementaires_zone_etude_valid_20231123.gpkg") %>% 
  rename(cdoh_plando = CdOH) %>%
  st_transform(crs = 2154)

estran <-
  sf::read_sf(dsn = "data/BDTOPO_V3_zone_estran.gpkg") %>%
  select(cleabs) %>%
  st_transform(crs = 2154)

me_transition <-
  sf::read_sf(dsn = "data/MasseDEauTransition_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

me_cotiere <-
  sf::read_sf(dsn = "data/MasseDEauCotiere_edl2019.gpkg") %>%
  select(cdeumassed) %>%
  st_transform(crs = 2154)

bv <- sf::read_sf(dsn = "data/bv_topage_zone_etude.gpkg") %>% 
  rename(cdoh_bv = CdOH) %>%
  st_transform(crs = 2154)

qa <- sf::read_sf(dsn = "data/qa_zone_etude.gpkg") %>%
  select(ID_BDCARTH, QABASN, QAMOY_MN, QAHAUN) %>%
  st_transform(crs = 2154)

q5 <- sf::read_sf(dsn = "data/q5_zone_etude.gpkg") %>%
  select(ID_BDCARTH, Q5BASN, Q5MOY_MN, Q5HAUN) %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>%
  st_transform(crs = 2154)

ce_topage <- sf::read_sf(dsn = "data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.gpkg") %>% 
  rename(cdoh_ce = CdOH, 
         persistance_ce = Persistanc) %>%
  select(cdoh_ce, NatureTH, persistance_ce, StreamOrde) %>%
  st_transform(crs = 2154)

sources <- sf::read_sf(dsn = "data/NoeudHydrographique_zone_etude.gpkg") %>%
  rename(cdoh_source = CdOH) %>%
  filter(CategorieN == "Source") %>%
  select(cdoh_source) %>%
  st_transform(crs = 2154)

communes <- sf::read_sf(dsn = "data/communes_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(code_insee,nom_officiel, code_insee_du_departement, code_insee_de_la_region) %>%
  rename(nom_com = nom_officiel,
         insee_dep = code_insee_du_departement,
         insee_reg = code_insee_de_la_region)

sages <- sf::read_sf(dsn = "data/sages_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(NOM) %>%
  rename(nom_sage = NOM)

bv_me <- sf::read_sf(dsn = "data/bassins_versants_me_zone_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(cdeumassed, nombvspemd) 

centroides_pe_qualifies <- sf::read_sf(dsn = "data/outputs/centroides_pe_litho_20231128.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(cdoh_plando, lithologie, distance_km)

litho <- sf::read_sf(dsn = "data/carte_lithologique_simplifiee_vectorisee_perimetre_etude.gpkg") %>%
  st_transform(crs = 2154) %>%
  select(descr)

charm_17 <- sf::read_sf(dsn = "data/charm_17.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_22 <- sf::read_sf(dsn = "data/charm_22.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_28 <- sf::read_sf(dsn = "data/charm_28.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_29 <- sf::read_sf(dsn = "data/charm_29.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_35 <- sf::read_sf(dsn = "data/charm_35.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_37 <- sf::read_sf(dsn = "data/charm_37.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_41 <- sf::read_sf(dsn = "data/charm_41.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_44 <- sf::read_sf(dsn = "data/charm_44.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_45 <- sf::read_sf(dsn = "data/charm_45.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_49 <- sf::read_sf(dsn = "data/charm_49.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_50 <- sf::read_sf(dsn = "data/charm_50.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_53 <- sf::read_sf(dsn = "data/charm_53.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_56 <- sf::read_sf(dsn = "data/charm_56.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_61 <- sf::read_sf(dsn = "data/charm_61.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_72 <- sf::read_sf(dsn = "data/charm_72.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_79 <- sf::read_sf(dsn = "data/charm_79.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_85 <- sf::read_sf(dsn = "data/charm_85.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

charm_86 <- sf::read_sf(dsn = "data/charm_86.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(geol = DESCR) %>%
  select(geol)

zhp <- sf::read_sf(dsn = "data/pe_zhp.gpkg") %>%
  st_drop_geometry() %>%
  select(cdoh_plando, zhp)

nappe_charm <-
  sf::read_sf(dsn = "data/nappe_charm.gpkg") %>%
  mutate(nappe_charm = 1)

roe_lb <- data.table::fread(file = "data/lb_temp20230402.csv",
                            encoding = "Latin-1",
                            colClasses = c("date_creation" = "character"))

roe_sn <- data.table::fread(file = "data/sn_temp20230402.csv",
                            encoding = "Latin-1",
                            colClasses = c("date_creation" = "character"))

roe_nr <- data.table::fread(file = "data/nr_temp20230402.csv",
                            encoding = "Latin-1",
                            colClasses = c("date_creation" = "character"))

# Fusion des bd_charm de la zone d'étude ----

charm_zone_etude <- 
  dplyr::bind_rows(
    charm_17,charm_22,charm_28,charm_29,charm_35,charm_37,charm_41,charm_44,charm_45,charm_49,charm_50,charm_53,charm_56,charm_61,charm_72,charm_79,charm_85,charm_86)
  
sf::write_sf(obj = charm_zone_etude, dsn = "data/outputs/charm_zone_etude_20231130.gpkg")

# Préparation préalable de la couche plan d'eau (pe) ----

## Ajouts et qualification (0/1) de l'attribut "bassin_orage" ----

surface_elementaire <- surface_elementaire %>%
  mutate(bassin_orage = case_when(
    NatureSE == "PE-réservoir-bassinorage" ~ 1,
    NatureSE != "PE-réservoir-bassinorage"~ 0))

## Ajouts et qualification (0/1) de l'attribut "bassin_ERU" ----

surface_elementaire <- surface_elementaire %>%
  mutate(bassin_eru = ifelse(
    (coef_gravelius < 1.015 & NatureSE != "PE-réservoir-bassinorage" & NatureSE != "Plan d'eau - mare" & NatureSE != "PE - réservoir -piscicult" & NatureSE != "Ecoulement naturel"), 
    1, 0))
  
## Ajouts et qualification (0/1) de l'attribut "zone_estuarienne" ----

surface_elementaire <- surface_elementaire %>%
  st_join(estran) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cleabs) ~ 1,
    is.na(cleabs) ~ 0)) %>%
  distinct() %>%
  select(-cleabs)

surface_elementaire <- surface_elementaire %>%
  st_join(me_transition) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ 1,
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_elementaire <- surface_elementaire %>%
  st_join(me_cotiere) %>% 
  mutate(zone_estuarienne = case_when(
    !is.na(cdeumassed) ~ 1,
    is.na(cdeumassed) ~ zone_estuarienne)) %>%
  distinct() %>%
  select(-cdeumassed)

surface_elementaire <- surface_elementaire %>%
  mutate(zone_estuarienne = ifelse(
    (NatureSE == "PE - retenue - bassinport" | NatureSE == "Plan d'eau - estuaire" ),
    1, zone_estuarienne))

# Filtrer les PE "réels" ----

## Ajouts et qualification (0/1) de l'attribut "a_retirer" ----

surface_elementaire <- surface_elementaire %>%
  mutate(a_retirer = ifelse(
    (ecoulement_naturel == 1 | 
       zone_estuarienne == 1 |
       bassin_eru == 1 |
       bassin_orage == 1 |
       marais_uhc1 == 1), 
    1, 0))

## Filtre des objets PE ----

pe <- surface_elementaire %>%
  filter(a_retirer == 0)

# export du résultat de qualification pour validation ----

sf::write_sf(obj = surface_elementaire, dsn = "data/outputs/surfaces_elementaires_qualifiees_20231123.gpkg")
sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231123.gpkg")

### ETAPE DE VALIDATION MANUELLE ----

# Import de la version corrigée de surface_elementaire et génération de la nouvelle couche pe ----

surface_elementaire <-
  sf::read_sf(dsn = "data/outputs/surfaces_elementaires_qualifiees_20231123.gpkg") %>%
  mutate(a_retirer = ifelse(
    (ecoulement_naturel == 1 | 
       zone_estuarienne == 1 |
       bassin_eru == 1 |
       bassin_orage == 1 |
       marais_uhc1 == 1), 
    1, 0)) %>%
  st_transform(crs = 2154)

pe <- surface_elementaire %>%
  filter(a_retirer == 0)

# Sauvegarde 1 ----

save(pe,
     surface_elementaire,
     qa,
     q5,
     bv,
     ce_topage,
     me_cotiere,
     me_transition,
     estran,
     sources,
     file = "data/outputs/w_plando1.RData")

# Calcul des ce topage intersectes ----

lineaire_topage_pe <- ce_topage %>% 
  st_intersection(pe) %>% # découpage des ce selon les pe
  mutate(longueur_topage_intersect = st_length(.)) 

sf::write_sf(obj = lineaire_topage_pe, dsn = "data/outputs/lineaires_topage_pe_20240605.gpkg")

lineaire_intersecte_par_pe <- lineaire_topage_pe %>%
  select(cdoh_plando, longueur_topage_intersect) %>%
  st_drop_geometry() %>%
  group_by(cdoh_plando) %>%
  summarise(longueur_topage_intersecte = sum(longueur_topage_intersect))

pe <- pe %>%
  left_join(lineaire_intersecte_par_pe %>%
              select(cdoh_plando, longueur_topage_intersecte))

# Calcul de la surface des PE ----

pe <- pe %>%
  mutate(surface_m2 = st_area(geom)) %>%
  units::drop_units()

# Ajouts et qualification (0/1) de l'attribut "mare" ----

pe <- pe %>%
  mutate(mare = ifelse(
    surface_m2 < 500 &
      is.na(longueur_topage_intersecte) &
      NatureSE !=  "PE - réservoir -piscicult" &  
      NatureSE !=  "Plan d'eau - gravière" & 
      NatureSE != "Plan d'eau - retenue"  & 
      NatureSE != "Plan d'eau - réservoir" ,
    1, 0))

# Sauvegarde couche PE ----

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231128.gpkg")

# Jointure des CD carthage et débits aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_troncons <- qa %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_debits <- intersect(bv_avec_troncons,
                                 bv_avec_plandos)

#Très long
troncons_plus_proches <- identifier_troncons_les_plus_proches (sf_plandos = pe,
                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_debits),
                                               sf_troncons = qa)
#Très long
troncons_plus_proches <- troncons_plus_proches %>% 
  left_join(y = qa %>% mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(QAMOY_MN == max(QAMOY_MN)) %>% # éventuellement choisir un autre débit
  ungroup() %>%
  unique()

troncons_carthage_plus_proche <- troncons_carthage_plus_proche %>%
  group_by(cdoh_plando) %>%
  slice(1) 

troncons_carthage_plus_proche2 <- troncons_carthage_plus_proche %>%
  left_join(y = q5 %>% mutate(ID_BDCARTH = as.character(ID_BDCARTH)) %>% st_drop_geometry(),
            by = c("ID_BDCARTH" = "ID_BDCARTH"), relationship = 'many-to-many')

sf::write_sf(obj = troncons_plus_proches, dsn = "data/outputs/troncons_carthage_plus_proches.gpkg")

troncons_carthage_plus_proche <-
  sf::read_sf(dsn = "data/outputs/troncons_carthage_plus_proches.gpkg")

qa <- qa %>%
  st_drop_geometry() %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH))

q5 <- q5 %>%
  st_drop_geometry() %>%
  mutate(ID_BDCARTH = as.character(ID_BDCARTH))

pe <- pe %>%
  left_join(y = troncons_plus_proches, join_by(cdoh_plando == cdoh_plando)) %>%
  unique()

pe <- pe %>%
  left_join(y = qa, join_by(ID_BDCARTH == ID_BDCARTH)) %>%
  left_join(y = q5, join_by(ID_BDCARTH == ID_BDCARTH))

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifiees_20231123_unique.gpkg")

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifiees_20231123.gpkg")

# Jointure des CD topage et attributs aux plans d'eau ----

## sous-jeu de données sur les BV qui intersectent des troncons et des plandos ----

bv_avec_ce_topage <- ce_topage %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_ce <- intersect(bv_avec_ce_topage,
                             bv_avec_plandos)

# Très long
troncons_topage_plus_proches <- identifier_troncons_topage_les_plus_proches (sf_plandos = pe,
                                                       sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_ce),
                                                       sf_troncons = ce_topage)

troncons_topage_plus_proches <- troncons_topage_plus_proches %>% 
  left_join(y = ce_topage %>% mutate(cdoh_ce = as.character(cdoh_ce)) %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  filter(StreamOrde == max(StreamOrde)) %>% # éventuellement choisir un autre filtre
  ungroup() %>%
  unique()

troncons_topage_plus_proche <- troncons_topage_plus_proche %>%
  group_by(cdoh_plando) %>%
  slice(1)

sf::write_sf(obj = troncons_topage_plus_proches, dsn = "data/outputs/troncons_topage_plus_proches.gpkg")

troncons_topage_plus_proche <-
  sf::read_sf(dsn = "data/outputs/troncons_topage_plus_proches.gpkg")

# Calcul des distances à la source topage ----

## sous-jeu de données sur les BV qui intersectent des sources et des plandos ----

bv_avec_source <- sources %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_avec_plandos <- pe %>%
  st_join(bv) %>%
  filter(!is.na(cdoh_bv)) %>%
  pull(cdoh_bv) %>%
  unique()

bv_selection_sources <- intersect(bv_avec_source,
                                 bv_avec_plandos)

source_plus_proche <- identifier_source_la_plus_proche (sf_plandos = pe,
                                                               sf_bv = bv %>% filter(cdoh_bv %in% bv_selection_sources),
                                                               sf_sources = sources)

source_plus_proche <- source_plus_proche %>% 
  left_join(y = sources %>% st_drop_geometry()) %>% 
  group_by(cdoh_plando) %>% 
  ungroup() %>%
  unique()

source_plus_proche <- source_plus_proche %>%
  group_by(cdoh_plando) %>%
  slice(1)

sf::write_sf(obj = source_plus_proche, dsn = "data/outputs/sources_plus_proches.gpkg")

source_plus_proche <-
  sf::read_sf(dsn = "data/outputs/sources_plus_proches.gpkg")

# Jointure des troncons et source plus proche à la couche pe ----

pe <- pe %>%
  left_join(troncons_topage_plus_proche,
            by = c("cdoh_plando" = "cdoh_plando")) %>%
  left_join(troncons_carthage_plus_proche,
            by = c("cdoh_plando" = "cdoh_plando")) %>%
  left_join(source_plus_proche,
            by = c("cdoh_plando" = "cdoh_plando"))

# Ajouts et qualification de l'attribut "SAGE" ----

sage_par_pe <- pe_decoup_sage %>%
  select(-starts_with("longueur"), -starts_with("surface")) %>%
  sf::st_drop_geometry()%>%
  group_by(cdoh_plando) %>%
  summarise(nom_sage = paste(unique(nom_sage), collapse = ', ')) %>% 
  select(cdoh_plando, nom_sage)

sf::write_sf(obj = sage_par_pe, dsn = "data/outputs/sage_par_pe_20231130.gpkg")

# Ajouts et qualification de l'attribut "ME" ----

me_par_pe <- pe_decoup_me %>%
  select(-starts_with("longueur"), -starts_with("surface")) %>%
  sf::st_drop_geometry()%>%
  group_by(cdoh_plando) %>%
  summarise(cd_me = paste(unique(cdeumassed), collapse = ', ')) %>% 
  select(cdoh_plando, cd_me)

sf::write_sf(obj = me_par_pe, dsn = "data/outputs/me_par_pe_20231130.gpkg")

# Ajouts et qualification de l'attribut "commune" ----

commune_par_pe <- pe_decoup_com %>% 
  select(-starts_with("longueur"), -starts_with("surface")) %>%
  sf::st_drop_geometry()%>%
  group_by(cdoh_plando) %>%
  summarise(nom_communes = paste(unique(nom_com), collapse = ', ')) %>%
  select(cdoh_plando, nom_communes)

# Jointure des sage, me et communes à la couche pe ----

pe <- pe %>%
  left_join(sage_par_pe,
            by = c("cdoh_plando" = "cdoh_plando")) %>%
  left_join(me_par_pe,
            by = c("cdoh_plando" = "cdoh_plando")) %>%
  left_join(commune_par_pe,
            by = c("cdoh_plando" = "cdoh_plando"))

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifies_20231131.gpkg")

# Sauvegarde 2 ----

save(pe,
     troncons_carthage_plus_proche,
     troncons_topage_plus_proche, 
     source_plus_proche,
     sage_par_pe,
     commune_par_pe,
     me_par_pe,
     file = "data/outputs/w_plando2.RData")

# Jointure de la lithologie simplifiée ----

## Recherche des codes manquants ----

pe_sans_cd_litho <- centroides_pe_qualifies %>%
  filter(lithologie == '' | is.na(lithologie)) 

plus_proche_litho <- sf::st_nearest_feature(x = pe_sans_cd_litho,
                                              y = litho)

dist <- st_distance(pe_sans_cd_litho, litho[plus_proche_litho,], by_element = TRUE)

view(plus_proche_litho)

cd_litho <- pe_sans_cd_litho %>% 
  cbind(dist) %>% 
  cbind(litho[plus_proche_litho,]) %>% 
  select(cdoh_plando,
         litho_la_plus_proche = descr,
         distance_m = dist) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_km = round(distance_m/1000,3))

## Mise à jour du code litho de la couche pe ----

# Très long (env. 2h)

centroides_pe_qualifies <- centroides_pe_qualifies  %>%
  left_join(cd_litho, by = c("cdoh_plando" = "cdoh_plando")) %>%  
  mutate(lithologie = ifelse(
    lithologie == '' | is.na(lithologie),
    litho_la_plus_proche,
    lithologie)) %>%
  distinct() %>%
  select(-litho_la_plus_proche, -distance_m) %>%
  mutate(lithologie_simplifiee = lithologie)

sf::write_sf(obj = centroides_pe_qualifies, dsn = "data/outputs/centroides_pe_litho_20231128.gpkg")

# Jointure de la géologie BD Charm ----

centroides_pe_qualifies <- centroides_pe_qualifies %>%
  st_join(charm_zone_etude) %>% 
  mutate(geologie = geol) %>%
  select(-geol) %>%
  distinct()

## Recherche des codes geol manquants ----

pe_sans_cd_geol <- centroides_pe_qualifies %>%
  filter(geologie == '' | is.na(geologie)) 

plus_proche_geol <- sf::st_nearest_feature(x = pe_sans_cd_geol,
                                           y = charm_zone_etude)

dist2 <- st_distance(pe_sans_cd_geol, charm_zone_etude[plus_proche_geol,], by_element = TRUE)

view(plus_proche_geol)

cd_geol <- pe_sans_cd_geol %>% 
  cbind(dist2) %>% 
  cbind(charm_zone_etude[plus_proche_geol,]) %>% 
  select(cdoh_plando,
         geol_la_plus_proche = geol,
         distance2_m = dist2) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance2_km = round(distance2_m/1000,3))

## Mise à jour du code geol de la couche pe ----

# Très long (env. 2h)

centroides_pe_qualifies <- centroides_pe_qualifies  %>%
  left_join(cd_geol, by = c("cdoh_plando" = "cdoh_plando")) %>%  
  mutate(geologie = ifelse(
    geologie == '' | is.na(geologie),
    geol_la_plus_proche,
    geologie)) %>%
  distinct() %>%
  select(-geol_la_plus_proche, -distance2_m) 

sf::write_sf(obj = centroides_pe_qualifies, dsn = "data/outputs/centroides_pe_geologie.gpkg")

# Jointure de la lithologie simplifiée ----

pe <- pe %>%
  left_join(centroides_pe_qualifies %>% sf::st_drop_geometry(),
            by = c("cdoh_plando" = "cdoh_plando"))

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifies_20231131.gpkg")

# Ajouts et qualification de l'attribut "zhp" ----

pe <- pe %>%
  left_join(zhp,
            by = c("cdoh_plando" = "cdoh_plando"))

# Ajouts et qualification de l'attribut "connecte" ----

## Ajouts et qualification de l'attribut "connecte_nappe" ----

pe <- pe %>%
  st_join(nappe_charm, 
          largest = T) %>% 
  mutate(connecte_nappe = case_when(
    !is.na(nappe_charm) ~ '1',
    is.na(nappe_charm) ~ '0')) %>%
  distinct() %>% 
  select(-distance_km, -distance2_km, -geol, -nappe_charm)  %>%
  mutate(connecte_nappe = as.numeric(connecte_nappe)) 
  

## Ajouts et qualification de l'attribut "connecte_source" ----

pe <- pe %>%
  mutate(connecte_source = case_when(
    distance_source <= 50 ~ '1',
    is.na(distance_source)| distance_source > 50 ~ '0')) %>%
  mutate(connecte_source = as.numeric(connecte_source)) 


## Ajouts et qualification de l'attribut "connecte_rh" ----

pe <- pe %>%
  mutate(connecte_lh = case_when(
    !is.na(longueur_topage_intersecte) ~ '1',
    is.na(longueur_topage_intersecte) ~ '0')) %>%
  mutate(connecte_source = as.numeric(connecte_source)) 

## Ajouts et qualification de l'attribut "connecte_rh" ----

pe <- pe %>%
  mutate(connecte_rh = ifelse(
    (connecte_nappe == 0 & connecte_source == 0 & connecte_lh == 0),
    0, 1))

# Jointure des ROE ----


## Fusionner les ROE par bassin ---- 

roe_total <- dplyr::bind_rows(roe_lb, roe_sn, roe_nr)

## Basculer le ROE en sf_geom ----

roe_geom <- roe_total %>% 
  filter(!is.na(x_l93)) %>%
  st_as_sf(coords = c("x_l93", "y_l93"), remove = FALSE, crs = 2154) %>%
  select(identifiant_roe)

## Affectation ROE le plus proche ----

plus_proche_roe <- sf::st_nearest_feature(x = pe,
                                         y = roe_geom)

distance_roe <- st_distance(pe,
                        roe_geom[plus_proche_roe,],
                        by_element = TRUE)

pe_roe <- pe %>% 
  cbind(distance_roe) %>% 
  cbind(roe_geom[plus_proche_roe,]) %>% 
  st_drop_geometry() %>% 
  mutate(distance_roe = round(distance_roe)) %>%
  select(cdoh_plando, identifiant_roe, distance_roe, longueur_topage_intersecte) %>%
  sf::st_drop_geometry() %>%
  units::drop_units()

pe <- pe %>% 
  left_join(y = pe_roe %>% filter(!is.na(longueur_topage_intersecte) &
                                         distance_roe <= 20))

pe <- pe %>%
  left_join(y = roe_total %>% 
              select(identifiant_roe, statut_nom, type_nom, fpi_nom1, usage_nom1, hauteur_chute_etiage),
            join_by('identifiant_roe' == 'identifiant_roe'))

sf::write_sf(obj = pe, dsn = "data/outputs/pe_qualifies_20240220.gpkg")

