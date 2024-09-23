# VERSION FINALISEE AU 20231207
## En cours de création

## CREER DES GRAPHS D'ANALYSE A PARTIR DES REFERENTIELS pe ET TERRITOIRES ----

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
library(kableExtra)


## Chargement des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20240220.gpkg")

dprt <-
  sf::read_sf(dsn = "data/departements.gpkg") %>%
  st_transform(crs = 2154) %>%
  mutate(surface_km2 = st_area(geom)/1000000) %>%
  units::drop_units() %>%
  mutate(insee_reg = ifelse((insee_dep == 22 |
                               insee_dep == 29 |
                               insee_dep == 35 |
                               insee_dep == 56), '53', '52'))
         
         

bv_me <-
  sf::read_sf(dsn = "data/outputs/bv_me_decoup_20240110.gpkg")

sage <-
  sf::read_sf(dsn = "data/sages_zone_etude.gpkg")

pe_decoup_me <-
  sf::read_sf(dsn = "data/outputs/pe_decoup_me_20240215.gpkg")

## PE selon leur surface ----

histo_surface_pe <-
  ggplot(data = pe, 
         aes(x = surface_m2/10000)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + labs(
    x = "Surface du Plan d'eau (Ha)",
    y = "Nombre de Plan d'eau",
    title = "Répartition des Plans d'eau selon leur surface", width=40)

histo_surface_pe

histo_prct_intercept_pe <-
  ggplot(data = bv_me, 
         aes(x = longueur_topage_intersecte_pe_tot*100 /longueur_ce_topage)) + 
  geom_histogram(fill="#2374ee")  + labs(
    x = "Ration du linéaire intercepté (%)",
    y = "Nombre de ME",
    title = "Répartition des masses d'eau selon leur ratio de linéaire intercepté", width=40)

histo_prct_intercept_pe


## Bassins versant selon leur surface et leur pesistance ----
# pti bug 'aes(x = ...' ne marche qu'avec les classes de surface (classe_surface) et non avec une distribution des surfaces (surface_ha)

histo_pe_persistance_surface <- 
  ggplot(data = pe_decoup_me %>% units::drop_units(), 
         aes(x = cdeumassed, y = sum(surface_intersect)/surface_me)) +
  geom_col(aes(fill = Persistanc), width = 0.7) + 
  scale_fill_manual(values = c( "#18d0f0", "#2374ee", "#fb01ff"))+
  labs(
    x = "Surface",
    y = "Surfaces cumulées",
    title = str_wrap("Surface cumulée de PE selon leur surface et leur persistance", width=50))

histo_pe_persistance_surface

# Tableur répartition PE selon type et territoire admin ----

synth_pe <- pe %>%
  filter(cd_dprt %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85)) %>%
  sf::st_drop_geometry() %>% 
  select(cd_dprt, Persistanc, StreamOrde, zone_marais, mare, surface_m2) %>%
  as.data.frame() %>%
  group_by(cd_dprt) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_tot = n(), 
            surf_pe_tot = sum(surface_m2/1000000)) 

synth_pe_hors_marais <-   pe %>%
  filter(cd_dprt %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  select(cd_dprt, Persistanc, StreamOrde, zone_marais, mare, surface_m2) %>%
  as.data.frame() %>%
  group_by(cd_dprt) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_marais = n(), 
            surf_pe_hors_marais = sum(surface_m2/1000000))  

synth_pe_hors_mares <- 
  pe %>%
  filter(cd_dprt %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           mare == 0 ) %>%
  sf::st_drop_geometry() %>% 
  select(cd_dprt, Persistanc, StreamOrde, zone_marais, mare, surface_m2) %>%
  as.data.frame() %>%
  group_by(cd_dprt) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares = n(), 
            surf_pe_hors_mares = sum(surface_m2/1000000))  

synth_pe_mares_marais <- 
  pe %>%
  filter(cd_dprt %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           mare == 0 &
           zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  select(cd_dprt, Persistanc, StreamOrde, zone_marais, mare, surface_m2) %>%
  as.data.frame() %>%
  group_by(cd_dprt) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares_marais = n(), 
            surf_pe_hors_mares_marais = sum(surface_m2/1000000))  

synth_pe_mares_marais_perm <- 
  pe %>%
  filter(cd_dprt %in% c(22, 29, 35, 44, 49, 53, 56, 72, 85) &
           mare == 0 &
           zone_marais == 0 & 
           Persistanc == 'permanent') %>%
  sf::st_drop_geometry() %>% 
  select(cd_dprt, Persistanc, StreamOrde, zone_marais, mare, surface_m2) %>%
  as.data.frame() %>%
  group_by(cd_dprt) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares_marais_perm = n(), 
            surf_pe_hors_mares_marais_perm = sum(surface_m2/1000000))  

dprt <- dprt %>%
  st_drop_geometry() %>%
  mutate(nom = nom_departement) %>%
  left_join(synth_pe, by = c("insee_dep" = "cd_dprt")) %>%
  left_join(synth_pe_hors_marais, by = c("insee_dep" = "cd_dprt")) %>%
  left_join(synth_pe_hors_mares, by = c("insee_dep" = "cd_dprt")) %>%
  left_join(synth_pe_mares_marais, by = c("insee_dep" = "cd_dprt")) %>%
  left_join(synth_pe_mares_marais_perm, by = c("insee_dep" = "cd_dprt"))
  
synth_region <- dprt %>%
  group_by(insee_reg) %>%
  summarise(surface_km2 = sum(surface_km2),
            nb_pe_tot = sum(nb_pe_tot),
            surf_pe_tot = sum(surf_pe_tot),
            nb_pe_hors_marais = sum(nb_pe_hors_marais),
            surf_pe_hors_marais = sum(surf_pe_hors_marais),
            nb_pe_hors_mares = sum(nb_pe_hors_mares),
            surf_pe_hors_mares = sum(surf_pe_hors_mares),
            nb_pe_hors_mares_marais = sum(nb_pe_hors_mares_marais),
            surf_pe_hors_mares_marais  = sum(surf_pe_hors_mares_marais),
            nb_pe_hors_mares_marais_perm = sum(nb_pe_hors_mares_marais_perm),
            surf_pe_hors_mares_marais_perm = sum(surf_pe_hors_mares_marais_perm))

synth_inter_region <- synth_region %>%
  summarise(nom = 'Inter-region',
            surface_km2 = sum(surface_km2),
            nb_pe_tot = sum(nb_pe_tot),
            surf_pe_tot = sum(surf_pe_tot),
            nb_pe_hors_marais = sum(nb_pe_hors_marais),
            surf_pe_hors_marais = sum(surf_pe_hors_marais),
            nb_pe_hors_mares = sum(nb_pe_hors_mares),
            surf_pe_hors_mares = sum(surf_pe_hors_mares),
            nb_pe_hors_mares_marais = sum(nb_pe_hors_mares_marais),
            surf_pe_hors_mares_marais  = sum(surf_pe_hors_mares_marais),
            nb_pe_hors_mares_marais_perm = sum(nb_pe_hors_mares_marais_perm),
            surf_pe_hors_mares_marais_perm = sum(surf_pe_hors_mares_marais_perm))


  
  synthese_territoire <-
  dplyr::bind_rows(dprt %>%
                     select(-insee_dep, -insee_reg), 
                   synth_region %>%
                     mutate(nom = ifelse(insee_reg == 52, 'Pays de la Loire', 'Bretagne')),
                   synth_inter_region) %>%
    select(-nom_departement,-insee_reg)

synthese_territoire <- synthese_territoire %>%
  mutate(dens_pe_tot = nb_pe_tot/surface_km2,
         dens_surf_pe_tot = (surf_pe_tot)*100/surface_km2,
         dens_pe_hors_marais = nb_pe_hors_marais/surface_km2,
         dens_surf_pe_hors_marais = (surf_pe_hors_marais)*100/surface_km2,
         dens_pe_hors_mares = nb_pe_hors_mares/surface_km2,
         dens_surf_pe_hors_mares = (surf_pe_hors_mares)*100/surface_km2,
         dens_pe_hors_mares_marais = nb_pe_hors_mares_marais/surface_km2,
         dens_surf_pe_hors_mares_marais = (surf_pe_hors_mares_marais)*100/surface_km2,
         dens_pe_hors_mares_marais_perm = nb_pe_hors_mares_marais_perm/surface_km2,
         dens_surf_pe_hors_mares_marais_perm = (surf_pe_hors_mares_marais_perm)*100/surface_km2) %>%
  select(nom, 
        surface_km2,
        nb_pe_tot, 
        dens_pe_tot,
        surf_pe_tot,
        dens_surf_pe_tot,
        nb_pe_hors_marais,
        dens_pe_hors_marais,
        surf_pe_hors_marais,
        dens_surf_pe_hors_marais,
        nb_pe_hors_mares,
        dens_pe_hors_mares,
        surf_pe_hors_mares,
        dens_surf_pe_hors_mares,
        nb_pe_hors_mares_marais,
        dens_pe_hors_mares_marais,
        surf_pe_hors_mares_marais,
        dens_surf_pe_hors_mares_marais,
        nb_pe_hors_mares_marais_perm,
        dens_pe_hors_mares_marais_perm,
        surf_pe_hors_mares_marais_perm,
        dens_surf_pe_hors_mares_marais_perm)
  
openxlsx::write.xlsx(synthese_territoire, file = "data/outputs/synthese_territoires_20240918.xls" )

# Liste SAGE unique par PE ----

centroide_pe <- pe %>%
  st_centroid()

pe_sage <- centroide_pe %>% 
  st_intersection(sage) %>% # découpage des plando selon les masses d'eau
  st_drop_geometry() %>% 
  mutate(nom_du_sage = NOM) %>%
  select(cdoh_plando,  # sélection des variables à conserver
         nom_du_sage,
         Persistanc, 
         StreamOrde, 
         zone_marais, 
         mare,
         surface_m2) 

# Tableur répartition PE selon type et SAGE ----

synth_pe_sage <- 
  pe_sage %>%
  filter(!is.na(nom_du_sage)) %>%
  sf::st_drop_geometry() %>% 
  as.data.frame() %>%
  group_by(nom_du_sage) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_tot = n(), 
            surf_pe_tot = sum(surface_m2/1000000)) 

synth_pe_hors_marais_sage <- 
  pe_sage %>%
  filter(zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  as.data.frame() %>%
  group_by(nom_du_sage) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_marais = n(), 
            surf_pe_hors_marais = sum(surface_m2/1000000))

synth_pe_hors_mares_sage <- 
  pe_sage %>%
  filter(mare == 0 ) %>%
  sf::st_drop_geometry() %>% 
  as.data.frame() %>%
  group_by(nom_du_sage) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares = n(), 
            surf_pe_hors_mares = sum(surface_m2/1000000)) 

synth_pe_mares_marais_sage <- 
  pe_sage %>%
  filter(mare == 0 & zone_marais == 0) %>%
  sf::st_drop_geometry() %>% 
  as.data.frame() %>%
  group_by(nom_du_sage) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares_marais = n(), 
            surf_pe_hors_mares_marais = sum(surface_m2/1000000)) 

synth_pe_mares_marais_perm_sage <- 
  pe_sage %>%
  filter(mare == 0 & zone_marais == 0 & Persistanc == 'permanent') %>%
  sf::st_drop_geometry() %>% 
  as.data.frame() %>%
  group_by(nom_du_sage) %>%
  sf::st_drop_geometry() %>% 
  summarise(nb_pe_hors_mares_marais_perm = n(), 
            surf_pe_hors_mares_marais_perm = sum(surface_m2/1000000)) 

synthese_sage <- sage %>%
  mutate(surface_km2 = st_area(geom)/1000000) %>%
  units::drop_units() %>%
  st_drop_geometry() %>%
  select(NOM, surface_km2) %>%
  left_join(synth_pe_sage, by = c("NOM" = "nom_du_sage")) %>%
  left_join(synth_pe_hors_marais_sage, by = c("NOM" = "nom_du_sage")) %>%
  left_join(synth_pe_hors_mares_sage, by = c("NOM" = "nom_du_sage")) %>%
  left_join(synth_pe_mares_marais_sage, by = c("NOM" = "nom_du_sage")) %>%
  left_join(synth_pe_mares_marais_perm_sage, by = c("NOM" = "nom_du_sage"))

# Calcul des densités ----

synthese_sage <- synthese_sage %>%
  mutate(nom= NOM,
         dens_pe_tot = nb_pe_tot/surface_km2,
         dens_surf_pe_tot = (surf_pe_tot)*100/surface_km2,
         dens_pe_hors_marais = nb_pe_hors_marais/surface_km2,
         dens_surf_pe_hors_marais = (surf_pe_hors_marais)*100/surface_km2,
         dens_pe_hors_mares = nb_pe_hors_mares/surface_km2,
         dens_surf_pe_hors_mares = (surf_pe_hors_mares)*100/surface_km2,
         dens_pe_hors_mares_marais = nb_pe_hors_mares_marais/surface_km2,
         dens_surf_pe_hors_mares_marais = (surf_pe_hors_mares_marais)*100/surface_km2,
         dens_pe_hors_mares_marais_perm = nb_pe_hors_mares_marais_perm/surface_km2,
         dens_surf_pe_hors_mares_marais_perm = (surf_pe_hors_mares_marais_perm)*100/surface_km2) %>%
  select(nom, 
         surface_km2,
         nb_pe_tot, 
         dens_pe_tot,
         surf_pe_tot,
         dens_surf_pe_tot,
         nb_pe_hors_marais,
         dens_pe_hors_marais,
         surf_pe_hors_marais,
         dens_surf_pe_hors_marais,
         nb_pe_hors_mares,
         dens_pe_hors_mares,
         surf_pe_hors_mares,
         dens_surf_pe_hors_mares,
         nb_pe_hors_mares_marais,
         dens_pe_hors_mares_marais,
         surf_pe_hors_mares_marais,
         dens_surf_pe_hors_mares_marais,
         nb_pe_hors_mares_marais_perm,
         dens_pe_hors_mares_marais_perm,
         surf_pe_hors_mares_marais_perm,
         dens_surf_pe_hors_mares_marais_perm)

openxlsx::write.xlsx(synthese_sage, file = "data/outputs/synthese_sage.xls" )

# Fusion des tables admin et SAGE ----

synthese_territoire_vf <- 
  dplyr::bind_rows(synthese_territoire, synthese_sage) 

openxlsx::write.xlsx(synthese_territoire_vf, file = "data/outputs/synthese_indicateurs_territoires_20240918.xls" )


synthese_territoire_vf %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12) 

