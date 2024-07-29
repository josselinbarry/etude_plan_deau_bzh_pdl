# Library ----
library(tidyverse)
library(sf)
library(units)
library(mapview)
library(dplyr)
library(kableExtra)
library(png)
library(rstatix)
library(ggpubr)

source(file = "R/compter_sommer_surfaces_dans_polygone.R")


# Import des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20240220.gpkg")%>%
  st_transform(crs = 2154)

litho <- sf::read_sf(dsn = "data/carte_lithoplogique_simplifiee_vectorisee_perimetre_etude.gpkg") %>%
  st_transform(crs = 2154)

zone_etude <- sf::read_sf(dsn = "data/outputs/zone_etude.gpkg") %>%
  st_transform(crs = 2154) 

bv_me <- sf::read_sf(dsn = "data/outputs/bv_me_qualifie_20240715.gpkg") %>%
  st_transform(crs = 2154) 

rpg53 <- sf::read_sf(dsn = "data/rpg_53.gpkg") %>%
  st_transform(crs = 2154)

rpg52 <- sf::read_sf(dsn = "data/rpg_52.gpkg") %>%
  st_transform(crs = 2154)

rpg24 <- sf::read_sf(dsn = "data/rpg_24.gpkg") %>%
  st_transform(crs = 2154)

rpg28 <- sf::read_sf(dsn = "data/rpg_28.gpkg") %>%
  st_transform(crs = 2154)

rpg75 <- sf::read_sf(dsn = "data/rpg_75.gpkg") %>%
  st_transform(crs = 2154)

communes <- sf::read_sf(dsn = "data/outputs/communes_qualifiees_20240216.gpkg") %>%
  st_transform(crs = 2154)

# Plan d'eau et lithologie ----

## Calcul de densités ----

### Litho de la zone d'étude ----

litho_zone_etude <- litho %>%
  st_intersection(zone_etude) %>%
  mutate(surface_intersect = st_area(.), 
         lithologie = descr) %>%
  st_drop_geometry() %>%
  group_by(lithologie) %>%
  summarise(surface_km2 = sum(surface_intersect)/1000000) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  units::drop_units()

### Calcul associé des densités de PE par type de litho ----

surf_pe_tot_litho <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = lithologie,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_litho <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = lithologie,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

### Jointure et calcul de densités ----

litho_densite_pe <- litho_zone_etude %>% 
  st_drop_geometry() %>%
  left_join(surf_pe_tot_litho, 
            join_by(lithologie == lithologie)) %>%
  left_join(surf_pehm_litho, 
            join_by(lithologie == lithologie)) %>%
  mutate(dens_num_pe = nb_pe_tot / surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2,
         dens_num_pehm = nb_pehm_tot / surface_km2, 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / surface_km2)

sf::write_sf(obj = litho_densite_pe, dsn = "data/outputs/densite_per_lithlogie_20240718.gpkg")

table_litho_densite_pe <- litho_densite_pe %>%
  st_drop_geometry() %>%
  mutate("Lithologie simplifiée" = lithologie,
         "Surface (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau (/km²)" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors marais (/km²)" = round(dens_num_pehm, 2),
         ) %>%
  select("Lithologie simplifiée", 
         "Surface (km²)", 
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau (/km²)",
         "Nombre de plans d'eau hors marais",
         "Densité numérique en plans d'eau hors marais (/km²)")

### Représentations graphiques ----

#### Densite numérique ----

densite_numerique_pe_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_num_pe,
           x = lithologie)) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_numerique_pehm_litho <-
ggplot(litho_densite_pe,
       aes(y = dens_num_pehm,
           x = lithologie)) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Densite surfacique ----

densite_surfacique_pe_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_surf_pe,
           x = lithologie)) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_surfacique_pehm_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_surf_pehm,
           x = lithologie)) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

## Taille des plans d'eau par rapport à la lithologie ---- 

pe_taille <- pe %>%
  filter(zone_marais== 0 & mare == 0 ) %>%
  st_drop_geometry() %>%
  group_by(lithologie) %>%
  summarise(surface_moy_pe = mean(surface_m2),
            surface_med_pe = median(surface_m2))

surface_moyenne_pehm_litho <-
  ggplot(pe_taille,
       aes(y = surface_moy_pe,
           x = fct_rev(lithologie))) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Surface moyenne des plans d'eau",
       title = "Surface moyenne des plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

surface_mediane_pehm_litho <-
ggplot(pe_taille,
       aes(y = surface_med_pe,
           x = fct_rev(lithologie))) + 
  geom_point() +
  labs(x = "Lithologie",
       y = "Surface médiane des plans d'eau",
       title = "Surface médiane des plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

# Plans d'eau et régime hydrologique ----

## Répartition des plans d'eau selon le rapport du QMNA5 sur le module ----

pe_q <- pe %>% 
  filter(!is.na(QAMOY_MN) & 
           QAMOY_MN >= 0) %>%
  mutate(ratio_q5_qa = case_when(
    (Q5MOY_MN/QAMOY_MN) >= 0 & (Q5MOY_MN/QAMOY_MN) <= 0.025 ~ '<= 40ème',
    (Q5MOY_MN/QAMOY_MN) > 0.025 & (Q5MOY_MN/QAMOY_MN) <= 0.05 ~ 'Entre le 20ème et le 40ème',
    (Q5MOY_MN/QAMOY_MN) > 0.05 & (Q5MOY_MN/QAMOY_MN) <= 0.1 ~ 'Entre le 10ème et le 20ème',
    (Q5MOY_MN/QAMOY_MN) > 0.1 ~ '> 10ème')) %>%
  st_drop_geometry()

repartition_pe_rh <-
  ggplot(data = pe_q, 
       aes(x = Q5MOY_MN/QAMOY_MN)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module",
       y = "Nombre de plans d'eau",
       title = str_wrap("Répartition des plans d'eau selon leur régime hydrologique", width=40),
       subtitle = "Bretagne et Pays de la Loire élargis")

repartition_pehm_rh <-
  ggplot(data = pe_q %>%
        filter(mare == 0 & zone_marais == 0), 
       aes(x = Q5MOY_MN/QAMOY_MN)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module",
       y = "Nombre de plans d'eau",
       title = str_wrap("Répartition des plans d'eau selon leur régime hydrologique", width=40),
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")

## Répartition des BV ME selon le rapport du QMNA5 sur le module ----

repartition_bv_rh <-
  ggplot(data = bv_me %>% filter(!is.na(QAMOY_max) & QAMOY_max>=0), 
       aes(x = Q5MOY_max/QAMOY_max)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module",
       y = "Nombre de bassins versant",
       title = str_wrap("Répartition des bassins versant selon leur régime hydrologique", width=40),
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")

## Densité numérique de plans d'eau selon le rapport du QMNA5 sur le module ----

bv_me_q <- bv_me %>%
  mutate(ratio_q5_qa = case_when(
    (Q5MOY_max/QAMOY_max) >= 0 & (Q5MOY_max/QAMOY_max) <= 0.025 ~ '<= 40ème',
    (Q5MOY_max/QAMOY_max) > 0.025 & (Q5MOY_max/QAMOY_max) <= 0.05 ~ 'Entre le 20ème et le 40ème',
    (Q5MOY_max/QAMOY_max) > 0.05 & (Q5MOY_max/QAMOY_max) <= 0.1 ~ 'Entre le 10ème et le 20ème',
    (Q5MOY_max/QAMOY_max) > 0.1 ~ '> 10ème')) %>%
  group_by(ratio_q5_qa) %>%
  summarise(surface_km2 = sum(surface_me)/1000000) %>%
  select(ratio_q5_qa, surface_km2) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  st_drop_geometry()

surf_pe_tot_q <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_q %>% 
      units::drop_units() %>% 
      st_drop_geometry(),
    var_id_polygone = ratio_q5_qa,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

surf_pehm_q <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_q %>% 
      units::drop_units() %>% 
      st_drop_geometry() %>%
      filter(mare == 0),
    var_id_polygone = ratio_q5_qa,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )


bv_me_q <- bv_me_q %>%
  left_join(surf_pe_tot_q, join_by(ratio_q5_qa == ratio_q5_qa)) %>%
  left_join(surf_pehm_q, join_by(ratio_q5_qa == ratio_q5_qa)) %>%
  mutate(dens_num_pe = nb_pe_tot/surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2, 
         dens_num_pehm = nb_pehm_tot/surface_km2, 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / surface_km2) %>%
  filter(ratio_q5_qa != '')

table_bv_me_q <- bv_me_q %>%
  mutate("Type de régime hydrologique (q5/qa)" = ratio_q5_qa,
         "Surface (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau (/km²)" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors mares et marais (/km²)" = round(dens_num_pehm, 2),
  ) %>%
  select("Type de régime hydrologique (q5/qa)", 
         "Surface (km²)", 
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau (/km²)",
         "Nombre de plans d'eau hors mares et marais",
         "Densité numérique en plans d'eau hors mares et marais (/km²)")

table_bv_me_q <- table_bv_me_q %>% 
  mutate("Type de régime hydrologique (q5/qa)" = fct_relevel(c("> 10ème", "Entre le 10ème et le 20ème", "Entre le 20ème et le 40ème","<= 40ème")))

bv_me_q <- bv_me_q %>% 
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa, c("> 10ème", "Entre le 10ème et le 20ème", "Entre le 20ème et le 40ème","<= 40ème")))

densite_numerique_pe_rh <-
  ggplot(bv_me_q,
       aes(y = dens_num_pe,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_numerique_pehm_rh <-
  ggplot(bv_me_q,
       aes(y = dens_num_pehm,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_surfacique_pe_rh <-
  ggplot(bv_me_q,
       aes(y = dens_surf_pe,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_surfacique_pehm_rh <-
ggplot(bv_me_q,
       aes(y = dens_surf_pehm,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

## Taille des plans d'eau par rapport au régime hydrologique ---- 

pe_q <- pe_q %>% 
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa, c("> 10ème", "Entre le 10ème et le 20ème", "Entre le 20ème et le 40ème","<= 40ème")))

pe_taille_q <- pe_q %>%
  st_drop_geometry() %>%
  group_by(ratio_q5_qa) %>%
  summarise(surface_moy_pe = mean(surface_m2),
            surface_med_pe = median(surface_m2)) %>%
  filter(!is.na(ratio_q5_qa))

pehm_taille_q <- pe_q %>%
  st_drop_geometry() %>%
  filter(zone_marais== 0 & mare == 0 ) %>%  
  group_by(ratio_q5_qa) %>%
  summarise(surface_moy_pehm = mean(surface_m2),
            surface_med_pehm = median(surface_m2))%>%
  filter(!is.na(ratio_q5_qa))

surface_moyenne_pe_rh <-
ggplot(pe_taille_q,
       aes(y = surface_moy_pe,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Surface moyenne des plans d'eau",
       title = "Surface moyenne des plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

surface_moyenne_pehm_rh <-
  ggplot(pehm_taille_q,
       aes(y = surface_moy_pehm,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Surface moyenne des plans d'eau",
       title = "Surface moyenne des plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

surface_mediane_pe_rh <-
  ggplot(pe_taille_q,
       aes(y = surface_med_pe,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Surface médiane des plans d'eau ",
       title = "Surface médiane des plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

surface_mediane_pehm_rh <-
  ggplot(pehm_taille_q,
       aes(y = surface_med_pehm,
           x = ratio_q5_qa)) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Surface médiane des plans d'eau ",
       title = "Surface médiane des plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

surface_pehm_rh <-
  ggplot(pe_q %>% filter(!is.na(QAMOY_MN) & Q5MOY_MN/QAMOY_MN >= 0 & surface_m2 < 15000000),
         aes(y = (Q5MOY_MN*100)/QAMOY_MN,
             x = surface_m2)) + 
  geom_point() +
  labs(x = "Surface des plans d'eau ",
       y = "Ratio du Qmna5 sur le module",
       title = "Surface des plans d'eau selon leur régime hydrologique",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

# Densite numerique plus forte sur certains types de ME (cotiere, transition + ?) ----

bv_me_type <- bv_me %>%
  mutate(type = case_when(
    substr(cdeumassed, 4,4) == 'R' ~ 'cours deau',
    substr(cdeumassed, 4,4) == 'L' ~ 'plan deau',
    substr(cdeumassed, 4,4) == 'C' ~ 'cotiere',
    substr(cdeumassed, 4,4) == 'T' ~ 'transition')) %>%
  group_by(type) %>%
  summarise(surface_km2 = sum(surface_me)/1000000) %>%
  select(type, surface_km2) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  st_drop_geometry()

pe_type <- pe %>%
  mutate(type = case_when(
    substr(cd_me, 4,4) == 'R' ~ 'cours deau',
    substr(cd_me, 4,4) == 'L' ~ 'plan deau',
    substr(cd_me, 4,4) == 'C' ~ 'cotiere',
    substr(cd_me, 4,4) == 'T' ~ 'transition')) 

surf_pe_type <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_type %>% 
      units::drop_units() %>% 
      st_drop_geometry(),
    var_id_polygone = type,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

surf_pehm_type <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_type %>% 
      units::drop_units() %>% 
      st_drop_geometry() %>%
      filter(mare == 0),
    var_id_polygone = type,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

bv_me_type <- bv_me_type %>%
  left_join(surf_pe_type, join_by(type == type)) %>%
  left_join(surf_pehm_type, join_by(type == type)) %>%
  mutate(dens_num_pe = nb_pe_tot/surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2, 
         dens_num_pehm = nb_pehm_tot/surface_km2, 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / surface_km2) %>%
  filter(type != '')

table_bv_me_type <- bv_me_type %>%
  mutate("Type de masse d'eau" = type,
         "Surface (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau (/km²)" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors mares et marais (/km²)" = round(dens_num_pehm, 2),
  ) %>%
  select("Type de masse d'eau", 
         "Surface (km²)", 
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau (/km²)",
         "Nombre de plans d'eau hors mares et marais",
         "Densité numérique en plans d'eau hors mares et marais (/km²)")

bv_me_type <- bv_me_type %>% 
  mutate(type = fct_rev(as.factor(type)))

densite_numerique_pe_type <-
  ggplot(bv_me_type,
       aes(y = dens_num_pe,
           x = type)) + 
  geom_point() +
  labs(x = "Type de masse d'eau",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_numerique_pehm_type <-
ggplot(bv_me_type,
       aes(y = dens_num_pehm,
           x = type)) + 
  geom_point() +
  labs(x = "Type de masse d'eau",
       y = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_surfacique_pe_type <-
  ggplot(bv_me_type,
       aes(y = dens_surf_pe,
           x = type)) + 
  geom_point() +
  labs(x = "Type de masse d'eau",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

densite_surfacique_pehm_type <-
ggplot(bv_me_type,
       aes(y = dens_surf_pehm,
           x = type)) + 
  geom_point() +
  labs(x = "Type de masse d'eau",
       y = "Densité surfacique de plans d'eau",
       title = "Densité surfacique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

# Densite numerique plus forte en zone de maraichage ? ----

## Construction table BV ME ----

maraichage_53 <- rpg53 %>%
  filter(CODE_CULTU %in% c('FEV', 'PHF', 'AIL', 'ART', 'CAR', 'CEL', 'CHU', 'CCN', 'EPI', 'FRA','LBF', 'MLO', 'NVT', 'OIG', 'RDI', 'POR', 'PVP', 'POT', 'TOM', 'TBT', 'FLA', 'FLP', 'CSS')) %>%
  mutate(surface_m2 = st_area(.)) %>%
  units::drop_units()

maraichage_52 <- rpg52 %>%
  filter(CODE_CULTU %in% c('FEV', 'PHF', 'AIL', 'ART', 'CAR', 'CEL', 'CHU', 'CCN', 'EPI', 'FRA','LBF', 'MLO', 'NVT', 'OIG', 'RDI', 'POR', 'PVP', 'POT', 'TOM', 'TBT', 'FLA', 'FLP', 'CSS')) %>%
  mutate(surface_m2 = st_area(.)) %>%
  units::drop_units()

maraichage_24 <- rpg24 %>%
  filter(CODE_CULTU %in% c('FEV', 'PHF', 'AIL', 'ART', 'CAR', 'CEL', 'CHU', 'CCN', 'EPI', 'FRA','LBF', 'MLO', 'NVT', 'OIG', 'RDI', 'POR', 'PVP', 'POT', 'TOM', 'TBT', 'FLA', 'FLP', 'CSS')) %>%
  mutate(surface_m2 = st_area(.)) %>%
  units::drop_units()

maraichage_28 <- rpg28 %>%
  filter(CODE_CULTU %in% c('FEV', 'PHF', 'AIL', 'ART', 'CAR', 'CEL', 'CHU', 'CCN', 'EPI', 'FRA','LBF', 'MLO', 'NVT', 'OIG', 'RDI', 'POR', 'PVP', 'POT', 'TOM', 'TBT', 'FLA', 'FLP', 'CSS')) %>%
  mutate(surface_m2 = st_area(.)) %>%
  units::drop_units()

maraichage_75 <- rpg75 %>%
  filter(CODE_CULTU %in% c('FEV', 'PHF', 'AIL', 'ART', 'CAR', 'CEL', 'CHU', 'CCN', 'EPI', 'FRA','LBF', 'MLO', 'NVT', 'OIG', 'RDI', 'POR', 'PVP', 'POT', 'TOM', 'TBT', 'FLA', 'FLP', 'CSS')) %>%
  mutate(surface_m2 = st_area(.)) %>%
  units::drop_units()

maraichage <- dplyr::bind_rows(maraichage_52, 
                               maraichage_53,
                               maraichage_24,
                               maraichage_28,
                               maraichage_75,)

maraichage_cd_bv <- maraichage %>% 
  st_intersection(bv_me %>% select(cdeumassed)) %>% 
  mutate(surface_m2 = st_area(.)) %>%
  st_drop_geometry()

maraichage_bv <- maraichage_cd_bv %>%
  group_by(cdeumassed) %>%
  summarise(surface_maraichage_m2 = sum(surface_m2)) %>%
  units::drop_units()

bv_me_maraichage <- bv_me %>%
  left_join(maraichage_bv) %>%
  mutate(proportion_maraichage = (coalesce(surface_maraichage_m2,0) * 100)/surface_me,
         cl_maraichage = case_when(
    proportion_maraichage == 0 ~ 'Pas de maraichage',
    proportion_maraichage > 0 & proportion_maraichage <= 1 ~ '<= 1%',
    proportion_maraichage > 1 & proportion_maraichage <= 5 ~ '1 à 5%',
    proportion_maraichage > 5 & proportion_maraichage <= 10 ~ '5 à 10%',
    proportion_maraichage > 10 & proportion_maraichage <= 20 ~ '10 à 20%',
    proportion_maraichage > 20 ~ '> 20%',
  ))

sf::write_sf(obj = bv_me_maraichage, dsn = "data/outputs/bv_me_maraichage_20240718.gpkg")

classes_maraichage <- bv_me_maraichage %>%
  group_by(cl_maraichage) %>%
  summarise(surface_km2 = sum(surface_me)/1000000,
            nb_pe_tot = sum(coalesce(nb_pe_tot, 0)),
            surf_pe_tot = sum(coalesce(surf_pe_tot, 0)),
            nb_pehm_tot = sum(coalesce(nb_pehm_tot, 0)),
            surf_pehm_tot = sum(coalesce(surf_pehm_tot,0)),
            nb_mares_tot = sum(coalesce(nb_mares_tot,0)),
            surf_mares_tot = sum(coalesce(surf_mares_tot,0)),
            surface_me = sum(surface_me))

table_classes_maraichage <- classes_maraichage %>%
  st_drop_geometry() %>%
  mutate("Proportion de culture maraichère" = cl_maraichage,
         "Surface (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round((surface_km2*100/sum(surface_km2)), 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau (/km²)" = round(nb_pe_tot/surface_km2, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors mares et marais (/km²)" = round(nb_pehm_tot/surface_km2, 2),
  ) %>%
  select("Proportion de culture maraichère", 
         "Surface (km²)", 
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau (/km²)",
         "Nombre de plans d'eau hors mares et marais",
         "Densité numérique en plans d'eau hors mares et marais (/km²)")

table_classes_maraichage <- table_classes_maraichage %>% 
  factor("Proportion de culture maraichère", levels = c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")) %>%
  xtabs(.)

df$culture <- factor(df$culture, levels = c("orge", "ble", "mais"))
tcd<-xtabs(surface~culture+departement, data =df)

## Représentation graphique BV ME----

bv_me_maraichage <- bv_me_maraichage %>% 
  mutate(cl_maraichage = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")))

repartition_bv_maraichage <-
  ggplot(data = bv_me_maraichage, 
         aes(x = proportion_maraichage)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Proportion de surface en activité maraîchère",
       y = "Nombre de bassins versant",
       title = str_wrap("Répartition des masses d'eau selon leur régime hydrologique", width=40),
       subtitle = "Bretagne et Pays de la Loire élargis")


maraichage_densite_numerique_tot <-
  ggplot(bv_me_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(nb_pe_tot, 0)+coalesce(nb_mares_tot,0)/(surface_me/1000000))))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité numérique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

maraichage_densite_numerique_pehm <-
  ggplot(bv_me_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(nb_pehm_tot, 0)/(surface_me/1000000))))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité numérique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

bv_me_maraichage <- bv_me_maraichage %>% 
  mutate(cl_maraichage = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")))

cl_maraichage_densite_numerique_pe <-
  ggplot(classes_maraichage,
         aes(y = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")),
             x = ((coalesce(nb_pe_tot,0)+coalesce(nb_mares_tot,0))/(surface_me/1000000)))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface de la masse d'eau en maraichage",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

cl_maraichage_densite_numerique_pehm <-
  ggplot(classes_maraichage,
         aes(y = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")),
             x = (coalesce(nb_pehm_tot,0)/(surface_me/1000000)))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface de la masse d'eau en maraichage",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

## Construction table commune ----

maraichage_cd_com <- maraichage %>% 
  st_intersection(communes %>% select(code_insee)) %>% 
  mutate(surface_m2 = st_area(.)) %>%
  st_drop_geometry()

maraichage_com <- maraichage_cd_com %>%
  group_by(code_insee) %>%
  summarise(surface_maraichage_m2 = sum(surface_m2)) %>%
  units::drop_units()

com_maraichage <- communes %>%
  left_join(maraichage_com) %>%
  mutate(proportion_maraichage = (coalesce(surface_maraichage_m2,0) * 100)/surface_com,
         cl_maraichage = case_when(
           proportion_maraichage == 0 ~ 'Pas de maraichage',
           proportion_maraichage > 0 & proportion_maraichage <= 1 ~ '<= 1%',
           proportion_maraichage > 1 & proportion_maraichage <= 5 ~ '1 à 5%',
           proportion_maraichage > 5 & proportion_maraichage <= 10 ~ '5 à 10%',
           proportion_maraichage > 10 & proportion_maraichage <= 20 ~ '10 à 20%',
           proportion_maraichage > 20 ~ '> 20%',
         ))

sf::write_sf(obj = com_maraichage, dsn = "data/outputs/com_maraichage_20240718.gpkg")

classes_maraichage_com <- com_maraichage %>%
  group_by(cl_maraichage) %>%
  summarise(surface_km2 = sum(surface_com)/1000000,
            nb_pe_tot = sum(coalesce(nb_pe_tot, 0)),
            surf_pe_tot = sum(coalesce(surf_pe_tot, 0)),
            nb_pehm_tot = sum(coalesce(nb_pehm_tot, 0)),
            surf_pehm_tot = sum(coalesce(surf_pehm_tot,0)),
            nb_mares_tot = sum(coalesce(nb_mares_tot,0)),
            surf_mares_tot = sum(coalesce(surf_mares_tot,0)),
            surface_com = sum(surface_com))

table_classes_maraichage_com <- classes_maraichage_com %>%
  st_drop_geometry() %>%
  mutate("Proportion de culture maraichère" = cl_maraichage,
         "Surface (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round((surface_km2*100/sum(surface_km2)), 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau (/km²)" = round(nb_pe_tot/surface_km2, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors mares et marais (/km²)" = round(nb_pehm_tot/surface_km2, 2),
  ) %>%
  select("Proportion de culture maraichère", 
         "Surface (km²)", 
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau (/km²)",
         "Nombre de plans d'eau hors mares et marais",
         "Densité numérique en plans d'eau hors mares et marais (/km²)")

## Représentation graphique Communes----

bv_me_maraichage <- bv_me_maraichage %>% 
  mutate(cl_maraichage = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")))

repartition_com_maraichage <-
  ggplot(data = com_maraichage, 
         aes(x = proportion_maraichage)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Proportion de surface en activité maraîchère",
       y = "Nombre de communes",
       title = str_wrap("Répartition des communes selon leur régime hydrologique", width=40),
       subtitle = "Bretagne et Pays de la Loire élargis")


maraichage_com_densite_numerique_tot <-
  ggplot(com_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(nb_pe_tot, 0)+coalesce(nb_mares_tot,0)/(surface_com/1000000))))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité numérique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

maraichage_com_densite_numerique_pehm <-
  ggplot(com_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(nb_pe_tot, 0)/(surface_com/1000000))))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité numérique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

maraichage_com_densite_surfacique_tot <-
  ggplot(com_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(surf_pe_tot/1000000, 0)+coalesce(surf_mares_tot/1000000,0)/(surface_com/1000000))))) + 
  geom_point() +
  labs(x = "Densité surfacique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité surfacique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

maraichage_com_densite_surfacique_pehm <-
  ggplot(com_maraichage,
         aes(y = proportion_maraichage,
             x = ((coalesce(surf_pehm_tot/1000000, 0)/(surface_com/1000000))))) + 
    geom_point() +
  labs(x = "Densité surfacique de plans d'eau",
       y = "Proportion de surface en activité de maraichage",
       title = "Densité numérique de plans d'eau selon le degré d'activité maraichère",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))


bv_me_maraichage <- bv_me_maraichage %>% 
  mutate(cl_maraichage = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")))

cl_maraichage_com_densite_numerique_pe <-
  ggplot(classes_maraichage_com,
         aes(y = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")),
             x = ((coalesce(nb_pe_tot,0)+coalesce(nb_mares_tot,0))/(surface_com/1000000)))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface de la commune en maraichage",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

cl_maraichage_com_densite_numerique_pehm <-
  ggplot(classes_maraichage_com,
         aes(y = fct_relevel(cl_maraichage, c("Pas de maraichage", "<= 1%", "1 à 5%","5 à 10%", "10 à 20%", "> 20%")),
             x = (coalesce(nb_pehm_tot, 0)/(surface_com/1000000)))) + 
  geom_point() +
  labs(x = "Densité numérique de plans d'eau",
       y = "Proportion de surface de la commune en maraichage",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

# Test ANOVA ----

## ANOVA litho ----

litho <- litho %>%
  st_drop_geometry()

densnum_geol <- litho_densite_pe %>% left_join(litho, join_by(lithologie == descr)) %>%
  select(value, 
         lithologie,
         dens_num_pehm) %>% 
  st_drop_geometry() %>%
  unique() %>%
  column_to_rownames("value")

densnum_geol %>%
  group_by(lithologie) %>%
  identify_outliers() # il y a des valeurs aberrantes...

model_densnum_geol <- lm(dens_num_pehm ~ lithologie,
                         data = densnum_geol)

ggqqplot(residuals(model_densnum_geol))

shapiro_test(residuals(model_densnum_geol))

# Sauvegarde ----

save( litho_densite_pe,
      table_litho_densite_pe,
      densite_numerique_pe_litho,
      densite_numerique_pehm_litho,
      densite_surfacique_pe_litho,
      densite_surfacique_pehm_litho,
      pe_taille,
      surface_moyenne_pehm_litho,
      surface_mediane_pehm_litho,
      pe_q,
      repartition_pe_rh,
      repartition_pehm_rh,
      repartition_bv_rh,
      bv_me_q,
      table_bv_me_q,
      densite_numerique_pe_rh,
      densite_numerique_pehm_rh,
      densite_surfacique_pe_rh,
      densite_surfacique_pehm_rh,
      pe_taille_q,
      pehm_taille_q,
      surface_moyenne_pe_rh,
      surface_moyenne_pehm_rh,
      surface_mediane_pe_rh,
      surface_mediane_pehm_rh,
      surface_pehm_rh,
      bv_me_type,
      table_bv_me_type,
      densite_numerique_pe_type,
      densite_numerique_pehm_type,
      densite_surfacique_pe_type,
      densite_surfacique_pehm_type,
      bv_me_maraichage,
      classes_maraichage,
      table_classes_maraichage,
      maraichage_densite_numerique_tot,
      maraichage_densite_numerique_pehm,
      cl_maraichage_densite_numerique_pe,
      cl_maraichage_densite_numerique_pehm,
      com_maraichage,
      classes_maraichage_com,
      table_classes_maraichage_com,
      maraichage_com_densite_numerique_tot,
      maraichage_com_densite_numerique_pehm,
      maraichage_com_densite_surfacique_tot,
      maraichage_com_densite_surfacique_pehm,
      cl_maraichage_com_densite_numerique_pe,
      cl_maraichage_com_densite_numerique_pehm,
      file = "data/outputs/w_autres_hypotheses.RData")

