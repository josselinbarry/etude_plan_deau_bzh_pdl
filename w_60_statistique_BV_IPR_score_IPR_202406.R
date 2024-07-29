# VERSION FINALISEE AU 202401
## En cours de création

# Analyse statistique des indicateurs PE et qualités d'eau ----

## Library ----

library(tidyverse)
library(sf)
library(units)
library(COGiter)
library(PerformanceAnalytics)
library(mapview)
library(readxl)
library(downloadthis)
library(FactoMineR)
library(aspe)
library(hubeau)

## Chargement des données ----

bv_score_ipr <-
  sf::read_sf(dsn = "data/bv_ipr_20240627_qualif.gpkg")

## Filtre éventuel des résultats pour ajuster un échantillon plus cohérent ---
#On test ici les BV dont le rang de strahler est inf ou égale à 4

bv_score_ipr <- bv_score_ipr %>%
  filter(
#    surface_ipr < 20321250 &
      is.na(hors_stat) &
      difference_area > 0.85 & difference_area < 1.15)

## Calcul des indicateurs densite, prct ----

bv_score_ipr <- bv_score_ipr  %>%
#  filter(!is.na(nb_pe_tot) & !is.na(longueur_ce_topage)) %>%
  mutate(densite_pe = coalesce(nb_pe_tot,0)/surface_ipr) %>%
  mutate(densite_pe_perm = coalesce(nb_pe_perm,0)/surface_ipr) %>%
  mutate(densite_pe_mares = (coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/surface_ipr) %>%
  mutate(densite_pe_mares_perm = (coalesce(nb_pe_perm,0) + coalesce(nb_mares_perm,0))/surface_ipr) %>%
  mutate(densite_pehm_tdbv = coalesce(nb_pehm_tdbv_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_connecte = coalesce(nb_pehm_connecte_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_sur_cours = coalesce(nb_pehm_sur_cours_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp = coalesce(nb_pehm_zhp_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp_zhp = coalesce(nb_pehm_zhp_tot,0)/surf_zhp) %>%
  mutate(densite_pehm_mareshm_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surface_ipr) %>%
  mutate(densite_pehm_mareshm_zhp_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surf_zhp) %>%
  mutate(q5_qa = (coalesce(Q5MOY_max,0) / coalesce(QAMOY_max,0))) %>%
  mutate(prop_zhp = (coalesce(surf_zhp,0) /surface_ipr)) %>%
  mutate(prop_marais = (coalesce(surf_marais,0) /surface_ipr)) %>%
  mutate(prop_tdbv = (coalesce(longueur_ce_tdbv_topage,0) /longueur_ce_topage)) %>%
  mutate(q5 = Q5MOY_max) %>%
  mutate(qa = QAMOY_max) %>%
  mutate(taux_drain = longueur_ce_topage/surface_ipr) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = coalesce(surf_pe_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_perm = coalesce(surf_pe_perm,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares = (coalesce(surf_pe_tot,0) + coalesce(surf_mares_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares_perm = (coalesce(surf_pe_perm,0)+ coalesce(surf_mares_perm,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_tdbv = coalesce(surf_pehm_tdbv_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_connecte = coalesce(surf_pehm_connecte_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surf_zhp) %>%
  mutate(prct_surf_pehm_mareshm_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_mareshm_zhp_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surf_zhp) %>%
  dplyr::select(sta_id, 
         surface_ipr,
         surface_moy_pe_perm,
         densite_pe,
         densite_pe_perm,
         densite_pe_mares,
         densite_pe_mares_perm,
         densite_pehm_tdbv,
         densite_pehm_connecte,
         densite_pehm_sur_cours,
         densite_pehm_zhp,
         densite_pehm_zhp_zhp,
         densite_pehm_mareshm_zhp,
         densite_pehm_mareshm_zhp_zhp,
         qa,
         q5,
         q5_qa,
#         prop_marais,
         prop_tdbv,
         prop_zhp,
         taux_drain,
         prct_intercept, 
         prct_intercept_perm, 
         prct_intercept_tdbv,
         prct_surf_pe, 
         prct_surf_pe_perm, 
         prct_surf_pe_mares, 
         prct_surf_pe_mares_perm, 
         prct_surf_pehm_tdbv, 
         prct_surf_pehm_connecte, 
         prct_surf_pehm_zhp,
         prct_surf_pehm_zhp_zhp,
         prct_surf_pehm_mareshm_zhp,
         prct_surf_pehm_mareshm_zhp_zhp,
         ner, 
         nel, 
         nte,
         dit, 
         dio, 
         dii,
         dti, 
         ipr) %>%
  st_drop_geometry()

## Loguer les variables quantitatives nécessaires (ou log(x+1) si inf) ----
##=> correlation type pearson

bv_score_ipr <- bv_score_ipr %>%
  filter(!is.na(q5))

bv_score_ipr_log <- bv_score_ipr %>% 
  mutate(across(surface_ipr:densite_pehm_tdbv, log)) %>%
  mutate(across(qa:prct_intercept_tdbv, function(x) log(1+x))) %>%
  mutate(across(prct_surf_pe:prct_surf_pehm_tdbv, log)) %>%
  mutate(across(prct_surf_pehm_connecte:prct_surf_pehm_mareshm_zhp_zhp, function(x) log(1+x))) %>%
  mutate(across(ner:ipr, log)) 

## Identification des groupes de variables synthétisables ----

corr_tot_bv_ipr <- bv_score_ipr_log %>% 
  dplyr::select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr, order="hclust",tl.srt=45, addrect = 10)

# en ne conservant que IPR NER et NEL

corr_tot_bv_ipr2 <- bv_score_ipr_log %>% 
  dplyr::select(-sta_id, -dit, -nte, -dio, -dii, -dti, -qa, -q5, -prop_marais, -prop_tdbv, -taux_drain) %>%
  cor(method = "pearson")

corrplot::corrplot(bv_score_ipr_log %>% 
                     dplyr::select(-sta_id, -dit, -nte, -dio, -dii, -dti, -qa, -q5,  -prop_zhp, -prop_tdbv, -taux_drain) %>%
                     cor(method = "pearson"), 
                   order="hclust",tl.srt=45, addrect = 9)

#=> 8 groupes cohérents

### ACP sur les pourcentages ----
# On sélectionne un groupe de variables très corrélées entre elles,
# on écarte du groupe de variable celle qui ne sont pas de même nature interprétative (densité et prct par expl) 
# on ajoute les identifiants des lignes en rownames

prct_tot_ipr <- bv_score_ipr_log %>% 
  dplyr::select(sta_id,
         prct_surf_pehm_connecte,
         prct_surf_pehm_zhp,
         prct_surf_pehm_mareshm_zhp, 
         prct_surf_pehm_zhp_zhp,
         prct_surf_pehm_mareshm_zhp_zhp,
         prct_surf_pe,
         prct_surf_pe_mares,
         prct_surf_pe_perm,
         prct_surf_pe_mares_perm) %>% 
  column_to_rownames(var = "sta_id")

acp_prct_tot_ipr <- FactoMineR::PCA(prct_tot_ipr, scale.unit = TRUE, ncp = 5, graph = T)

names(acp_prct_tot_ipr)

df_prct_synth_ipr <- acp_prct_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(prct_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densites ----

densite_tot_ipr <- bv_score_ipr_log %>% 
  dplyr::select(sta_id,
         densite_pehm_zhp,
         densite_pehm_mareshm_zhp,
         densite_pe_perm,
         densite_pe_mares_perm,
         densite_pe,
         densite_pe_mares,
         densite_pehm_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_densite_tot_ipr <- FactoMineR::PCA(densite_tot_ipr, scale.unit = TRUE, ncp = 5, graph = T)

df_densite_synth_ipr <- acp_densite_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les intercept ----

intercept_tot_ipr <- bv_score_ipr_log %>% 
  dplyr::select(sta_id,
         prct_intercept,
         prct_intercept_perm,
         prct_intercept_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_intercept_tot_ipr <- FactoMineR::PCA(intercept_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_intercept_synth_ipr <- acp_intercept_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(intercept_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densite_zhp_zhp ----

densite_zhp_zhp_tot_ipr <- bv_score_ipr_log %>% 
  dplyr::select(sta_id,
         densite_pehm_zhp_zhp,
         densite_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "sta_id")

acp_densite_zhp_zhp_tot_ipr <- FactoMineR::PCA(densite_zhp_zhp_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_zhp_zhp_synth_ipr <- acp_densite_zhp_zhp_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_zhp_zhp_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les connections ----

densite_connect_tot_ipr <- bv_score_ipr_log %>% 
  dplyr::select(sta_id,
         densite_pehm_connecte,
         densite_pehm_sur_cours) %>%
  column_to_rownames(var = "sta_id")

acp_connect_tot_ipr <- FactoMineR::PCA(densite_connect_tot_ipr, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_connect_synth_ipr <- acp_connect_tot_ipr$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_connect_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

## Assemblage du tableau pour la modélisation ----

bv_score_ipr_log2 <- bv_score_ipr_log %>%
  mutate(sta_id=as.character(sta_id))

stat_corr_bv_ipr <- bv_score_ipr_log2 %>% 
  dplyr::select(sta_id,
         surface_ipr,
         surface_moy_pe_perm,
         prct_surf_pehm_tdbv,
         ipr, 
         nel,
         ner,
         qa,
         q5,
         q5_qa,
#         prop_marais,
         prop_zhp,
         prop_tdbv,
         taux_drain) %>% 
  left_join(y = df_prct_synth_ipr) %>% 
  left_join(y = df_densite_synth_ipr) %>%
  left_join(y = df_intercept_synth_ipr) %>%
  left_join(y = df_densite_zhp_zhp_synth_ipr) %>%
  left_join((y = df_densite_connect_synth_ipr))
  
save(bv_score_ipr_log,
     corr_tot_bv_ipr,
     prct_tot_ipr,
     densite_tot_ipr,
     intercept_tot_ipr,
     densite_zhp_zhp_tot_ipr,
     densite_connect_tot_ipr,
     stat_corr_bv_ipr, 
     file = "data_processed/w_pe_bv_ipr_stat_vf_tot.RData")

## Chargement des données pour limiter la distance de cook à 0,01 -------

bv_score_ipr_lim <-
  sf::read_sf(dsn = "data/bv_ipr_20240627_qualif.gpkg") 

## Filtre éventuel des résultats pour ajuster un échantillon plus cohérent ---
#On test ici les BV dont le rang de strahler est inf ou égale à 4

bv_score_ipr_lim <- bv_score_ipr_lim %>%
  filter(
    #    surface_ipr < 20321250 &
    is.na(hors_stat) &
      difference_area > 0.85 & difference_area < 1.15)

bv_score_ipr_lim <- bv_score_ipr_lim %>%
  left_join(dist_cook) %>%
  filter(!is.na(cook) &
           cook < 0.01)

## Calcul des indicateurs densite, prct ----

bv_score_ipr_lim <- bv_score_ipr_lim  %>%
  #  filter(!is.na(nb_pe_tot) & !is.na(longueur_ce_topage)) %>%
  mutate(densite_pe = coalesce(nb_pe_tot,0)/surface_ipr) %>%
  mutate(densite_pe_perm = coalesce(nb_pe_perm,0)/surface_ipr) %>%
  mutate(densite_pe_mares = (coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/surface_ipr) %>%
  mutate(densite_pe_mares_perm = (coalesce(nb_pe_perm,0) + coalesce(nb_mares_perm,0))/surface_ipr) %>%
  mutate(densite_pehm_tdbv = coalesce(nb_pehm_tdbv_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_connecte = coalesce(nb_pehm_connecte_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_sur_cours = coalesce(nb_pehm_sur_cours_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp = coalesce(nb_pehm_zhp_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp_zhp = coalesce(nb_pehm_zhp_tot,0)/surf_zhp) %>%
  mutate(densite_pehm_mareshm_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surface_ipr) %>%
  mutate(densite_pehm_mareshm_zhp_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surf_zhp) %>%
  mutate(q5_qa = (coalesce(Q5MOY_max,0) / coalesce(QAMOY_max,0))) %>%
  mutate(prop_zhp = (coalesce(surf_zhp,0) /surface_ipr)) %>%
  mutate(prop_marais = (coalesce(surf_marais,0) /surface_ipr)) %>%
  mutate(prop_tdbv = (coalesce(longueur_ce_tdbv_topage,0) /longueur_ce_topage)) %>%
  mutate(q5 = Q5MOY_max) %>%
  mutate(qa = QAMOY_max) %>%
  mutate(taux_drain = longueur_ce_topage/surface_ipr) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = coalesce(surf_pe_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_perm = coalesce(surf_pe_perm,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares = (coalesce(surf_pe_tot,0) + coalesce(surf_mares_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares_perm = (coalesce(surf_pe_perm,0)+ coalesce(surf_mares_perm,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_tdbv = coalesce(surf_pehm_tdbv_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_connecte = coalesce(surf_pehm_connecte_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surf_zhp) %>%
  mutate(prct_surf_pehm_mareshm_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_mareshm_zhp_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surf_zhp) %>%
  dplyr::select(sta_id, 
                surface_ipr,
                surface_moy_pe_perm,
                densite_pe,
                densite_pe_perm,
                densite_pe_mares,
                densite_pe_mares_perm,
                densite_pehm_tdbv,
                densite_pehm_connecte,
                densite_pehm_sur_cours,
                densite_pehm_zhp,
                densite_pehm_zhp_zhp,
                densite_pehm_mareshm_zhp,
                densite_pehm_mareshm_zhp_zhp,
                qa,
                q5,
                q5_qa,
                #         prop_marais,
                prop_tdbv,
                prop_zhp,
                taux_drain,
                prct_intercept, 
                prct_intercept_perm, 
                prct_intercept_tdbv,
                prct_surf_pe, 
                prct_surf_pe_perm, 
                prct_surf_pe_mares, 
                prct_surf_pe_mares_perm, 
                prct_surf_pehm_tdbv, 
                prct_surf_pehm_connecte, 
                prct_surf_pehm_zhp,
                prct_surf_pehm_zhp_zhp,
                prct_surf_pehm_mareshm_zhp,
                prct_surf_pehm_mareshm_zhp_zhp,
                ner, 
                nel, 
                nte,
                dit, 
                dio, 
                dii,
                dti, 
                ipr) %>%
  st_drop_geometry()

## Loguer les variables quantitatives nécessaires (ou log(x+1) si inf) ----
##=> correlation type pearson

bv_score_ipr_lim <- bv_score_ipr_lim %>%
  filter(!is.na(q5))

bv_score_ipr_lim_log <- bv_score_ipr_lim %>% 
  mutate(across(surface_ipr:densite_pehm_tdbv, log)) %>%
  mutate(across(qa:prct_intercept_tdbv, function(x) log(1+x))) %>%
  mutate(across(prct_surf_pe:prct_surf_pehm_tdbv, log)) %>%
  mutate(across(prct_surf_pehm_connecte:prct_surf_pehm_mareshm_zhp_zhp, function(x) log(1+x))) %>%
  mutate(across(ner:ipr, log)) 

## Identification des groupes de variables synthétisables ----

corr_tot_bv_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr_lim, order="hclust",tl.srt=45, addrect = 10)

# en ne conservant que IPR NER et NEL

corr_tot_bv_ipr2 <- bv_score_ipr_lim_log %>% 
  dplyr::select(-sta_id, -dit, -nte, -dio, -dii, -dti) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr2, order="hclust",tl.srt=45, addrect = 9)

#=> 8 groupes cohérents

### ACP sur les pourcentages ----
# On sélectionne un groupe de variables très corrélées entre elles,
# on écarte du groupe de variable celle qui ne sont pas de même nature interprétative (densité et prct par expl) 
# on ajoute les identifiants des lignes en rownames

prct_tot_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(sta_id,
                prct_surf_pehm_connecte,
                prct_surf_pehm_zhp,
                prct_surf_pehm_mareshm_zhp, 
                prct_surf_pehm_zhp_zhp,
                prct_surf_pehm_mareshm_zhp_zhp,
                prct_surf_pe,
                prct_surf_pe_mares,
                prct_surf_pe_perm,
                prct_surf_pe_mares_perm) %>% 
  column_to_rownames(var = "sta_id")

acp_prct_tot_ipr_lim <- FactoMineR::PCA(prct_tot_ipr_lim, scale.unit = TRUE, ncp = 5, graph = T)

names(acp_prct_tot_ipr_lim)

df_prct_synth_ipr_lim <- acp_prct_tot_ipr_lim$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(prct_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densites ----

densite_tot_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(sta_id,
                densite_pehm_zhp,
                densite_pehm_mareshm_zhp,
                densite_pe_perm,
                densite_pe_mares_perm,
                densite_pe,
                densite_pe_mares,
                densite_pehm_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_densite_tot_ipr_lim <- FactoMineR::PCA(densite_tot_ipr_lim, scale.unit = TRUE, ncp = 5, graph = T)

df_densite_synth_ipr_lim <- acp_densite_tot_ipr_lim$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les intercept ----

intercept_tot_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(sta_id,
                prct_intercept,
                prct_intercept_perm,
                prct_intercept_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_intercept_tot_ipr_lim <- FactoMineR::PCA(intercept_tot_ipr_lim, scale.unit = TRUE, ncp = 4, graph = T)

df_intercept_synth_ipr_lim <- acp_intercept_tot_ipr_lim$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(intercept_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densite_zhp_zhp ----

densite_zhp_zhp_tot_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(sta_id,
                densite_pehm_zhp_zhp,
                densite_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "sta_id")

acp_densite_zhp_zhp_tot_ipr_lim <- FactoMineR::PCA(densite_zhp_zhp_tot_ipr_lim, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_zhp_zhp_synth_ipr_lim <- acp_densite_zhp_zhp_tot_ipr_lim$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_zhp_zhp_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les connections ----

densite_connect_tot_ipr_lim <- bv_score_ipr_lim_log %>% 
  dplyr::select(sta_id,
                densite_pehm_connecte,
                densite_pehm_sur_cours) %>%
  column_to_rownames(var = "sta_id")

acp_connect_tot_ipr_lim <- FactoMineR::PCA(densite_connect_tot_ipr_lim, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_connect_synth_ipr_lim <- acp_connect_tot_ipr_lim$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_connect_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

## Assemblage du tableau pour la modélisation ----

bv_score_ipr_lim_log2 <- bv_score_ipr_lim_log %>%
  mutate(sta_id=as.character(sta_id))

stat_corr_bv_ipr_lim <- bv_score_ipr_lim_log2 %>% 
  dplyr::select(sta_id,
                surface_ipr,
                surface_moy_pe_perm,
                prct_surf_pehm_tdbv,
                ipr, 
                nel,
                ner,
                qa,
                q5,
                q5_qa,
                #         prop_marais,
                prop_zhp,
                prop_tdbv,
                taux_drain) %>% 
  left_join(y = df_prct_synth_ipr_lim) %>% 
  left_join(y = df_densite_synth_ipr_lim) %>%
  left_join(y = df_intercept_synth_ipr_lim) %>%
  left_join(y = df_densite_zhp_zhp_synth_ipr_lim) %>%
  left_join((y = df_densite_connect_synth_ipr_lim))

save(bv_score_ipr_lim_log,
     prct_tot_ipr_lim,
     densite_tot_ipr_lim,
     intercept_tot_ipr_lim,
     densite_zhp_zhp_tot_ipr_lim,
     densite_connect_tot_ipr_lim,
     stat_corr_bv_ipr_lim, 
     file = "data_processed/w_pe_bv_ipr_stat_vf_lim.RData")



# test

corr_tot_test_ipr <- stat_corr_bv_ipr %>% 
  dplyr::select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_test_ipr, order="hclust",tl.srt=45, addrect = 8)

## Chargement des données pour limiter la distance de cook à 0,01 et la taille des BV-------

bv_score_ipr_lim_bv <-
  sf::read_sf(dsn = "data/bv_ipr_20240627_qualif.gpkg") 

## Filtre éventuel des résultats pour ajuster un échantillon plus cohérent ---
#On test ici les BV dont le rang de strahler est inf ou égale à 4

bv_score_ipr_lim_bv <- bv_score_ipr_lim_bv %>%
  filter(
    surface_ipr < 20321250 &
    is.na(hors_stat) &
      difference_area > 0.85 & difference_area < 1.15)

bv_score_ipr_lim_bv <- bv_score_ipr_lim_bv %>%
  left_join(dist_cook) %>%
  filter(!is.na(cook) &
           cook < 0.01)

## Calcul des indicateurs densite, prct ----

bv_score_ipr_lim_bv <- bv_score_ipr_lim_bv  %>%
  #  filter(!is.na(nb_pe_tot) & !is.na(longueur_ce_topage)) %>%
  mutate(densite_pe = coalesce(nb_pe_tot,0)/surface_ipr) %>%
  mutate(densite_pe_perm = coalesce(nb_pe_perm,0)/surface_ipr) %>%
  mutate(densite_pe_mares = (coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/surface_ipr) %>%
  mutate(densite_pe_mares_perm = (coalesce(nb_pe_perm,0) + coalesce(nb_mares_perm,0))/surface_ipr) %>%
  mutate(densite_pehm_tdbv = coalesce(nb_pehm_tdbv_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_connecte = coalesce(nb_pehm_connecte_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_sur_cours = coalesce(nb_pehm_sur_cours_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp = coalesce(nb_pehm_zhp_tot,0)/surface_ipr) %>%
  mutate(densite_pehm_zhp_zhp = coalesce(nb_pehm_zhp_tot,0)/surf_zhp) %>%
  mutate(densite_pehm_mareshm_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surface_ipr) %>%
  mutate(densite_pehm_mareshm_zhp_zhp = (coalesce(nb_pehm_zhp_tot,0) + coalesce(nb_mareshm_zhp_tot,0))/surf_zhp) %>%
  mutate(q5_qa = (coalesce(Q5MOY_max,0) / coalesce(QAMOY_max,0))) %>%
  mutate(prop_zhp = (coalesce(surf_zhp,0) /surface_ipr)) %>%
  mutate(prop_marais = (coalesce(surf_marais,0) /surface_ipr)) %>%
  mutate(prop_tdbv = (coalesce(longueur_ce_tdbv_topage,0) /longueur_ce_topage)) %>%
  mutate(q5 = Q5MOY_max) %>%
  mutate(qa = QAMOY_max) %>%
  mutate(taux_drain = longueur_ce_topage/surface_ipr) %>%
  mutate(prct_intercept = coalesce(longueur_topage_intersecte_pehm_tot,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_perm = coalesce(longueur_topage_intersecte_pe_perm,0)*100/longueur_ce_topage) %>%
  mutate(prct_intercept_tdbv = coalesce(longueur_topage_intersecte_pehm_tdbv_tot,0)*100/longueur_ce_tdbv_topage) %>%
  mutate(prct_surf_pe = coalesce(surf_pe_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_perm = coalesce(surf_pe_perm,0)*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares = (coalesce(surf_pe_tot,0) + coalesce(surf_mares_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pe_mares_perm = (coalesce(surf_pe_perm,0)+ coalesce(surf_mares_perm,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_tdbv = coalesce(surf_pehm_tdbv_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_connecte = coalesce(surf_pehm_connecte_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surface_ipr) %>%
  mutate(prct_surf_pehm_zhp_zhp = coalesce(surf_pehm_zhp_tot,0)*100/surf_zhp) %>%
  mutate(prct_surf_pehm_mareshm_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surface_ipr) %>%
  mutate(prct_surf_pehm_mareshm_zhp_zhp = (coalesce(surf_pehm_zhp_tot,0)+ coalesce(surf_mareshm_zhp_tot,0))*100/surf_zhp) %>%
  dplyr::select(sta_id, 
                surface_ipr,
                surface_moy_pe_perm,
                densite_pe,
                densite_pe_perm,
                densite_pe_mares,
                densite_pe_mares_perm,
                densite_pehm_tdbv,
                densite_pehm_connecte,
                densite_pehm_sur_cours,
                densite_pehm_zhp,
                densite_pehm_zhp_zhp,
                densite_pehm_mareshm_zhp,
                densite_pehm_mareshm_zhp_zhp,
                qa,
                q5,
                q5_qa,
                #         prop_marais,
                prop_tdbv,
                prop_zhp,
                taux_drain,
                prct_intercept, 
                prct_intercept_perm, 
                prct_intercept_tdbv,
                prct_surf_pe, 
                prct_surf_pe_perm, 
                prct_surf_pe_mares, 
                prct_surf_pe_mares_perm, 
                prct_surf_pehm_tdbv, 
                prct_surf_pehm_connecte, 
                prct_surf_pehm_zhp,
                prct_surf_pehm_zhp_zhp,
                prct_surf_pehm_mareshm_zhp,
                prct_surf_pehm_mareshm_zhp_zhp,
                ner, 
                nel, 
                nte,
                dit, 
                dio, 
                dii,
                dti, 
                ipr) %>%
  st_drop_geometry()

## Loguer les variables quantitatives nécessaires (ou log(x+1) si inf) ----
##=> correlation type pearson

bv_score_ipr_lim_bv <- bv_score_ipr_lim_bv %>%
  filter(!is.na(q5))

bv_score_ipr_lim_bv_log <- bv_score_ipr_lim_bv %>% 
  mutate(across(surface_ipr:densite_pehm_tdbv, log)) %>%
  mutate(across(qa:prct_intercept_tdbv, function(x) log(1+x))) %>%
  mutate(across(prct_surf_pe:prct_surf_pehm_tdbv, log)) %>%
  mutate(across(prct_surf_pehm_connecte:prct_surf_pehm_mareshm_zhp_zhp, function(x) log(1+x))) %>%
  mutate(across(ner:ipr, log)) 

## Identification des groupes de variables synthétisables ----

corr_tot_bv_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr_lim_bv, order="hclust",tl.srt=45, addrect = 10)

# en ne conservant que IPR NER et NEL

corr_tot_bv_ipr2 <- bv_score_ipr_lim_log %>% 
  dplyr::select(-sta_id, -dit, -nte, -dio, -dii, -dti) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_bv_ipr2, order="hclust",tl.srt=45, addrect = 9)

#=> 8 groupes cohérents

### ACP sur les pourcentages ----
# On sélectionne un groupe de variables très corrélées entre elles,
# on écarte du groupe de variable celle qui ne sont pas de même nature interprétative (densité et prct par expl) 
# on ajoute les identifiants des lignes en rownames

prct_tot_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(sta_id,
                prct_surf_pehm_connecte,
                prct_surf_pehm_zhp,
                prct_surf_pehm_mareshm_zhp, 
                prct_surf_pehm_zhp_zhp,
                prct_surf_pehm_mareshm_zhp_zhp,
                prct_surf_pe,
                prct_surf_pe_mares,
                prct_surf_pe_perm,
                prct_surf_pe_mares_perm) %>% 
  column_to_rownames(var = "sta_id")

acp_prct_tot_ipr_lim_bv <- FactoMineR::PCA(prct_tot_ipr_lim_bv, scale.unit = TRUE, ncp = 5, graph = T)

names(acp_prct_tot_ipr_lim_bv)

df_prct_synth_ipr_lim_bv <- acp_prct_tot_ipr_lim_bv$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(prct_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densites ----

densite_tot_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(sta_id,
                densite_pehm_zhp,
                densite_pehm_mareshm_zhp,
                densite_pe_perm,
                densite_pe_mares_perm,
                densite_pe,
                densite_pe_mares,
                densite_pehm_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_densite_tot_ipr_lim_bv <- FactoMineR::PCA(densite_tot_ipr_lim_bv, scale.unit = TRUE, ncp = 5, graph = T)

df_densite_synth_ipr_lim_bv <- acp_densite_tot_ipr_lim_bv$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les intercept ----

intercept_tot_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(sta_id,
                prct_intercept,
                prct_intercept_perm,
                prct_intercept_tdbv) %>%
  column_to_rownames(var = "sta_id")

acp_intercept_tot_ipr_lim_bv <- FactoMineR::PCA(intercept_tot_ipr_lim_bv, scale.unit = TRUE, ncp = 4, graph = T)

df_intercept_synth_ipr_lim_bv <- acp_intercept_tot_ipr_lim_bv$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(intercept_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les densite_zhp_zhp ----

densite_zhp_zhp_tot_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(sta_id,
                densite_pehm_zhp_zhp,
                densite_pehm_mareshm_zhp_zhp) %>%
  column_to_rownames(var = "sta_id")

acp_densite_zhp_zhp_tot_ipr_lim_bv <- FactoMineR::PCA(densite_zhp_zhp_tot_ipr_lim_bv, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_zhp_zhp_synth_ipr_lim_bv <- acp_densite_zhp_zhp_tot_ipr_lim_bv$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_zhp_zhp_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

### ACP sur les connections ----

densite_connect_tot_ipr_lim_bv <- bv_score_ipr_lim_bv_log %>% 
  dplyr::select(sta_id,
                densite_pehm_connecte,
                densite_pehm_sur_cours) %>%
  column_to_rownames(var = "sta_id")

acp_connect_tot_ipr_lim_bv <- FactoMineR::PCA(densite_connect_tot_ipr_lim_bv, scale.unit = TRUE, ncp = 4, graph = T)

df_densite_connect_synth_ipr_lim_bv <- acp_connect_tot_ipr_lim_bv$ind$coord %>%
  as.data.frame() %>% 
  dplyr::select(densite_connect_synth = Dim.1) %>% 
  rownames_to_column(var = "sta_id")

## Assemblage du tableau pour la modélisation ----

bv_score_ipr_lim_bv_log2 <- bv_score_ipr_lim_bv_log %>%
  mutate(sta_id=as.character(sta_id))

stat_corr_bv_ipr_lim_bv <- bv_score_ipr_lim_bv_log2 %>% 
  dplyr::select(sta_id,
                surface_ipr,
                surface_moy_pe_perm,
                prct_surf_pehm_tdbv,
                ipr, 
                nel,
                ner,
                qa,
                q5,
                q5_qa,
                #         prop_marais,
                prop_zhp,
                prop_tdbv,
                taux_drain) %>% 
  left_join(y = df_prct_synth_ipr_lim_bv) %>% 
  left_join(y = df_densite_synth_ipr_lim_bv) %>%
  left_join(y = df_intercept_synth_ipr_lim_bv) %>%
  left_join(y = df_densite_zhp_zhp_synth_ipr_lim_bv) %>%
  left_join((y = df_densite_connect_synth_ipr_lim_bv))

save(bv_score_ipr_lim_bv_log,
     prct_tot_ipr_lim_bv,
     densite_tot_ipr_lim_bv,
     intercept_tot_ipr_lim_bv,
     densite_zhp_zhp_tot_ipr_lim_bv,
     densite_connect_tot_ipr_lim_bv,
     stat_corr_bv_ipr_lim_bv, 
     file = "data_processed/w_pe_bv_ipr_stat_vf_lim_bv.RData")



# test

corr_tot_test_ipr <- stat_corr_bv_ipr %>% 
  dplyr::select(-sta_id) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_test_ipr, order="hclust",tl.srt=45, addrect = 8)

