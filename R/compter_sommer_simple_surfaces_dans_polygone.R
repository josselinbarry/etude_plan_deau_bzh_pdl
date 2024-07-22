#' Compter et sommer simplement des surfaces incluses dans un polygone.
#' 
#' A partir d'une couche d'intersection entre des surfaces (petites, par exemple des plans d'eau)
#'     et des polygones (grands, par exemple des BV de masses d'eau) qui contient un attribut de surface, 
#'     la fonction propose de décompter et sommer simplement ces surfaces contenues dans chacun des polygones. 
#'
#' @param couche_objet Objet de classe sf ...
#' @param var_id_polygone,var_a_sommer Nom des variables de `couche_surface` indiquant respectivement 
#'     le code unique des polygones et la superficie des surfaces.  
#' @param var_nb_objets Nom de la variable qui contiendra le résultat du décompte
#'     des objets
#' @param var_somme_surfaces Nom de la variable qui contiendra le résultat de la somme
#'     des surfaces des objets.
#'
#' @return un dataframe composé de deux attributs : code unique des polygones et 
#'     somme de la superficie des surfaces contenues dans le polygone. 
#' @export 
#'
#' @examples
#' \dontrun{
#' test <-
#' compter_sommer_surfaces_dans_polygone(
#'   couche_surface = pe_decoup_sage %>% units::drop_units(),
#'   var_id_polygone = nom_sage,
#'   var_a_sommer = surface_intersect,
#' )
#' }
#' 
compter_sommer_simple_surfaces_dans_polygone <- function(couche_surface,
                                                  var_id_polygone,
                                                  var_a_sommer = "surface_intersect",
                                                  var_nb_objets = "nb_objets",
                                                  var_somme_surfaces = "somme_surfaces")
  
{
  var_id_polygone <- enquo(var_id_polygone)
  var_a_sommer <- enquo(var_a_sommer)
  var_nb_objets <- enquo(var_nb_objets)
  var_somme_surfaces <- enquo(var_somme_surfaces)
  
  decompte_et_somme_simple_surfaces_dans_polygone <- couche_surface %>% 
    group_by(!!var_id_polygone) %>%
    summarise(!!var_nb_objets := n(),
              !!var_somme_surfaces := sum(!!var_a_sommer)) %>%
    select(!!var_id_polygone, # sélection des variables à conserver
           !!var_nb_objets,
           !!var_somme_surfaces)
  
  decompte_et_somme_simple_surfaces_dans_polygone 
  
}

