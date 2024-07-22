#' Sommer des volumes dans un polygone.
#' 
#' A partir d'une couche d'intersection entre des points (contenant des volumes et l'id des polygones)
#'     et des polygones (grands, par exemple des BV de masses d'eau) qui contient un attribut de surface, 
#'     la fonction propose de sommer ces volumes contenus dans chacun des polygones. 
#'
#' @param couche_points Objet de classe sf ...
#' @param var_id_polygone,var_a_sommer Nom des variables de `couche_surface`et couhe_points 
#' indiquant respectivement le code unique des polygones et le volume des points.  
#' @param var_somme_volumes Nom de la variable qui contiendra le résultat de la somme
#'     des surfaces des objets.
#' @param seulement_retenues 

#'
#' @return un dataframe composé de deux attributs : code unique des polygones et 
#'     somme des volumes des points contenus dans le polygone. 
#' @export 
#'
#' @examples
#' \dontrun{
#' test <-
#' compter_sommer_surfaces_dans_polygone(
#'   couche_surface = pe_decoup_sage %>% units::drop_units(),
#'   var_id_polygone = nom_sage,
#'   var_a_sommer = surface_intersect,
#'   seulement_retenues = FALSE
#' )
#' }
sommer_volume_points_ds_polygone <- function(couche_points,
                                                  var_id_polygone,
                                                  var_a_sommer = "volume_preleve",
                                                  var_somme_volumes = "somme_volumes",
                                                  seulement_retenues = FALSE)
  
{
  var_id_polygone <- enquo(var_id_polygone)
  var_a_sommer <- enquo(var_a_sommer)
  var_somme_volumes <- enquo(var_somme_volumes)
  
  if (seulement_retenues) {
    couche_points <- couche_points %>%
      filter(NATURE_RESSOURCE == "RA" | 
               NATURE_RESSOURCE == "RC" |
               NATURE_RESSOURCE == "RN" |
               NATURE_RESSOURCE == 'RO' |
               NATURE_RESSOURCE == "RP")
  }
  
 somme_volumes_dans_polygone <- couche_points %>% 
    group_by(!!var_id_polygone) %>%
    summarise(!!var_somme_volumes := sum(!!var_a_sommer)) %>%
    select(!!var_id_polygone, # sélection des variables à conserver
           !!var_somme_volumes)
  
 somme_volumes_dans_polygone 
  
}

