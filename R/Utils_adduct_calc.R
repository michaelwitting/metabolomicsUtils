#' @title Calculate adduct mass from neutral exact mass
#'
#' @export
calc_adduct_mass <- function(exact_mass, adduct) {
  
  adduct_rule_list <- adduct_rules()
  
  if(!adduct %in% names(adduct_rule_list)) {
    stop(paste0("Unknown adduct: ", adduct))
  }
  
  mass_multi <- adduct_rule_list[[adduct]]$mass_multi
  mass_add <- adduct_rule_list[[adduct]]$mass_add
  
  ion_mass <- exact_mass * mass_multi + mass_add
  
  return(ion_mass)
  
  
}

#' @title Calculate neutral mass from adduct mass
#'
#' @export
calc_neutral_mass <- function(ion_mass, adduct) {
  
  adduct_rule_list <- adduct_rules()
  
  if(!adduct %in% names(adduct_rule_list)) {
    stop(paste0("Unknown adduct: ", adduct))
  }
  
  mass_multi <- adduct_rule_list[[adduct]]$mass_multi
  mass_add <- adduct_rule_list[[adduct]]$mass_add
  
  exact_mass <- (ion_mass - mass_add) / mass_multi
  
  return(exact_mass)
  
}

#'
#'
#' @export
create_ion_formula <- function(chem_formula, adduct) {
  
  # get all adduct rules
  adduct_rules_all <- adduct_rules()
  
  # create ion formula
  ion_formula <- formula_addition(chem_formula, adduct_rules_all[[adduct]]$formula_add)
  ion_formula <- formula_subtraction(ion_formula, adduct_rules_all[[adduct]]$formula_sub)
  
  # add brackets and charge
  if(stringr::str_detect(adduct, "\\+")) {
    
    ion_formula <- paste0("[", ion_formula, "]", abs(adduct_rules_all[[adduct]]$charge), "+")
    
  } else if(stringr::str_detect(adduct, "\\-")) {
    
    ion_formula <- paste0("[", ion_formula, "]", abs(adduct_rules_all[[adduct]]$charge), "-")
    
  }

  # return result
  return(ion_formula)
}
