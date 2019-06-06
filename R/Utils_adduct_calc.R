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
