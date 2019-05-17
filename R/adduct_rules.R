
adduct_rules <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- list('[M+H]+' = list(mass_multi = 1,
                                      mass_add =  rcdk::get.formula('H', charge = 1)@mass,
                                      formula_add = 'H',
                                      formula_sub = 'C0'),
                      '[M+Na]+' = list(mass_multi = 1,
                                       mass_add = rcdk::get.formula('Na', charge = 1)@mass,
                                       formula_add = 'Na',
                                       formula_sub = 'C0'),
                      '[M+NH4]+' = list(mass_multi = 1,
                                        mass_add = rcdk::get.formula('NH4', charge = 1)@mass,
                                        formula_add = 'NH4',
                                        formula_sub = 'C0'))
  
}


adduct_rules_pos <- function() {
  
}


adduct_rules_neg <- function() {
  
}