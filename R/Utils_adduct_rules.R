#' @title Create list with all adduct calculation rules (all rules)
#' 
#' This function returns a list with rules for the calculation of adducts. 
#' It is required for the calculation of adduct m/z values from exact masses 
#' and the other way round. This list contains all rules for both ionization 
#' modes.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{adduct_rules_neg}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules_all <- function() {
  
  # get adduct lists
  adduct_list_neg <- adduct_rules_neg()
  adduct_list_pos <- adduct_rules_pos()
  
  adduct_list <- append(adduct_list_pos,
                        adduct_list_neg)
  
  
  ## return values
  return(adduct_list)
  
}


#' @title Create list with all adduct calculation rules (positive ionisation)
#' 
#' This function returns a list with rules for the calculation of adducts. 
#' It is required for the calculation of adduct m/z values from exact masses 
#' and the other way round. This list contains all rules for the positive 
#' ionization mode.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules_all}}
#' @seealso \code{\link{adduct_rules_neg}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules_pos <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- list(
    
    # triple charged -----------------------------------------------------------
    '[M+3H]3+'      = list(mass_multi = 1 / 3,
                           mass_add = 3 * rcdk::get.formula('H', charge = 1)@mass / 3,
                           formula_add = 'H3',
                           formula_sub = 'C0',
                           charge = 3),
    '[M+2H+Na]3+'   = list(mass_multi = 1 / 3,
                           mass_add = (2 * rcdk::get.formula('H', charge = 1)@mass + 1 * rcdk::get.formula('Na', charge = 1)@mass) / 3,
                           formula_add = 'H2Na',
                           formula_sub = 'C0',
                           charge = 3),
    '[M+H+Na2]3+'   = list(mass_multi = 1 / 3,
                           mass_add = (1 * rcdk::get.formula('H', charge = 1)@mass + 2 * rcdk::get.formula('Na', charge = 1)@mass) / 3,
                           formula_add = 'HNa2',
                           formula_sub = 'C0',
                           charge = 3),
    '[M+Na3]3+'     = list(mass_multi = 1 / 3,
                           mass_add = (3 * rcdk::get.formula('Na', charge = 1)@mass) / 3,
                           formula_add = 'Na3',
                           formula_sub = 'C0',
                           charge = 3),
    
    # double charged -----------------------------------------------------------
    '[M+2H]2+'      = list(mass_multi = 1 / 2,
                           mass_add = 2 * rcdk::get.formula('H', charge = 1)@mass / 2,
                           formula_add = 'H2',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+H+NH4]2+'   = list(mass_multi = 1 / 2,
                           mass_add = (rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('NH4', charge = 1)@mass) / 2,
                           formula_add = 'NH5',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+H+K]2+'     = list(mass_multi = 1 / 2,
                           mass_add = (rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('K', charge = 1)@mass) / 2,
                           formula_add = 'HK',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+H+Na]2+'    = list(mass_multi = 1 / 2,
                           mass_add = (rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('Na', charge = 1)@mass) / 2,
                           formula_add = 'HNa',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+C2H3N+2H]2+'= list(mass_multi = 1 / 2,
                           mass_add = (2 * rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('C2H3N', charge = 0)@mass) / 2,
                           formula_add = 'C2H5N',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+2Na]2+'     = list(mass_multi = 1 / 2,
                           mass_add = 2 * rcdk::get.formula('Na', charge = 1)@mass / 2,
                           formula_add = 'Na2',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+C4H6N2+2H]2+'= list(mass_multi = 1 / 2,
                           mass_add = (2 * rcdk::get.formula('H', charge = 1)@mass + 2 * rcdk::get.formula('C2H3N', charge = 0)@mass) / 2,
                           formula_add = 'C4H8N2',
                           formula_sub = 'C0',
                           charge = 2),
    '[M+C6H9N3+2H]2+' = list(mass_multi = 1 / 2,
                           mass_add = (2 * rcdk::get.formula('H', charge = 1)@mass + 3 * rcdk::get.formula('C2H3N', charge = 0)@mass) / 2,
                           formula_add = 'C6H11N3',
                           formula_sub = 'C0',
                           charge = 2),
    
    # single charged -----------------------------------------------------------
    '[M+H]+'        = list(mass_multi = 1,
                           mass_add =  rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'H',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+Li]+'       = list(mass_multi = 1,
                            mass_add =  rcdk::get.formula('Li', charge = 1)@mass,
                            formula_add = 'Li',
                            formula_sub = 'C0',
                            charge = 1),
    '[M+2Li-H]+'    = list(mass_multi = 1,
                           mass_add =  2 * rcdk::get.formula('H', charge = 1)@mass -  rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'Li2',
                           formula_sub = 'H',
                           charge = 1),
    '[M+NH4]+'      = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('NH4', charge = 1)@mass,
                           formula_add = 'NH4',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+H2O+H]+'   = list(mass_multi = 1,
                          mass_add = rcdk::get.formula('H3O', charge = 1)@mass,
                          formula_add = 'H3O',
                          formula_sub = 'C0',
                          charge = 1),
    '[M+Na]+'       = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('Na', charge = 1)@mass,
                           formula_add = 'Na',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+CH4O+H]+'  = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('CH4O', charge = 0)@mass,
                           formula_add = 'Na',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+K]+'        = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('K', charge = 1)@mass,
                           formula_add = 'K',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+C2H3N+H]+'  = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('C2H3N', charge = 0)@mass,
                           formula_add = 'C2H4N',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+2Na-H]+'    = list(masS_multi = 1,
                           mass_add = 2 * rcdk::get.formula('Na', charge = 1)@mass - rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'Na2',
                           formula_sub = 'H',
                           charge = 1),
    '[M+C3H8O+H]+'  = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('C3H8O', charge = 0)@mass,
                           formula_add = 'C3H9O',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+C2H3N+Na]+'   = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('Na', charge = 1)@mass + rcdk::get.formula('C2H3N', charge = 0)@mass,
                           formula_add = 'C2H3NNa',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+2K-H]+'     = list(mass_multi = 1,
                           mass_add = 2 * rcdk::get.formula('K', charge = 1)@mass - rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'K2',
                           formula_sub = 'H',
                           charge = 1),
    '[M+C2H6OS+H]+'   = list(mass_multi = 1,
                           masS_add = rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('C2H6OS', charge = 0)@mass,
                           formula_add = 'C2H7OS',
                           formula_sub = 'C0',
                           charge = 1),
    '[M+C4H6N2+H]+'   = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass + 2 * rcdk::get.formula('C2H3N', charge = 0)@mass,
                           formula_add = 'C2H4N',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+H]+'       = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'H',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+NH4]+'     = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('NH4', charge = 1)@mass,
                           formula_add = 'NH4',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+Na]+'      = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('Na', charge = 1)@mass,
                           formula_add = 'Na',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+K]+'       = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('K', charge = 1)@mass,
                           formula_add = 'K',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+C2H3N+H]+'   = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('C2H3N', charge = 0)@mass,
                           formula_add = 'C2H4N',
                           formula_sub = 'C0',
                           charge = 1),
    '[2M+C2H3N+Na]+'  = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('Na', charge = 1)@mass + rcdk::get.formula('C2H3N', charge = 0)@mass,
                           formula_add = 'C2H3NNa',
                           formula_sub = 'C0',
                           charge = 1)
    )
  
  ## return values
  return(adduct_list)
  
}

#' @title Create list with all adduct calculation rules (negative ion model)
#' 
#' This function returns a list with rules for the calculation of adducts. 
#' It is required for the calculation of adduct m/z values from exact masses 
#' and the other way round. This list contains all rules for the negative 
#' ionization mode.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules_all}}
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules_neg <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- list(
    # triple charged -----------------------------------------------------------
    '[M-3H]3-'      = list(mass_multi = 1 / 3,
                           mass_add = - 3 * rcdk::get.formula('H', charge = 1)@mass / 3,
                           formula_add = 'C0',
                           formula_sub = 'H3',
                           charge = -3),
    
    # double charged -----------------------------------------------------------
    '[M-2H]2-'      = list(mass_multi = 1 / 2,
                           mass_add = - 2 * rcdk::get.formula('H', charge = 1)@mass / 2,
                           formula_add = 'C0',
                           formula_sub = 'H2',
                           charge = -2),
    
    # single charged -----------------------------------------------------------
    '[M-H]-'        = list(mass_multi = 1 ,
                           mass_add = - rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'C0',
                           formula_sub = 'H',
                           charge = -1),
    '[M+Na-2H]-'    = list(mass_multi = 1,
                           mass_add = - 2 * rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('Na', charge = 1)@mass,
                           formula_add = 'Na',
                           formula_sub = 'H2',
                           charge = -1),
    '[M+Cl]-'       = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('Cl', charge = -1)@mass,
                           formula_add = 'Cl',
                           formula_sub = 'C0',
                           charge = -1),
    '[M+K-2H]-'     = list(mass_multi = 1,
                           mass_add = - 2 * rcdk::get.formula('H', charge = 1)@mass + rcdk::get.formula('K', charge = 1)@mass,
                           formula_add = 'K',
                           formula_sub = 'H2',
                           charge = -1),
    '[M+C2H3N-H]-'  = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('C2H3N', charge = 0)@mass - rcdk::get.formula('H', charge = 1)@mass,
                           formula_add = 'C2H3N',
                           formula_sub = 'H',
                           charge = -1),
    '[M+CH2O2-H]-'     = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('CHO2', charge = -1)@mass,
                           formula_add = 'CHO2',
                           formula_sub = 'C0',
                           charge = -1),
    '[M+C2H4O2-H]-'    = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('C2H3O2', charge = -1)@mass,
                           formula_add = 'C2H3O2',
                           formula_sub = 'C0',
                           charge = -1),
    '[M+Br]-'       = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('Br', charge = -1)@mass,
                           formula_add = 'Br',
                           formula_sub = 'C0',
                           charge = -1),
    '[M+C2HO2F3-H]-'= list(mass_multi = 1,
                           mass_add = rcdk::get.formula('C2F3O2', charge = -1)@mass,
                           formula_add = 'C2F3O2',
                           formula_sub = 'C0',
                           charge = -1),
    '[2M-H]-'       = list(mass_multi = 1,
                           mass_add = rcdk::get.formula('C2F3O2', charge = -1)@mass,
                           formula_add = 'C2F3O2',
                           formula_sub = 'C0',
                           charge = -1),
    '[2M+CH2O2-H]-' = list(mass_multi = 2,
                           mass_add = rcdk::get.formula('CHO2', charge = -1)@mass,
                           formula_add = 'CHO2',
                           formula_sub = 'C0',
                           charge = -1),
    '[2M+C2H4O2-H]-'= list(mass_multi = 2,
                           mass_add = rcdk::get.formula('C2H3O2', charge = -1)@mass,
                           formula_add = 'C2H3O2',
                           formula_sub = 'C0',
                           charge = -1),
    '[3M-H]-'      = list(mass_multi = 3 ,
                          mass_add = - rcdk::get.formula('H', charge = 1)@mass,
                          formula_add = 'C0',
                          formula_sub = 'H',
                          charge = -1))
  
  ## return values
  return(adduct_list)
  
}

#' @title Return current adduct naming
#' 
#' This function returns either all or only positive or negative ionization 
#' mode adduct names used in this package.
#' 
#' @param mode Ionization mode for which the adduct names shall be returned, 
#' either \code{all}, \code{positive} or \code{negative}
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules}}
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{adduct_rules_neg}}
#'
#' @export
get_adduct_names <- function(mode ="all") {

  if(mode == "all") {
    
    return(names(metabolomicsUtils::adduct_rules_all()))
    
  } else if(mode == "positive") {
    
    return(names(metabolomicsUtils::adduct_rules_pos()))
    
  } else if(mode == "negative") {
    
    return(names(metabolomicsUtils::adduct_rules_neg()))
    
  } else {
    
    stop("Unknown ion mode")
  }
}