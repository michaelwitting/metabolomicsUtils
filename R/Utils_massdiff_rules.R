## functions for mass differences
massdiff_rules <- function() {
  
  ## list of mass differences
  massdiff_list <- list('trihexose' = list(formula = "C18H30O15",
                                           mass = rcdk::get.formula('C18H30O15', charge = 0)@mass,
                                           polarity = "+"),
                        'dihexose'  = list(formula = "C12H20O10",
                                           mass = rcdk::get.formula('C6H10O5', charge = 0)@mass,
                                           polarity = "+"),
                        'hexose'    = list(formula = 'C6H10O5',
                                           mass = rcdk::get.formula('C6H10O5', charge = 0)@mass,
                                           polarity = "+"),
                        'pentose'   = list(formula = 'C5H8O4',
                                           mass = rcdk::get.formula('C6H10O5', charge = 0)@mass,
                                           polarity = "+"))
  
}