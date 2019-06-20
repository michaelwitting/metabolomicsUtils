#' This function parses a chemical chem_formula into a named vector
#' @param chem_formula Single string with chemical chem_formula
#' @return Named vector with all elements
#' @examples
#' parseChemFormula("C6H12O6")
#' @export
formula_to_list <- function(chem_formula) {
  
  # regex pattern to isolate all elements
  element_pattern <- "([A][cglmrstu]|[B][aehikr]?|[C][adeflmnorsu]?|[D][bsy]|[E][rsu]|[F][elmr]?|[G][ade]|[H][efgos]?|[I][nr]?|[K][r]?|[L][airuv]|[M][cdgnot]|[N][abdehiop]?|[O][gs]?|[P][abdmortu]?|[R][abefghnu]|[S][bcegimnr]?|[T][abcehilms]|[U]|[V]|[W]|[X][e]|[Y][b]?|[Z][nr])([0-9]*)"
  
  # extract all matching pattern
  regexMatch <- stringr::str_extract_all(chem_formula, element_pattern)
  
  # get individual elements and their count
  elements <- stringr::str_extract(regexMatch[[1]], "[aA-zZ]+")
  numbers <- as.numeric(stringr::str_extract(regexMatch[[1]], "[0-9]+"))
  
  # replace NAs with 1 for elements which have a count of one
  numbers[is.na(numbers)] <- 1
  
  # create named vector for return
  formula_list <- numbers
  names(formula_list) <- elements
  
  # remove atoms that might have a count of 0
  formula_list <- formula_list[which(formula_list >= 0)]
  
  return(formula_list)
}

#' This function generates a chemical chem_formula from a named vector of elemental counts
#' @param formula_list Named list containing the elements as names and their abudance as values
#' @return Single string with chemical chem_formula
#' @examples
#' chem_formula <- list("C" = 6,
#'                 "H" = 12,
#'                 "O" = 6)
#' generateformula(chem_formula)
#' @export
list_to_formula <- function(formula_list) {
  
  # create empty string to append parts of chem_formula
  chem_formula <- ""
  
  # first C H N O S P, then elements by alphabetical order
  for(atom in c("C", "H", "N", "O", "S", "P")) {
    if (!is.na(formula_list[atom])) {
      if (formula_list[atom] == 1.0) {
        chem_formula <- paste0(chem_formula, atom)
      } else {
        chem_formula <- paste0(chem_formula, atom, formula_list[atom])
      }
    }
  }
  
  # get all remaining elements
  restElements <- names(formula_list)
  restElements <- restElements[!restElements %in% c("C", "H", "N", "O", "S", "P")]
  
  # iterate through all remaining elements in alphabetical order
  for (atom in sort(restElements)) {
    if (formula_list[atom] == 1.0) {
      chem_formula <- paste0(chem_formula, atom)
    } else {
      chem_formula <- paste0(chem_formula, atom, formula_list[atom])
    }
  }
  
  # return chem_formula
  return(chem_formula)
}

#' This function standardizes a supplied chemical chem_formula according to the Hill notation system.
#' @param chem_formula Single string with chemical chem_formula
#' @return Single string with standardized chemical formua
#' @examples
#' standardizeChemFormula("O6C6H12")
#' @export
standardize_formula <- function(chem_formula) {
  
  # parse and reconstruct
  formula_list <- formula_to_list(chem_formula)
  std_chem_formula <- list_to_formula(formula_list)
  
  # return
  return(std_chem_formula)
}

#'
#'
#' @export
formula_to_exactmass <- function(chem_formula, ...) {
  
  exact_mass <- rcdk::get.formula(chem_formula, ...)@mass
  
  return(exact_mass)
  
}
