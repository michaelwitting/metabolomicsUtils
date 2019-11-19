#' @title Get the ID of a MoNA entry
#' 
#' This function returns the ID associated with a specific entry in a MoNA JSON object
#' 
#' @param x A single entry of a list from a parsed MoNA JSON
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @export
mona_isolate_id <- function(x) {
  
  if(!is.null(x$id)) {
    return(x$id)
  } else {
    return(NA)
  }
}

#'
#'
#' @export
mona_isolate_smiles <- function(x) {
  
  # isolate metadata of compound
  metaData <- x$compound[[1]]$metaData
  
  # find field with SMILES
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "SMILES") {
      return(metaData[[i]]$value)
      break
    }
  }
}

#'
#'
#' @export
mona_isolate_inchi <- function(x) {
  
  if(!is.null(x$compound[[1]]$inchi)) {
    return(x$compound[[1]]$inchi)
  } else {
    return(NA)
  }
}

#'
#'
#' @export
mona_isolate_splash <- function(x) {
  
  if(!is.null(x$splash$splash)) {
    return(x$splash$splash)
  } else {
    return(NA)
  }
}

#'
#'
#' @export
mona_isolate_precursorMz <- function(x) {
 
  # isolate metadata
  metaData <- x$metaData
  
  # find field with precursor m/z
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "precursor m/z") {
      return(as.numeric(metaData[[i]]$value))
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_collision_energy <- function(x) {
  
  # isolate metadata
  metaData <- x$metaData
  
  # find field with precursor m/z
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "collision energy") {
      return(as.numeric(metaData[[i]]$value))
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_ms_level <- function(x) {
  
  # isolate metadata
  metaData <- x$metaData
  
  # find field with precursor m/z
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "ms level") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_instrument_type <- function(x) {
  
  # isolate metadata
  metaData <- x$metaData
  
  # find field with instrument type
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "instrument type") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_instrument <- function(x) {
  
  # isolate metadata
  metaData <- x$metaData
  
  # find field with instrument type
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "instrument") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_exact_mass <- function(x) {
  
  # isolate metadata of compound
  metaData <- x$compound[[1]]$metaData
  
  # find field with exact mass
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "total exact mass") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_formula <- function(x) {
  
  # isolate metadata of compound
  metaData <- x$compound[[1]]$metaData
  
  # find field with molecular formula
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "molecular formula") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_name <- function(x) {
  
  if(!is.null(x$compound[[1]]$names[[1]]$name)) {
    return(x$compound[[1]]$names[[1]]$name)
  } else {
    return(NA)
  }
}

#'
#'
#' @export
mona_isolate_adduct <- function(x) {
  
  # isolate metadata
  metaData <- x$metaData
  
  # find field with precursor type
  for(i in 1:length(metaData)) {
    if(metaData[[i]]$name == "precursor type") {
      return(metaData[[i]]$value)
      break
    }
  }
  
  return(NA)
  
}

#'
#'
#' @export
mona_isolate_spectrum <- function(x) {
  
  if(!is.null(x$spectrum)) {
    return(x$spectrum)
  } else {
    return(NA)
  }
}

#'
#'
#' @export
mona_parse_spectrum <- function(x) {
  spectrum_df <- as.data.frame(do.call(rbind, strsplit(unlist(strsplit(x, " ")), ":")), stringsAsFactors = FALSE)
  
  spectrum_df$V1 <- as.numeric(spectrum_df$V1)
  spectrum_df$V2 <- as.numeric(spectrum_df$V2)
  
  colnames(spectrum_df) <- c("mz", "intensity")
  
  return(spectrum_df)
  
}
