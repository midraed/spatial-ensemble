#' Produces an spatial ensemble of models 
#' 
#' @param predvalues     Raster stack with the n models.
#' @param serrors        Raster stack with the errors for every
#'                       model in the same order as in predvalues.
#' @param basemap        Integer. The model to select if all the 
#'                       have exactly the same value for serrors.
#'
#' @details 
#' This function ensembles different models selection the model with
#' the lowet error on a pixel by pixel basis.
#' 
#' @return Raster Stack with 3 layers: ensembled model, error and 
#' selected model
#' @author Guillermo Federico Olmedo, Mario Guevara, Gonzalo Gavilán

ensemble <- function(predvalues, serrors, basemap = 1){
  require(raster)
  serrors <- round(serrors, 2)
  result <- predvalues[[basemap]]
  names(result) <- 'result'
  model <- raster(predvalues[[1]])
  values(model) <- basemap
  model[is.na(result)] <- NA
  minerror <- min(stack(serrors))
  names(model) <- "model"
  names(minerror) <- "error"
  for(i in 2:nlayers(predvalues)){
    result[serrors[[i]] == minerror] <- predvalues[[i]][serrors[[i]] == minerror]
    model[serrors[[i]] == minerror] <- i
  }
  minerror <- mask(minerror, result)
  model <- mask(model, result)
  return(stack(result, minerror, model))
}
