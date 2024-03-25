rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(102191, 200177, 410551, 552581, 892237)






for ( vcp in c( -0.5, 0 , 0.1 ) )
  for ( vmax_depth in c(4,6,8,10,12,14,16) )
    for( vmin_split in c(10,20,40,80,160,320,640) )
      for( vmin_bucket in c(2,4,8,16,32, min_split/4 ) )
      {
        gan <- ArbolEstimarGanancia( datos, cp=vcp, max_depth=vmax_depth, min_split=vmin_split,min_bucket=vmin_bucket)
        
        grabar(vcp, vmax_depthm, vmin_split, vmin_bucket, gan)
      }
