# limpio la memoria
rm( list=ls() )  # remove all objects
gc()             # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b1/" )  # establezco la carpeta donde voy a trabajar
# cargo el dataset
dataset <- fread( "./datasets/dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/canarios/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("./exp/canarios/")

# uso esta semilla para los canaritos
set.seed(606323)


numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]

numeric_cols <- numeric_cols [-which(numeric_cols  == "numero_de_cliente")]
numeric_cols <- numeric_cols [-which(numeric_cols  == "foto_mes")]

columnas_filtradas <- numeric_cols[sapply(numeric_cols, function(col) {
  # Verificar si la columna contiene más de dos valores únicos
  if (length(unique(dataset[[col]])) > 2) {
    return(TRUE)
  }
  # Verificar si la columna contiene valores diferentes de 0 y 1
  if (!all(dataset[[col]] %in% c(0, 1), na.rm = TRUE)) {
    return(TRUE)
  }
  return(FALSE)
})]
sample_columnas <- sample(columnas_filtradas, 100, replace = TRUE)
combinations <- CJ(sample_columnas, sample_columnas)



for (i in 1:nrow(combinations)) {
  col1 <- as.character(combinations[i, 1])
  col2 <- as.character(combinations[i, 2])
  cat(col1,col2)
  if(col1!=col2){
    try({
  new_col_name <- paste0(col1, "_divided_by_", col2)
  dataset[, (new_col_name) := dataset[[col1]] / dataset[[col2]]]
    })
  }
}


# agrego los siguientes canaritos
for( i in 1:154 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


# Usted utilice sus mejores hiperparamatros
# yo utilizo los encontrados por Elizabeth Murano
 modelo  <- rpart(formula= "clase_ternaria ~ .",
               data= dataset[ foto_mes==202107,],
               model = TRUE,
               xval = 0,
               cp = -0.5,
               minsplit =  600,
               minbucket = 150,
               maxdepth = 6)


#pdf(file = "./arbol_canaritos_consumo_visa.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
importancia <- modelo$variable.importance

plot(importancia, xlab="variable", 
     ylab="Importance", xaxt = "n", pch=20)
axis(1, at=1:310, labels=names(dataset))
library(vip)
vip(modelo,num_features = 100,height = 100)
plot(vi(modelo))
print(vi(modelo),n=100)
