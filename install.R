#setwd("/data/Tutos/R/creerPackage/")
devtools::install_github("klutometis/roxygen")
require(roxygen2)
#create("r2spmf") # Pour la première création uniquement
setwd("/data/Tutos/R/creerPackage/r2spmf")
document()
?SequentialRules
