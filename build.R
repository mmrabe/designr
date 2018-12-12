
devtools::document("package")
devtools::build("package", manual=T)
detach("package:designr")
remove.packages("designr")
archives <- list.files(pattern="^designr_.*\\.tar\\.gz$")
install.packages(archives[length(archives)], repos=NULL)

library(designr)

