
detach("package:designr")
devtools::document(".")
devtools::build(".", manual=T)
remove.packages("designr")
archives <- list.files("..", pattern="^designr_.*\\.tar\\.gz$", full.names = T)
install.packages(archives[length(archives)], repos=NULL)
