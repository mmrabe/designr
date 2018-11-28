
devtools::document("package")
devtools::build("package", manual=T)
detach("package:designr")
remove.packages("designr")
install.packages("designr_0.1.2.tar.gz", repos=NULL)

library(designr)

