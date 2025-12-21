prjdir <- getwd()
library(dartRverse)
setwd('./inst/tutorials/getting-started/www/')
getwd()

tympo.gl <- gl.read.dart('Report_DTym25-13579_SNP.csv',
                         ind.metafile = 'Tympo_ind_metadata.csv')
tympo.gl@other$history
usethis::use_data(tympo.gl, overwrite = TRUE)

tympo.gl.filtered <- gl.load('Tympo_SNP_filtered.RData')
usethis::use_data(tympo.gl.filtered, overwrite = TRUE)

tympo.pca <- gl.pcoa(tympo.gl.filtered)
usethis::use_data(tympo.pca, overwrite = TRUE)

turtles.df <- read.table("my_input.dat")
usethis::use_data(turtles.df, overwrite = TRUE)

setwd(prjdir)
