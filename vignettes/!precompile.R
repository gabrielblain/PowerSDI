# vignettes that depend on Internet access need to be precompiled and take a
# while to run
library(knitr)
knit(input = "vignettes/Intro_to_PowerSDI.Rmd.orig",
     output = "vignettes/Intro_to_PowerSDI.Rmd")

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/Intro_to_PowerSDI.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
# this replaces the .gif with .png extension, the radar .gif image is converted
# when knitting
replace <- gsub(".gif", ".png", replace)
fileConn <- file("vignettes/Intro_to_PowerSDI.Rmd")
writeLines(replace, fileConn)
close(fileConn)

# build vignettes
library(devtools)
build_vignettes()

# move resource files to /doc
resources <-
  list.files(pattern = ".png$", full.names = TRUE)
file.copy(from = resources,
          to = "doc",
          overwrite =  TRUE)
