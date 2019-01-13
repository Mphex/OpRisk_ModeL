

my_packages <- c("acepack", "assertthat", "backports", "base64enc", "BH", "bindr", 
                 "bindrcpp", "bitops", "cairoDevice", "caTools", "checkmate", 
                 "chron", "cli", "colorspace", "colourpicker", "corrplot", "crayon", 
                 "crosstalk", "data.table", "dichromat", "digest", "dplyr", "DT", 
                 "evaluate", "filehash", "formatR", "Formula", "futile.logger", 
                 "futile.options", "gdtools", "GGally", "ggplot2", "ggraptR", 
                 "ggthemes", "glm2", "glue", "gridExtra", "gtable", "highr", "Hmisc", 
                 "hms", "htmlTable", "htmltools", "htmlwidgets", "httpuv", "jsonlite", 
                 "knitr", "labeling", "lambda.r", "later", "latticeExtra", "lazyeval", 
                 "magrittr", "markdown", "mime", "miniUI", "MuMIn", "munsell", 
                 "openxlsx", "pacman", "pander", "pillar", "pkgconfig", "plogr", 
                 "plyr", "png", "prettyunits", "progress", "promises", "purrr", 
                 "R6", "randomForest", "rattle", "RColorBrewer", "Rcpp", "remotes", 
                 "reshape", "reshape2", "rggobi", "RGtk2", "rlang", "rmarkdown", 
                 "rpart.plot", "rprojroot", "rstudioapi", "scales", "shiny", "shinyBS", 
                 "shinyjs", "sourcetools", "stargazer", "stringi", "stringr", 
                 "svglite", "tibble", "tidyr", "tidyselect", "tikzDevice", "tinytex", 
                 "utf8", "viridis", "viridisLite", "withr", "xfun", "XML", "xtable", 
                 "yaml", "zip", "base", "boot", "class", "cluster", "codetools", 
                 "compiler", "datasets", "foreign", "graphics", "grDevices", "grid", 
                 "KernSmooth", "lattice", "MASS", "Matrix", "methods", "mgcv", 
                 "nlme", "nnet", "parallel", "rpart", "spatial", "splines", "stats", 
                 "stats4", "survival", "tcltk", "tools", "translations", "utils"
)

for ( p in my_packages) {
  if (!p %in% installed.packages()) {
   install.packages(p)
  }
}
