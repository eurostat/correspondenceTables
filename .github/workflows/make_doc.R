if (!("pkgdown" %in% rownames(installed.packages()))){install.packages("pkgdown")}
if (!("devtools" %in% rownames(installed.packages()))){install.packages("devtools")}
devtools::install_github("eurostat/correspondenceTables")
cat(getwd())
message(getwd())
pkgdown::build_site()
