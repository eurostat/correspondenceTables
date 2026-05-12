pkgs<-c("pkgdown","pak")
pkgs_to_install<-pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(pkgs_to_install)>0) install.packages(pkgs_to_install)


pak::pak("eurostat/correspondenceTables")
cat(getwd())
message(getwd())
pkgdown::build_site()
system("git config --global --add safe.directory /__w/correspondenceTables/correspondenceTables")
