#' @importFrom data.table fread fwrite
#' @importFrom stats aggregate
#' @importFrom utils menu read.csv tail write.csv txtProgressBar setTxtProgressBar

.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
      "Acode", "Alevel", "Asuperior",
      "Bcode", "Blevel", "Bsuperior"
    ))
  }
  # Forcer la méthode "libcurl" pour download.file et url()
  options(download.file.method = "libcurl")
  options(url.method = "libcurl")
}
