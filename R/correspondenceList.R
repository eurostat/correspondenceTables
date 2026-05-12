# This function is deprecated and kept only for backward compatibility.
# Users should now use \code{correspondenceTableList()} instead, which provides
# the same functionality with a clearer and more stable interface.

#   # Deprecated: please use correspondenceTableList() instead.
#   x <- correspondenceList("ALL", showQuery = FALSE)
#   str(x)

correspondenceList <- function(endpoint = "ALL", showQuery = FALSE) {
  .Deprecated("correspondenceTableList")
  correspondenceTableList(endpoint = endpoint, showQuery = showQuery)
}

# correspondenceList <- function(endpoint) {
#   
#   # Normaliser l'endpoint
#   endpoint <- toupper(endpoint)
#   
#   # Cas 1 : on veut tout → on rappelle la fonction pour chaque endpoint
#   if (endpoint == "ALL") {
#     res_cellar <- correspondenceList("CELLAR")
#     res_fao    <- correspondenceList("FAO")
#     
#     return(list(
#       CELLAR = res_cellar,
#       FAO    = res_fao
#     ))
#   }
#   
#   # Vérification basique
#   if (!endpoint %in% c("CELLAR", "FAO")) {
#     stop("endpoint must be one of 'CELLAR', 'FAO' or 'ALL'.")
#   }
#   
#   # cycle = 1 pour CELLAR, 2 pour FAO, comme dans ton code
#   if (endpoint == "CELLAR") {
#     cycle <- 1L
#   } else if (endpoint == "FAO") {
#     cycle <- 2L
#   }
#   
#   data <- list()
#   
#   for (j in cycle) {
#     e <- c("CELLAR", "FAO")[j]
#     
#     # Définition du SPARQL endpoint + séparateur
#     if (e == "CELLAR") {
#       source <- "http://publications.europa.eu/webapi/rdf/sparql"
#       sep    <- "_"
#       rm     <- 1:16
#     } else if (e == "FAO") {
#       source <- "https://stats.fao.org/caliper/sparql/AllVocs"
#       sep    <- "-"
#       rm     <- 1:16
#     }
#     
#     ## Préfixes SPARQL
#     prefixlist <- prefixList(e)
#     prefixlist <- as.character(paste(prefixlist, collapse = "\n"))
#     
#     prefixes_loop <- unlist(
#       lapply(
#         strsplit(as.character(prefixList(e)), " "),
#         function(x) x[2]
#       )
#     )
#     prefixes_loop <- prefixes_loop[-rm]
#     
#     data_t <- list()
#     
#     for (i in seq_along(prefixes_loop)) {
#       prefix <- prefixes_loop[i]
#       
#       SPARQL.query <- paste0(
#         prefixlist, "
#        SELECT ?ID_table ?A ?B ?Table ?URL 
# 
#        WHERE {
#              ?s a xkos:Correspondence ;
#                 skos:prefLabel ?Label .
#     
#             BIND (STR(?s) AS ?URL)
#             BIND (STR(?Label) AS ?Table)
#   
#             BIND (STRAFTER(STR(?s), STR(", prefix, ")) AS ?ID_table)
#             BIND (STRAFTER(STR(?ID_table), '", sep, "') AS ?B)
#             BIND (STRBEFORE(STR(?ID_table), '", sep, "') AS ?A)  
#   
#             FILTER (STRLEN(?ID_table) != 0) 
#         }
#       "
#       )
#       
#       # Appel HTTP : on demande explicitement du CSV, et on récupère le texte brut
#       response <- httr::POST(
#         url    = source,
#         body   = list(query = SPARQL.query),
#         encode = "form",
#         httr::accept("text/csv")
#       )
#       
#       csv_txt <- httr::content(response, as = "text", encoding = "UTF-8")
#       
#       if (!nzchar(csv_txt)) {
#         # Réponse vide : data.frame vide avec les colonnes attendues
#         df <- data.frame(
#           ID_table = character(),
#           A        = character(),
#           B        = character(),
#           Table    = character(),
#           URL      = character(),
#           stringsAsFactors = FALSE
#         )
#       } else {
#         # Lecture du CSV renvoyé
#         df <- utils::read.csv(
#           text = csv_txt,
#           stringsAsFactors = FALSE
#         )
#       }
#       
#       # Ajout de la colonne prefix (même logique que ton code)
#       if (nrow(df) == 0) {
#         df <- cbind(prefix = character(), df)
#       } else {
#         df <- cbind(
#           prefix = rep(gsub(":", "", prefix), nrow(df)),
#           df
#         )
#       }
#       
#       data_t[[i]] <- df
#       names(data_t)[i] <- prefix
#     }
#     
#     data[[j]] <- data_t
#     names(data)[j] <- c("CELLAR", "FAO")[j]
#   }
#   
#   return(data)
# }
