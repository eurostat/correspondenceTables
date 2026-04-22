
# This function is deprecated and kept only for backward compatibility.
# Users should now use \code{classificationList()} instead, which provides
# the same functionality with a clearer and more stable interface.

#   Deprecated: please use classificationList() instead.
#   cl <- classificationEndpoint("ALL", showQuery = FALSE)
#   print(cl)

 classificationEndpoint <- function(endpoint = "ALL", showQuery = FALSE) {
   .Deprecated("classificationList")
   classificationList(endpoint = endpoint, showQuery = showQuery)
 }
 
# classificationEndpoint <- function(endpoint = "ALL") {
#   
#   endpoint <- match.arg(endpoint, c("ALL", "CELLAR", "FAO"))
#   
#   #-----------------------------#
#   # Helper : CELLAR             #
#   #-----------------------------#
#   fetch_cellar <- function() {
#     endpoint_cellar <- "http://publications.europa.eu/webapi/rdf/sparql"
#     
#     SPARQL.query_cellar <- "
#       SELECT DISTINCT ?s ?Title
#       WHERE { ?s a skos:ConceptScheme ;
#               skos:prefLabel ?Title ;
#               ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> 
#                FILTER (LANG(?Title) = 'en')}
#       ORDER BY ?Title
#     "
#     
#     resp <- tryCatch(
#       httr::POST(
#         url   = endpoint_cellar,
#         httr::accept("text/csv"),
#         body  = list(query = SPARQL.query_cellar),
#         encode = "form"
#       ),
#       error = function(e) {
#         warning(
#           sprintf(
#             "CELLAR endpoint is currently unavailable (network error: %s). Classifications from CELLAR will not be returned.",
#             conditionMessage(e)
#           ),
#           call. = FALSE
#         )
#         return(NULL)
#       }
#     )
#     
#     if (is.null(resp)) return(NULL)
#     
#     if (httr::http_error(resp)) {
#       status <- httr::status_code(resp)
#       warning(
#         sprintf(
#           "CELLAR endpoint is currently unavailable (HTTP status %s). Classifications from CELLAR will not be returned.",
#           status
#         ),
#         call. = FALSE
#       )
#       return(NULL)
#     }
#     
#     data_cellar <- utils::read.csv(
#       text = httr::content(resp, "text", encoding = "UTF-8"),
#       sep  = ",",
#       stringsAsFactors = FALSE
#     )
#     
#     if (nrow(data_cellar) == 0L) {
#       warning("CELLAR SPARQL query returned no rows. Classifications from CELLAR will not be returned.",
#               call. = FALSE)
#       return(NULL)
#     }
#     
#     # --- Post-traitement ---
#     uri_vec <- data_cellar[, 1]
#     title   <- data_cellar[, 2]
#     
#     str_list <- strsplit(as.character(uri_vec), "/+")
#     mat_str  <- suppressWarnings(do.call(rbind, str_list))
#     
#     if (ncol(mat_str) < 5) {
#       warning(
#         "CELLAR URI structure not as expected; cannot derive Prefix/ConceptScheme. Classifications from CELLAR will not be returned.",
#         call. = FALSE
#       )
#       return(NULL)
#     }
#     
#     uri <- paste0(mat_str[, 1], "/", "/", mat_str[, 2], "/", mat_str[, 3], "/", mat_str[, 4])
#     prefix <- gsub("\\.", "", mat_str[, 4])
#     conceptscheme <- mat_str[, 5]
#     
#     out <- data.frame(
#       Prefix        = prefix,
#       ConceptScheme = conceptscheme,
#       URI           = uri,
#       Title         = title,
#       stringsAsFactors = FALSE
#     )
#     rownames(out) <- NULL
#     out
#   }
#   
#   #-----------------------------#
#   # Helper : FAO                #
#   #-----------------------------#
#   fetch_fao <- function(
#     endpoint_fao = getOption(
#       "correspondenceTables.fao_endpoint",
#       "https://caliper.integratedmodelling.org/caliper/sparql"
#     )
#   ) {
#     SPARQL.query_fao <- "
#       PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
#       PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#       PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
# 
#       SELECT ?scheme ?notation ?label_en WHERE {
#         ?scheme rdf:type skos:ConceptScheme .
#         ?scheme skos:notation ?notation .
#         ?scheme skos:prefLabel ?label_en . FILTER(lang(?label_en)='en') .
#       }
#       ORDER BY ASC(?notation)
#     "
#     
#     resp <- tryCatch(
#       httr::POST(
#         url   = endpoint_fao,
#         httr::accept("text/csv"),
#         body  = list(query = SPARQL.query_fao),
#         encode = "form"
#       ),
#       error = function(e) {
#         warning(
#           sprintf(
#             "FAO endpoint is currently unavailable (network error: %s). Classifications from FAO will not be returned.",
#             conditionMessage(e)
#           ),
#           call. = FALSE
#         )
#         return(NULL)
#       }
#     )
#     
#     if (is.null(resp)) return(NULL)
#     
#     if (httr::http_error(resp)) {
#       status <- httr::status_code(resp)
#       warning(
#         sprintf(
#           "FAO endpoint is currently unavailable (HTTP status %s). Classifications from FAO will not be returned.",
#           status
#         ),
#         call. = FALSE
#       )
#       return(NULL)
#     }
#     
#     data_fao <- utils::read.csv(
#       text = httr::content(resp, "text", encoding = "UTF-8"),
#       sep  = ",",
#       stringsAsFactors = FALSE
#     )
#     
#     if (nrow(data_fao) == 0L) {
#       warning("FAO SPARQL query returned no rows. Classifications from FAO will not be returned.",
#               call. = FALSE)
#       return(NULL)
#     }
#     
#     uri_vec <- data_fao[, 1]
#     prefix  <- data_fao[, 2]
#     
#     str_list   <- strsplit(uri_vec, "/")
#     mat_str_dt <- suppressWarnings(do.call(rbind, str_list))
#     df_str_dt  <- as.data.frame(mat_str_dt, stringsAsFactors = FALSE)
#     
#     if (ncol(df_str_dt) >= 4) {
#       uri_root <- paste(df_str_dt[, 1], df_str_dt[, 2], df_str_dt[, 3], df_str_dt[, 4], sep = "/")
#       uri <- paste(uri_root, prefix, sep = "/")
#     } else {
#       uri <- uri_vec
#     }
#     
#     if (ncol(df_str_dt) >= 6) {
#       ConceptScheme <- paste0(df_str_dt[, 5], df_str_dt[, 6])
#     } else if (ncol(df_str_dt) >= 5) {
#       ConceptScheme <- df_str_dt[, 5]
#     } else {
#       ConceptScheme <- prefix
#     }
#     
#     out <- data.frame(
#       Prefix        = prefix,
#       ConceptScheme = ConceptScheme,
#       URI           = uri,
#       Title         = data_fao[, 3],
#       stringsAsFactors = FALSE
#     )
#     rownames(out) <- NULL
#     out
#   }
#   
#   #-----------------------------#
#   # Corps principal             #
#   #-----------------------------#
#   res <- list()
#   
#   if (endpoint %in% c("ALL", "CELLAR")) {
#     cellar <- fetch_cellar()
#     if (!is.null(cellar)) res$CELLAR <- cellar
#   }
#   
#   if (endpoint %in% c("ALL", "FAO")) {
#     fao <- fetch_fao()
#     if (!is.null(fao)) res$FAO <- fao
#   }
#   
#   if (endpoint == "ALL") {
#     if (!("CELLAR" %in% names(res))) {
#       message("CELLAR endpoint is unavailable; only FAO classifications are returned.")
#     }
#     if (!("FAO" %in% names(res))) {
#       message("FAO endpoint is unavailable; only CELLAR classifications are returned.")
#     }
#   }
#   
#   if (endpoint != "ALL" && length(res) == 0L) {
#     stop("Endpoint '", endpoint, "' is currently unavailable (see warnings above).", call. = FALSE)
#   }
#   
#   if (endpoint == "ALL" && length(res) == 0L) {
#     stop("No endpoint could be reached (CELLAR and FAO both failed).", call. = FALSE)
#   }
#   
#   # ---- Logique de sortie ----
#   if (endpoint == "ALL") {
#     return(res)      # LISTE
#   } else {
#     return(res[[1]]) # DATA.FRAME
#   }
# }
