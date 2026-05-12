#' Internal helper to derive code segment boundaries by level
#'
#' Computes, for a given classification, the character positions corresponding
#' to each hierarchical level within the compact code (i.e., without dots and
#' spaces). Used internally by higher-level routines; not part of the public API.
#'
#' @param endpoint Character; SPARQL endpoint (e.g., "CELLAR", "FAO").
#' @param prefix Character; catalogue prefix identifying the classification
#'   (e.g., "nace2", "cn2022").
#' @param conceptScheme Character; concept scheme identifier used by the SPARQL calls.
#' @param correction Logical; if TRUE (default), applies a small set of
#'   classification-specific corrections used in production.
#'
#' @return A data.frame with two columns:
#' \itemize{
#'   \item \code{charb}: starting character position (1-based) of the segment
#'         corresponding to each hierarchical level (in the compact code),
#'   \item \code{chare}: ending character position (1-based) of that segment.
#' }
#'
#' @keywords internal
#' @noRd
lengthsFile <- function(endpoint, prefix, conceptScheme, correction = TRUE) {
  
  # ---- Normalize and validate inputs ---------------------------------------
  endpoint <- .validate_endpoints(endpoint)  # accepts "CELLAR" / "FAO" (and would throw on invalid)
  if (identical(endpoint, "ALL") || length(endpoint) != 1L) {
    stop("'endpoint' must be a single endpoint (e.g., 'CELLAR' or 'FAO'), not 'ALL' or a vector.",
         call. = FALSE)
  }
  prefix <- tolower(trimws(prefix))
  
  if (!is.logical(correction) || length(correction) != 1L || is.na(correction)) {
    stop("'correction' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  
  # ---- 1) Retrieve level metadata ------------------------------------------
  level_dt <- dataStructure(endpoint, prefix, conceptScheme)
  
  # Apply small, classification-specific ordering/removal corrections used in production
  if (isTRUE(correction)) {
    # PRODCOM: reorder two highest levels in some vintages
    if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022")) {
      # Expecting at least 3 rows; guard with drop=FALSE
      idx <- c(2, 1, 3)
      idx <- idx[idx <= nrow(level_dt)]
      if (length(idx)) level_dt <- level_dt[idx, , drop = FALSE]
    }
    
    # CN: drop the first level
    if (prefix %in% c("cn2017", "cn2018", "cn2019", "cn2020", "cn2021", "cn2022", "cn2023")) {
      if (nrow(level_dt) >= 2) level_dt <- level_dt[-1, , drop = FALSE]
    }
    
    # CPA 2.1: custom level ordering observed in some providers
    if (prefix %in% c("cpa21")) {
      # Make sure indices are within bounds
      idx <- c(1, 2, 3, 6, 4, 5)
      idx <- idx[idx <= nrow(level_dt)]
      if (length(idx)) level_dt <- level_dt[idx, , drop = FALSE]
    }
  }
  
  # For other classifications, ensure levels are in ascending order
  # based on the second column (assumed hierarchy depth indicator).
  special_prefix <- c(
    "prodcom2019", "prodcom2021", "prodcom2022",
    "cn2017", "cn2018", "cn2019", "cn2020", "cn2021", "cn2022", "cn2023",
    "cpa21"
  )
  if (!(prefix %in% special_prefix)) {
    ord <- order(suppressWarnings(as.numeric(level_dt[, 2])))
    if (length(ord)) level_dt <- level_dt[ord, , drop = FALSE]
  }
  
  nL <- nrow(level_dt)
  if (nL == 0L) {
    stop("dataStructure() returned no levels for this classification.", call. = FALSE)
  }
  
  # Assume the second column contains the level identifier (provider-dependent)
  level_ids <- level_dt[, 2]
  
  # Storage for total compact-length per level, and segment bounds
  total_len <- rep(NA_integer_, nL)
  charb     <- rep(NA_integer_, nL)
  chare     <- rep(NA_integer_, nL)
  
  # ---- 2) Iterate over levels ----------------------------------------------
  for (l in seq_len(nL)) {
    
    # Retrieve codes for a specific level
    cl <- retrieveClassificationTable(
      endpoint      = endpoint,
      prefix        = prefix,
      conceptScheme = conceptScheme,
      level         = level_ids[l]
    )
    dt <- cl$ClassificationTable
    
    # ---- Classification-specific corrections (kept for backward compatibility)
    if (isTRUE(correction)) {
      
      # ECOICOP: remove ".0" for 10, 11, 12 at level 1 only
      if (prefix %in% c("ecoicop") && l == 1L) {
        idx <- which(dt[, 1] %in% c("10.0", "11.0", "12.0"))
        if (length(idx) > 0L) dt[idx, 1] <- c("10", "11", "12")
      }
      
      # PRODCOM: remove rare odd codes at level 1
      if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022", "prodcom2023", "prodcom2024", "prodcom2025", "prodcom2026") && l == 1L) {
        bad <- which(dt[, 1] %in% c("00.99.t", "00.99.z"))
        if (length(bad) > 0L) dt <- dt[-bad, , drop = FALSE]
      }
      
      # NACE / NACE 2.1 / CPA / ISIC: prepend "A" for levels > 1 (provider quirk)
      if (prefix %in% c("nace2", "nace21", "cpa21", "isicrev4") && l > 1L) {
        dt[, 1] <- paste0("A", dt[, 1])
      }
      
      # ICC_v11: add leading zero at level 2 only
      if (prefix %in% c("icc_v11") && l == 2L) {
        dt[, 1] <- sprintf("%.2f", dt[, 1])
      }
    }
    
    # ---- 3) Clean codes: remove spaces and dots to get compact form ----------
    codes       <- as.character(dt[, 1])
    codes_clean <- gsub("[ .]", "", codes)
    len_unique  <- unique(nchar(codes_clean))
    
    if (length(len_unique) != 1L || is.na(len_unique[1L])) {
      # Inconsistent code length for this level → cannot compute segment bounds reliably
      total_len[l] <- NA_integer_
      charb[l]     <- NA_integer_
      chare[l]     <- NA_integer_
      
      warning(
        "Inconsistent code lengths at level ", level_ids[l],
        " for prefix ", prefix, ". Segment boundaries may be unreliable for this level."
      )
      
    } else {
      total_len[l] <- len_unique[1L]
      
      if (l == 1L) {
        # First level: entire code is the first segment
        charb[l] <- 1L
        chare[l] <- total_len[l]
      } else {
        # Subsequent levels: require strictly increasing compact length
        if (is.na(total_len[l - 1L]) || total_len[l] <= total_len[l - 1L]) {
          charb[l] <- NA_integer_
          chare[l] <- NA_integer_
          
          warning(
            "Non-increasing or invalid compact length from level ", level_ids[l - 1L],
            " to level ", level_ids[l], " for prefix ", prefix,
            ". Segment boundaries may be unreliable for this level."
          )
          
        } else {
          seg_len  <- total_len[l] - total_len[l - 1L]
          charb[l] <- chare[l - 1L] + 1L
          chare[l] <- chare[l - 1L] + seg_len
        }
      }
    }
  }
  
  # ---- 3) Final table -------------------------------------------------------
  lengths <- data.frame(
    charb = charb,
    chare = chare,
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(lengths$charb) | is.na(lengths$chare))) {
    warning(
      "Some levels have missing segment boundaries. ",
      "Please verify the classification data; the result may not be fully reliable."
    )
  }
  
  if (identical(correction, FALSE)) {
    warning(
      "Classification-specific corrections are disabled (correction = FALSE). ",
      "Results may differ from production behavior."
    )
  }
  
  lengths
}