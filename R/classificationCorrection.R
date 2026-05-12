#' Internal helper to correct raw classification tables
#'
#' This function applies a set of prefix-specific correction rules to a
#' classification table (e.g. from CELLAR or FAO). It is for internal use only
#' and is not part of the public API.
#'
#' @keywords internal
#' @noRd
ClassificationCorrection <- function(classification, prefix) {
  # Basic checks on inputs
  if (!is.data.frame(classification)) {
    stop("`classification` must be a data.frame.")
  }
  
  if (missing(prefix) || !is.character(prefix) || length(prefix) != 1L) {
    stop("`prefix` must be a single character string (e.g. 'nace2', 'cn2022').")
  }
  
  # Normalise prefix (we expect lower-case, consistent with classificationList)
  prefix <- tolower(prefix)
  
  # Identify code and label columns:
  # - code column is assumed to be named as the prefix (e.g. "cn2022")
  # - label column is assumed to be "Name"
  code_col  <- prefix
  label_col <- "Name"
  
  if (!(code_col %in% colnames(classification))) {
    stop(paste0("Column '", code_col, "' not found in `classification`."))
  }
  
  if (!(label_col %in% colnames(classification))) {
    stop("Column 'Name' not found in `classification`.")
  }
  
  # Save original codes and number of rows to detect corrections later
  original_codes <- classification[[code_col]]
  original_nrow  <- nrow(classification)
  
  # Create normalized temporary columns for internal processing
  classification$Code  <- classification[[code_col]]
  classification$Label <- classification[[label_col]]
  
  # ------------------------------------------------------------------
  # 1) Add letters to codes for NACE, NACE 2.1, CPA21, ISIC rev.4
  # ------------------------------------------------------------------
  if (prefix %in% c("nace2", "nace21", "cpa21", "isicrev4")) {
    
    two_digits <- substr(classification$Code, 1, 2)
    
    add_letter <- function(rows, letter) {
      if (length(rows) > 0L) {
        classification$Code[rows] <<- paste0(letter, classification$Code[rows])
      }
    }
    
    add_letter(which(two_digits %in% c("01","02","03")), "A")
    add_letter(which(two_digits %in% c("05","06","07","08","09")), "B")
    add_letter(which(two_digits %in% sprintf("%02d", 10:33)), "C")
    add_letter(which(two_digits == "35"), "D")
    add_letter(which(two_digits %in% c("36","37","38","39")), "E")
    add_letter(which(two_digits %in% c("41","42","43")), "F")
    add_letter(which(two_digits %in% c("45","46","47")), "G")
    add_letter(which(two_digits %in% c("49","50","51","52","53")), "H")
    add_letter(which(two_digits %in% c("55","56")), "I")
    add_letter(which(two_digits %in% c("58","59","60","61","62","63")), "J")
    add_letter(which(two_digits %in% c("64","65","66")), "K")
    add_letter(which(two_digits == "68"), "L")
    add_letter(which(two_digits %in% c("69","70","71","72","73","74","75")), "M")
    add_letter(which(two_digits %in% c("77","78","79","80","81","82")), "N")
    add_letter(which(two_digits == "84"), "O")
    add_letter(which(two_digits == "85"), "P")
    add_letter(which(two_digits %in% c("86","87","88")), "Q")
    add_letter(which(two_digits %in% c("90","91","92","93")), "R")
    add_letter(which(two_digits %in% c("94","95","96")), "S")
    add_letter(which(two_digits %in% c("97","98")), "T")
    add_letter(which(two_digits == "99"), "U")
  }
  
  # ------------------------------------------------------------------
  # 2) ECOICOP: remove trailing ".0" for main groups 10.0, 11.0, 12.0
  # ------------------------------------------------------------------
  if (prefix == "ecoicop") {
    idx <- which(classification$Code %in% c("10.0", "11.0", "12.0"))
    classification$Code[idx] <- c("10", "11", "12")
  }
  
  # ------------------------------------------------------------------
  # 3) PRODCOM 2019: remove weird codes "00.99.t" and "00.99.z"
  # ------------------------------------------------------------------
  if (prefix == "prodcom2019") {
    weird <- which(classification$Code %in% c("00.99.t", "00.99.z"))
    if (length(weird) > 0L) {
      classification <- classification[-weird, , drop = FALSE]
    }
  }
  
  # ------------------------------------------------------------------
  # 4) CN classifications: remove section rows (codes containing letters)
  # ------------------------------------------------------------------
  if (prefix %in% c("cn2017","cn2018","cn2019","cn2020",
                    "cn2021","cn2022","cn2023")) {
    has_letters <- grepl("[A-Za-z]", classification$Code)
    classification <- classification[!has_letters, , drop = FALSE]
  }
  
  # ------------------------------------------------------------------
  # 5) CBF: remove trailing "." from the code (if present)
  # ------------------------------------------------------------------
  if (prefix == "cbf10") {
    classification$Code <- sub("\\.$", "", classification$Code)
  }
  
  # Write the corrected code back into the original code column
  classification[[code_col]] <- classification$Code
  
  # Drop internal helper columns
  classification$Code  <- NULL
  classification$Label <- NULL
  
  # Detect if any correction was applied
  corrections_applied <- !identical(classification[[code_col]], original_codes) ||
    nrow(classification) != original_nrow
  
  attr(classification, "corrections_applied") <- corrections_applied
  
  classification
}
