#' @title Aggregate values from classification A to classification B
#'
#' @description
#' Aggregates numeric values from a source classification A into a target
#' classification B using a correspondence table AB.  
#' Each value in A is redistributed to one or more B codes according to weights
#' in AB (if available), or split equally when no valid weights exist.
#'
#' @details
#' This function is used when data expressed in one classification must be
#' converted into another classification using an A→B correspondence table.
#'
#' The workflow is:
#' \enumerate{
#'   \item Detect and standardize column names in \code{AB}, \code{A} and \code{B}
#'         (case-insensitive).
#'   \item For each code in A, determine all corresponding B codes.
#'   \item Apply proportional allocation:
#'         \itemize{
#'           \item If \code{AB} contains a numeric weight column, weights
#'                 are normalized per source code.
#'           \item If weights are missing, invalid, or sum to zero, values are
#'                 split equally across all mapped B codes.
#'         }
#'   \item Values are multiplied by the resulting weights and aggregated by B code.
#'   \item If \code{B} is provided, the result is aligned to its list of codes
#'         (unmatched codes receive 0).
#' }
#'
#' This function is appropriate for tasks such as converting statistical datasets
#' from NACE to CPA, CPA to CN, CPC to HS, or any situation where values have to
#' be re-expressed according to a different classification system.
#'
#' @param AB A \code{data.frame} containing A→B links, with optional numeric
#'           \code{weight}.
#' @param A  A \code{data.frame} with one (character) code column and one
#'           numeric value column.
#' @param B  Optional \code{data.frame} with a code column defining the output
#'           domain. Additional columns (e.g., labels) are preserved.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{result}: data.frame with aggregated values for each B code
#'         (and any additional columns from \code{B}, if provided),
#'   \item \code{mapping}: standardized A→B mapping used internally,
#'   \item \code{diagnostics}: list with total input value, mapped value,
#'         coverage ratio, and codes in A that could not be mapped.
#' }
#'
#' @examples
#' ## Simple example (equal split)
#' AB <- data.frame(
#'   from_code = c("A1","A1","A2"),
#'   to_code   = c("B1","B2","B1")
#' )
#'
#'print(AB)
#'
#' A_tbl <- data.frame(
#'   code  = c("A1","A2"),
#'   value = c(100, 50)
#' )
#'
#'print(A_tbl)
#'
#' result <- aggregateCorrespondenceTable(AB, A_tbl)
#' 
#' print(result$result) 
#' print(result$mapping)
#'
#' @seealso \code{\link{retrieveCorrespondenceTable}} for downloading AB tables.
#'
#' @export

aggregateCorrespondenceTable <- function(AB, A, B = NULL) {
  # --- helpers ------------------------------------------
  stop_if <- function(cond, msg) if (isTRUE(cond)) stop(msg, call. = FALSE)
  
  trim_chr <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  
  # case-insensitive named-extractor for lists or named atomic vectors
  guess_col <- function(df, candidates, want_numeric = FALSE) {
    cols <- names(df)
    # direct match (case-sensitive)
    for (c in candidates) if (nzchar(c) && c %in% cols) return(c)
    # case-insensitive match
    low <- tolower(cols)
    for (c in candidates) {
      idx <- which(low == tolower(c))
      if (length(idx) == 1) return(cols[idx])
    }
    # fallback by type
    if (want_numeric) {
      nums <- cols[vapply(df, is.numeric, logical(1))]
      if (length(nums)) return(nums[1])
    } else {
      chrs <- cols[vapply(df, function(z) !(is.numeric(z) || is.logical(z)), logical(1))]
      if (length(chrs)) return(chrs[1])
    }
    NULL
  }
  
  # --- accept only data.frames ----------------------------------------------
  stop_if(!is.data.frame(AB), "Argument 'AB' must be a data.frame.")
  stop_if(!is.data.frame(A),  "Argument 'A' must be a data.frame.")
  if (!is.null(B)) stop_if(!is.data.frame(B), "Argument 'B' must be a data.frame or NULL.")
  
  # ----- Standardization helpers --------------------------------------------
  # Standardize AB to: code_A, code_B, weight (normalized per code_A)
  
  standardize_AB <- function(AB_df, A_codes = NULL) {
    
    # 1) First try explicit known names (your existing logic)
    A_cand <- c("code_A","from_code","A","from","source","a_code","Acode")
    B_cand <- c("code_B","to_code","B","to","target","dest","b_code","Bcode")
    W_cand <- c("weight","w","share","split","prop","ratio","coef")
    
    cA <- guess_col(AB_df, A_cand, want_numeric = FALSE)
    cB <- guess_col(AB_df, B_cand, want_numeric = FALSE)
    cW <- guess_col(AB_df, W_cand, want_numeric = TRUE)
    
    # 2) If explicit names NOT found, use a smarter heuristic
    if (is.null(cA) || is.null(cB) || identical(cA, cB)) {
      
      nms <- names(AB_df)
      nms_lc <- tolower(nms)
      
      # (a) ignore typical metadata columns from retrieveCorrespondenceTable()
      ignore_pat <- "^(correspondenceid|prefix|uri|url|table\\.name|comment)$|label|include|exclude|broader"
      keep <- !grepl(ignore_pat, nms_lc)
      
      # candidate columns are non-numeric and not ignored
      cand <- nms[keep & vapply(AB_df, function(z) !(is.numeric(z) || is.logical(z)), logical(1))]
      if (length(cand) < 2) {
        stop("Could not detect A/B code columns in 'AB'. Please provide columns like 'from_code'/'to_code' or two code columns.", call. = FALSE)
      }
      
      # (b) If CorrespondenceID exists like "NACE2_CPA21" and columns NACE2 + CPA21 exist, use them
      if ("CorrespondenceID" %in% nms) {
        id <- unique(as.character(AB_df[["CorrespondenceID"]]))
        id <- id[!is.na(id) & nzchar(id)]
        if (length(id)) {
          toks <- strsplit(id[1], "_", fixed = TRUE)[[1]]
          if (length(toks) >= 2 && all(toks[1:2] %in% nms)) {
            cA <- toks[1]
            cB <- toks[2]
          }
        }
      }
      
      # (c) If still not found, choose cA as the column that best matches A_codes
      if (is.null(cA) || is.null(cB) || identical(cA, cB)) {
        score_overlap <- function(col) {
          v <- trimws(as.character(AB_df[[col]]))
          v <- v[!is.na(v) & nzchar(v)]
          if (is.null(A_codes)) return(0L)
          sum(v %in% A_codes)
        }
        
        scores <- vapply(cand, score_overlap, integer(1))
        ord <- order(scores, decreasing = TRUE)
        
        cA <- cand[ord[1]]
        cB <- cand[ord[2]]
        
        # If overlap is useless (all 0), fall back to the first two remaining candidates
        if (all(scores == 0L)) {
          cA <- cand[1]
          cB <- cand[2]
        }
      }
    }
    
    # ---- Now proceed with your existing standardization logic ----
    code_A <- trim_chr(AB_df[[cA]])
    code_B <- trim_chr(AB_df[[cB]])
    keep   <- !is.na(code_A) & !is.na(code_B)
    
    out <- data.frame(code_A = code_A[keep], code_B = code_B[keep], stringsAsFactors = FALSE)
    stop_if(nrow(out) == 0, "Argument 'AB' has no valid records.")
    
    if (!is.null(cW) && is.numeric(AB_df[[cW]])) {
      w <- AB_df[[cW]][keep]
      w[!is.finite(w)] <- NA_real_
      out$weight <- as.numeric(w)
      out <- stats::aggregate(weight ~ code_A + code_B, data = out, FUN = function(z) sum(z, na.rm = TRUE))
      out <- out[, c("code_A","code_B","weight")]
    } else {
      n_per_A <- table(out$code_A)
      out$weight <- 1 / as.numeric(n_per_A[match(out$code_A, names(n_per_A))])
    }
    
    split_idx <- split(seq_len(nrow(out)), out$code_A)  
    for (idx in split_idx) {
      sw <- sum(out$weight[idx], na.rm = TRUE)
      if (isTRUE(sw > 0)) {
        out$weight[idx] <- out$weight[idx] / sw
      } else {
        out$weight[idx] <- 1 / length(idx)
      }
    }
    out
  }
  # Standardize A to: code_A, value
  standardize_A <- function(A_df) {
    A_code_cand <- c("code_A","A","from","source","code","item","prod","id","Acode")
    A_val_cand  <- c("value","val","amount","qty","quantity","count","obs_value","measure","num","n")
    
    cA <- guess_col(A_df, A_code_cand, want_numeric = FALSE)
    cV <- guess_col(A_df, A_val_cand,  want_numeric = TRUE)
    stop_if(is.null(cA), "Could not detect code column in 'A' (e.g., 'code').")
    stop_if(is.null(cV) || !is.numeric(A_df[[cV]]),
            "Could not detect numeric value column in 'A' (e.g., 'value').")
    
    code_A <- trim_chr(A_df[[cA]])
    value  <- as.numeric(A_df[[cV]])
    data.frame(code_A = code_A, value = value, stringsAsFactors = FALSE)
  }
  
  # Standardize B to: code_B (+ extra cols kept)
  standardize_B <- function(B_df) {
    if (is.null(B_df)) return(NULL)
    B_code_cand <- c("code_B","B","to","target","dest","code","id","Bcode")
    cB <- guess_col(B_df, B_code_cand, want_numeric = FALSE)
    stop_if(is.null(cB), "Could not detect code column in 'B' (e.g., 'code').")
    out <- B_df
    names(out)[names(out) == cB] <- "code_B"
    out$code_B <- trim_chr(out$code_B)
    out
  }
  
  # --- Standardize inputs ---------------------------------------------------
  A_std  <- standardize_A(A)
  AB_std <- standardize_AB(AB, A_codes = unique(A_std$code_A))
  B_std  <- standardize_B(B)
  
  # --- Join and proportional allocation ------------------------------------
  joined <- merge(A_std, AB_std, by = "code_A", all.x = TRUE)
  
  mapped_flag <- !is.na(joined$code_B)
  total_value <- sum(A_std$value, na.rm = TRUE)
  
  # multiply and aggregate (treat NA weights as 0)
  w <- joined$weight
  w[is.na(w)] <- 0
  joined$adj_value <- joined$value * w
  
  mapped_value      <- sum(joined$adj_value[mapped_flag], na.rm = TRUE)
  covered_a_codes   <- unique(joined$code_A[mapped_flag])
  is_not_empty_code <- function(x) !is.na(x) & nzchar(x)
  unmapped_a_codes  <- setdiff(unique(A_std$code_A[is_not_empty_code(A_std$code_A)]), covered_a_codes)
  
  # keep only mapped
  joined <- joined[mapped_flag, c("code_B","adj_value"), drop = FALSE]
  
  # aggregate by code_B
  if (nrow(joined)) {
    agg <- stats::aggregate(adj_value ~ code_B, data = joined, FUN = function(x) sum(x, na.rm = TRUE))
    names(agg) <- c("code_B","value")
  } else {
    agg <- data.frame(code_B = character(0), value = numeric(0), stringsAsFactors = FALSE)
  }
  
  # right-join with B domain if provided (preserve B; fill 0)
  if (!is.null(B_std)) {
    agg <- merge(B_std, agg, by = "code_B", all.x = TRUE, sort = FALSE)
    if (!("value" %in% names(agg))) agg$value <- 0
    agg$value[is.na(agg$value)] <- 0
    # put code_B first
    pos <- match("code_B", names(agg))
    ord <- c(pos, setdiff(seq_along(names(agg)), pos))
    agg <- agg[, ord, drop = FALSE]
  }
  
  diagnostics <- list(
    total_value_in_A       = total_value,
    mapped_value_to_B      = mapped_value,
    mapping_coverage_ratio = if (isTRUE(total_value > 0)) mapped_value / total_value else NA_real_,
    unmapped_A_codes       = unmapped_a_codes
  )
  
  list(result = agg, mapping = AB_std, diagnostics = diagnostics)
}

