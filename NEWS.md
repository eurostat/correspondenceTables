# correspondenceTables 0.8.1

- adding the functions `prefixList()`,  `classEndpoint()`, `correspondenceList()`, `retrieveCorrespondenceTable()`, `retrieveClassificationTable()`, `structureData()`

# correspondenceTables 0.8.0

- adding test cases
- corrections for the `newCorrespondenceTable()` and `updateCorrespondenceTable()` functions

# correspondenceTables 0.7.5

- updating the vignette showing the initializing code part

# correspondenceTables 0.7.4

- aligning to the second round of comments from CRAN 
- replacing `readr::write_excel_csv()` with `data.table::fwrite()`
- removing absolute paths from vignettes and examples
- the example of `newCorrespondenceTable()` using only the 100 hundred rows of the classifications
- more details in the error messages
- CRAN release

# correspondenceTables 0.7.3

- aligning to the comments from CRAN 
- using `txtProgressBar()` and `setTxtProgressBar()` functions
- replacing `cat()` with `message()` function
- examples updated to be able to test it automatically 

# correspondenceTables 0.7.2

- adjustment for CRAN approval

# correspondenceTables 0.7.1

- initial public release on GitHub


# correspondenceTables 0.10.22

## 🔧 Changes and Improvements

- Rewrote the logic for `classificationList()` and `correspondenceTableList()` to handle the `"ALL"` endpoint through recursive calls, ensuring consistent and robust behavior.
- Internal function `prefixList()` is now no longer exported, as it is not meant for end-users. It remains accessible via `correspondenceTables:::prefixList()` if needed.
- Cleaned up the "RetrieveClassificationsAndCorrespondenceTables" vignette:
  - Restored uncommented example code (e.g., `correspondenceTableList("ALL")`) to improve readability and reproducibility.
  - Removed internal calls to `prefixList()` from examples to avoid confusion.
- Fixed vignette rendering errors by defining missing intermediate objects (e.g., `result_ls`).
- Improved `@examples` sections and corrected Rd documentation accordingly.

##  Package Infrastructure

- Reduced export clutter by making utility-only functions internal.
- Ensured full `R CMD check` compliance (0 errors, 0 warnings, 1 note on line width).

##  Minor

- Cleaned up documentation and fixed formatting for clarity.
- Updated vignettes titles to ensure consistency between YAML and `\VignetteIndexEntry`.

## correspondenceTables 0.10.29
----------------------------

New features
~~~~~~~~~~~~
* Added `correspondenceList()` to list available correspondence tables in CELLAR and FAO,
  with a structured output and improved robustness against malformed SPARQL responses.

* Extended `classificationList()` with:
  - `endpoint` ("CELLAR", "FAO", "ALL")
  - `showQuery` to return SPARQL queries.
  Now supports offline mode via `options(useLocalDataForVignettes = TRUE)` using
  bundled CSV files for reproducible vignette builds and offline work.

Improvements and API changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* `correctionClassification()`:
  - Added new `prefix` argument to enable prefix-specific corrections.
  - Replaced SPARQL example with static demo data (`inst/extdata/Nace2.csv`) so
    that examples work offline and during CRAN checks.
  - Documentation enhanced with clearer input/output description.

* `lengthsFile()`:
  - Improved detection of inconsistent code lengths across levels.
  - More explicit warnings when code hierarchies do not behave as expected.

* `retrieveClassificationTable()`:
  - Cleaned all non-ASCII characters in comments and messages.
  - Strengthened error handling when SPARQL queries fail or return unexpected
    number of columns.
  - Added explicit imports for `utils::capture.output` and `utils::str`.

Quality control module
~~~~~~~~~~~~~~~~~~~~~~
* `classificationQC()` functionality has been temporarily disabled for this
  release (tests skipped), but its supporting imports (`stringr`, `tools`) remain
  in place for the upcoming version.

Testing improvements
~~~~~~~~~~~~~~~~~~~~
* Added new unit tests for:
  - `dataStructure()` : validation of returned level/size structure.
  - `prefixList()` : consistency of prefix extraction for each endpoint.
  - `retrieveCorrespondenceTable()` : correct parsing, duplicate checks, error handling.
  - `retrieveClassificationTable()` : correct structure, column naming, error detection.

* Updated existing tests for:
  - `analyseCorrespondenceTable()` : better handling of missing files, mismatches,
    duplicates, and filtering options.
  - `aggregateCorrespondenceTable()` : improved robustness when validating AB tables.



# correspondenceTables 0.10.30

- Cleaned helpfile to hide internal utility functions from end users.
- Removed obsolete function `structureData()` and related documentation.
- Marked `classificationEndpoint()` and `correspondenceList()` as deprecated,
  in favour of `classificationList()` and `correspondenceTableList()`.
- Harmonised user-facing extraction functions:
  all now expose a `showQuery` argument with consistent ordering of parameters.
- Clarified in the documentation which functions accept both data.frames
  and CSV file paths as input (e.g. `classificationQC()`).
- Added basic unit tests to ensure internal utilities are not exported and
  that the interface of extraction functions is stable.

- Reworked `classificationList()` to:
  - use a JSON configuration file for endpoint URLs with sensible fallbacks,
  - provide a stable `Prefix` (lower-case, no spaces) compatible with
    `retrieveClassificationTable()`,
  - add a `Languages` column summarising all languages used in concept labels,
  - support offline mode via the global option `useLocalDataForVignettes`.

- Made `classificationEndpoint()` a thin deprecated wrapper around
  `classificationList()`, with dedicated tests checking argument names,
  returned values and deprecation warnings.

- Updated `aggregateCorrespondenceTable()` and
  `analyseCorrespondenceTable()` to:
  - accept both CSV file paths and data.frames for all table inputs (AB, A, B),
  - standardise input validation through `testInputTable()`,
  - improve error messages for duplicate codes, missing values and hierarchy
    inconsistencies,
  - keep `CSVout` as the last argument and validate it via `testCsvParameter()`.

- Simplified `retrieveClassificationTable()`:
  - removed the obsolete `localData` argument (behaviour now governed solely by
    `options(useLocalDataForVignettes = TRUE/FALSE)`),
  - ensured that both offline (embedded CSV) and online (SPARQL) modes respect
    the `showQuery` contract (`data.frame` vs list with `SPARQL.query`),
  - documented the argument order (`endpoint`, `prefix`, `conceptScheme`,
    `language`, `level`, `CSVout`, `showQuery`).

- Aligned the interface of `retrieveCorrespondenceTable()` so that `endpoint`
  precedes `prefix` and updated the documentation and tests accordingly.

- Updated `lengthsFile()` to:
  - clarify that `correction` is a Boolean argument,
  - handle special classifications (CN, PRODICOM, CPA, etc.) in a more explicit
    and documented way,
  - issue clearer warnings when code lengths are inconsistent across levels.

- Reordered arguments of `newCorrespondenceTable()` so that:
  - the logical/semantic arguments come first (`Tables`, `Reference`,
    `MismatchTolerance`, `Redundancy_trim`),
  - `CSVout` is now the final, optional output argument,
  - examples and tests now call the function with explicit argument names to
    avoid ambiguity.

- Updated `updateCorrespondenceTable()` so that:
  - all classification-related inputs (`A`, `B`, `Astar`, `AB`, `AAstar`)
    accept either CSV file paths or data.frames,
  - `CSVout` is the last argument,
  - input validation and CSV export are routed through the same shared helpers
    as the other correspondence-table functions.
