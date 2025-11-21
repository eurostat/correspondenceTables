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

