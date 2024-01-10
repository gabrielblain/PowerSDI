# PowerSDI 1.0.0

## Major Change

* Replace the suspicious values for `PowerSDI.ScientSDI` with the limits provided by the users.

# PowerSDI 0.1.3

## Minor Changes

* Document the `\value` for the custom `print()` for `PowerSDI.Accuracy` objects as requested by CRAN maintainers.

* Standardise `\value` sections in {ROxygen} portions of each file.

* Remove unimported package, {utils}.

* Remove unused functions from `@importFrom` in {ROxygen} portions of several files.

# PowerSDI 0.1.2

## Minor Changes

* Add CITATION file.

* Ensure that user's options for `par()` are not modified after using `PlotData()`.

* Correct the "Value" for `PlotData()` in documentation.

* Standardise the use of "Standardise" vs. "Standardize" in package, use en-GB.

* Add reference to Description field describing the methods used in this package.

* Internal changes to reduce code complexity and runtime.

# PowerSDI 0.1.1

## Minor Changes

* Add a list of words in DESCRIPTION to `.aspell/PowerSDI.rds` for "non-existing words" to avoid CRAN NOTES.

* Add a CITATION file for proper package citations.

* Add a full suite of tests to ensure the package's functionality is accurate.

* Add a vignette for users to refer to.

* Cache POWER data in-session so that users do not experience repeated downloads of the same data, resulting in a faster response to functions that use the same data sets.

* Use best practices for errors, warnings and messages related to user inputs and calculations.

* Pre-calculate values where possible to save computational time.

* Only run examples in an interactive session so that they are not run on CRAN due to long running calculations and/or relying on downloaded data from the POWER API that may fail if there is no Internet connection or the POWER API is down which would have resulted in failures on CRAN.

* De-duplicate functionality in the code base related to downloading data and calculating values.

# PowerSDI 0.1.0

* Initial CRAN submission.
