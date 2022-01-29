This is mainly a maintenance release:

- removing testthat::context()
- getting rid of CRAN note on LazyData
- improving the way how the package itself is being generated.

## Checks

### Local check seems fine

`devtools::check()` result:

   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

0 errors √ | 1 warning x | 0 notes √

### Online checks seem fine as well:

- check_win_devel()
- check_rhub()

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors √ | 0 warnings √ | 1 note x
