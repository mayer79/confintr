# New release

Hi CRAN team

This release fixes a mistake in the calculation of studentized boostrap confidence intervals.

## Checks

### Local check seems fine

`devtools::check()` result:

❯ checking for unstated dependencies in examples ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 1 warning ✖ | 1 note ✖

### `check_rhub()`

Ubuntu Linux (release)

* checking for future file timestamps ... NOTE
unable to verify current time

Win (devel)

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

Fedora Linux (devel)

* checking for future file timestamps ... NOTE
unable to verify current time
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
