# confintr 1.0.2

Hello CRAN team

This update fixes a problem with latex ("text" instead of "textrm") introduced in the last release, yielding warnings/errors on

- r-oldrel-macos-arm64
- r-oldrel-macos-x86_64

I will also fix the same problem in another package of mine.

## Checks

### RevDep

OK: 1
BROKEN: 0

### Local check(): Note

Skipping checking HTML validation: no command 'tidy' found

### Winbuilder

OK

### RHub: Notes

* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

