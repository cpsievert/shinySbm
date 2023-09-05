## First changes

add "console_verbosity" agrument on `shinySbmApp` function

## Last advice

We see:
       Found the following (possibly) invalid URLs:
     URL: https://cran.r-project.org/web/packages/sbm/index.html
       From: inst/doc/ShinySbmApp.html
       Status: 200
       Message: OK
       CRAN URL not in canonical form
     The canonical URL of the CRAN page for a package is
       https://CRAN.R-project.org/package=pkgname

Please proof-read your description text.
Currently it reads: "... It also contain useful functions ..." and "...
as learning the basics functions of 'sbm'."
Probably it should be: "... It also contains useful functions ..." and "
... as learning the basic functions of 'sbm'."


In addition:

The maintainer should only be a single person (no mailing list)

## Changes done 

 - URL: https://cran.r-project.org/web/packages/sbm/index.html to https://CRAN.R-project.org/package=sbm
 - description text : "contain" to "contains" and "basics" to "basic"
 - Creating Author and Maintainer fields this way:
 
Author: Theodore Vanrenterghem [cre, aut],
        Julie Auber [aut]
Maintainer: Theodore Vanrenterghem <shiny.sbm.dev@gmail.com>

error 0 | warn 0 | note 0
