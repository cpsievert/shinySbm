## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

add "console_verbosity" argument on `shinySbmApp` function


## last comment :

Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles.
 From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all
components of the package must be clear and unambiguous (including from
the authors specification in the DESCRIPTION file). Where code is copied
(or derived) from the work of others (including from R itself), care
must be taken that any copyright/license statements are preserved and
authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the
authors of such code. Alternatively, the ‘Author’ field should list
these authors as contributors. Where copyrights are held by an entity
other than the package authors, this should preferably be indicated via
‘cph’ roles in the ‘Authors@R’ field, or using a ‘Copyright’ field (if
necessary referring to an inst/COPYRIGHTS file)."
e.g.: Golem User in your LICENSE file
Please explain in the submission comments what you did about this issue.


## Authors@R:  was

  c(person(given = "Theodore", family = "Vanrenterghem", 
           email = "shiny.sbm.dev@gmail.com", role = c("cre", "aut")),
    person(given = "Julie", family = "Aubert", role = c("aut"),
           email = "julie.aubert@inrae.fr",
           comment = c(ORCID = "0000-0001-5203-5748")),
    person("großBM team", role = c("cph")),
    person("ThinkR", role = "cph")))

I think the last problem was coming from "ThinkR" company's name not fitting with the LICENCE name "Golem User".
The company name is "ThinkR" and they are using a licence named differently. I'm not really sure about this though... I can't find anywhere indication about this issue. I choose to try with licence name as copyright holder 

## Authors@R: is

  c(...
    person(given = "Saint-Clair", family = "Chabert-Liddell", role = c("aut"),
           email = "saint-clair.chabert-liddell@agroparistech.fr",
           comment = c(ORCID = "0000-0001-5604-7308")), ## add another person that gave me a piece of code.
    person("großBM team", role = c("ctb")), 
    person("Golem User", role = "cph")))
    
    
    
    
