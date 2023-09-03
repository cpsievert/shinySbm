### diapo

[test CRAN](https://cran.r-project.org/web/packages/submission_checklist.html)

todo users :
- vignette
- video

todo mineure :
- print table search on columns
- copy paste on app
- digits comme paramètres globaux ou parematers flotant dans (print table)


### Typos ou mise en forme/harmonisation


### Exploration

- Loupe pour réduire la matrice en fonction d'une valeur : ne fonctionne que sur les lignes et pas sur les colonnes
(testé en bipartite)

### Pb ??

Je n'ai pas réussi à voir les matrices que ce soit brutes, ou réordonnées ou attendues (test avec version Migale)






ideas from factoshiny : see on package files

Get Home Language : `strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]` 

Traduction : `gettext("Which graphs to use?",domain="R-Factoshiny")` 

Rapport automatique : `FactoInvestigate::Investigate` 





1.  All this tabset :
 -   A tabset for data uploading
 \|(bigger panel) uploading option\| print small head\|
 -   I want to mix raw_table and matrix_plot in one tabset maybe (`plotMat`)
 \|plot/print parameters\|(bigger panel) the plot/print\|saving the plot option\|
 -   I whish that there is like in the `{BlockmodelingGUI}` to implement a tabset for modeling
 \|sbm option (bigger panel) \|saving the result in a csv option\|
 -   Then 2 exploration tabset :
 - One for ordered matrix :
 \|plot parameters\|(bigger panel) the plot\|saving the plot option\|
 - One for network plot :
 \|plot parameters\|(bigger panel) the plot\|saving the plot option\|

2.  For all those tabset :
 I want to put a verbatim windows to show the print and other message shown by s3 sbmMatrix managment

Very important to remind : see file : dev/02_dev.R
 - `verbatimTextOutput("summary")`
 - `blockmodeling::plotMat(UniTable$matrix)`
 - `x <- igraph::graph.adjacency(adjmatrix = UniTable$matrix, add.rownames = TRUE)`
 - `igraph::plot.igraph(x)`

Here UniTable is an sbmMatrix class object, function `plotMat` could be used because it's quite beautiful.

