# shinySbm 0.0.0.9000

What I want to add :

<<<<<<< HEAD
1.  All this tabset :
=======
1.   All this tabset :
>>>>>>> 0cb2cac1c2d2c6e1b6aa99c0c3078611d477399a

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

<<<<<<< HEAD
2.  For all those tabset :
=======
2.   For all those tabset :
>>>>>>> 0cb2cac1c2d2c6e1b6aa99c0c3078611d477399a

    I want to put a verbatim windows to show the print and other message shown by s3 sbmMatrix managment

Very important to remind : see file : dev/02_dev.R

-   `verbatimTextOutput("summary")`
-   `blockmodeling::plotMat(UniTable$matrix)`
-   `x <- igraph::graph.adjacency(adjmatrix = UniTable$matrix, add.rownames = TRUE)`
-   `igraph::plot.igraph(x)`

Here UniTable is an sbmMatrix class object, function `plotMat` could be used because it's quite beautiful.
