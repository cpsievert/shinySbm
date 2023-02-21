library(sbm)
library(ggplot2)
library(dplyr)
library(reshape2)


plot_sbm <- function(fit, ordered = F, transpose = NULL, values = T, prediction = T, labels = list(row = "row", col = "col"),
                     col_pred = "red", col_value = scales::muted("black", l = 0)) {
  UseMethod(".plot_sbm", fit)
}


.plot_sbm.BipartiteSBM_fit <- function(fit, ordered = F, transpose = F, values = T, prediction = T, labels = list(row = "row", col = "col"),
                                       col_pred = "red", col_value = scales::muted("black", l = 0)) {
  clustering <- setNames(fit$memberships, c("row", "col"))
  if (ordered) {
    oRow <- order(clustering$row, decreasing = !transpose)
    oCol <- order(clustering$col, decreasing = transpose)
    uRow <- cumsum(table(clustering$row)) + 0.5
    uCol <- cumsum(table(clustering$col)) + 0.5
    tCol <- if (transpose) {
      uRow
    } else {
      uCol
    }
    tCol <- tCol[-length(tCol)]
    tRow <- if (transpose) {
      uCol
    } else {
      uRow
    }
    tRow <- tRow[length(tRow)] - tRow[-length(tRow)] + 0.5
    mat_exp <- fit$connectParam$mean[clustering$row, clustering$col][oRow, oCol]
    mat_pure <- fit$networkData[oRow, oCol]
  } else {
    tRow <- NULL
    tCol <- NULL
    mat_exp <- fit$connectParam$mean[clustering$row, clustering$col]
    mat_pure <- fit$networkData
  }

  plot_net <- reshape2::melt(mat_exp) %>%
    dplyr::mutate(base_value = reshape2::melt(mat_pure)$value)

  if (transpose) {
    names(plot_net)[c(1, 2)] <- c("Var2", "Var1")
  }

  plt <- ggplot2::ggplot(data = plot_net, ggplot2::aes(x = Var2, y = Var1, fill = base_value, alpha = base_value))
  if (prediction) {
    plt <- plt +
      ggplot2::geom_tile(ggplot2::aes(x = Var2, y = Var1, alpha = value), fill = col_pred, size = 0, show.legend = FALSE)
  }
  if (values) {
    plt <- plt +
      ggplot2::geom_tile(show.legend = FALSE)
  }
  plt <- plt +
    ggplot2::geom_hline(
      yintercept = tRow,
      col = col_pred, size = .3
    ) +
    ggplot2::geom_vline(
      xintercept = tCol,
      col = col_pred, size = .3
    ) +
    ggplot2::scale_fill_gradient(
      low = scales::muted("white"), high = col_value,
      guide = "colourbar"
    ) +
    ggplot2::xlab(if (transpose) {
      labels$row
    } else {
      labels$col
    }) + ggplot2::ylab(if (transpose) {
      labels$col
    } else {
      labels$row
    }) +
    ggplot2::scale_alpha(range = c(0, 1)) +
    ggplot2::scale_x_discrete(breaks = "") +
    ggplot2::scale_y_discrete(breaks = "", guide = ggplot2::guide_axis(angle = 0)) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw(base_size = 20, base_rect_size = 1, base_line_size = 1) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())
  plot(plt)
}


.plot_sbm.SimpleSBM_fit <- function(fit, ordered = F, prediction = T, values = T, labels = list(row = "nodes", col = "nodes"),
                                    col_pred = "red", col_value = scales::muted("black", l = 0), ...) {
  if (length(labels) == 1) {
    labels <- list(row = labels, col = labels)
  }
  clustering <- list(row = fit$memberships, col = fit$memberships)
  if (ordered) {
    oRow <- order(clustering$row)
    fRow <- cumsum(table(clustering$row))
    uRow <- fRow + 0.5
    bRow <- c(0,fRow[-length(fRow)])
    tCol <- bRow + (fCol - bRow)/2
    tRow <- rev(tCol)
    uCol <- uRow[-length(uRow)]
    uRow <- uRow[length(uRow)] - uRow[-length(uRow)] + 0.5
    mat_exp <- fit$connectParam$mean[clustering$row, clustering$col][oRow, oRow]
    mat_pure <- fit$networkData[oRow, oRow]
    text_net <- reshape2::melt(round(fit$connectParam$mean,1)) %>%
      dplyr::mutate(Var2 = tCol[Var2],Var1 = tRow[Var1])
  } else {
    text_net <- NULL
    uRow <- NULL
    uCol <- NULL
    mat_exp <- fit$connectParam$mean[clustering$row, clustering$col]
    mat_pure <- fit$networkData
  }
  nb_rows <- dim(mat_pure)[1]
  mat_exp <- mat_exp[nb_rows:1, ]
  mat_pure <- mat_pure[nb_rows:1, ]

  plot_net <- reshape2::melt(mat_exp) %>%
    dplyr::mutate(base_value = reshape2::melt(mat_pure)$value)
  ## Test


  plt <- ggplot2::ggplot()

  if (prediction) {
    plt <- plt +
      ggplot2::geom_tile(ggplot2::aes(x = plot_net$Var2, y = plot_net$Var1, alpha = plot_net$value), fill = col_pred, size = 0, show.legend = FALSE)
  }

  if (values) {
    plt <- plt +
      ggplot2::geom_tile(ggplot2::aes(x = plot_net$Var2, y = plot_net$Var1, fill = plot_net$base_value, alpha = plot_net$base_value),show.legend = FALSE)
  }

  plt <- plt +
    ggplot2::geom_hline(
      yintercept = uRow,
      col = col_pred, size = .3
    ) +
    ggplot2::geom_vline(
      xintercept = uCol,
      col = col_pred, size = .3
    ) +
    ggplot2::scale_fill_gradient(
      low = scales::muted("white"), high = col_value,
      guide = "colourbar"
    ) +
    ggplot2::geom_text(ggplot2::aes(x = text_net$Var2, y = text_net$Var1, label = text_net$value, size = text_net$value),col = 'black', show.legend = FALSE) +
    ggplot2::ylab(labels$row) + ggplot2::xlab(labels$col) +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::scale_x_discrete(breaks = "") +
    ggplot2::scale_y_discrete(breaks = "", guide = ggplot2::guide_axis(angle = 0)) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw(base_size = 20, base_rect_size = 1, base_line_size = 1) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())
  plot(plt)
}

data_bi <- fungusTreeNetwork$fungus_tree
data_uni <- fungusTreeNetwork$tree_tree
my_sbm_bi <- estimateBipartiteSBM(data_bi)
my_sbm_uni <- estimateSimpleSBM(data_uni, model = "poisson")


labels <- list(col = "tree", row = "fungus")



plot_sbm(my_sbm_bi, ordered = T, transpose = F, prediction = T, values = F, col_pred = "deeppink3", col_value = "darkblue", labels = labels)

plot_sbm(my_sbm_uni, ordered = F, prediction = T, values = F, labels = labels)

fit$connectParam$mean

text_net
