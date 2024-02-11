#' Generate Board-Layouts for a number of pots.
#'
#' Given the dimensions of the pot, the number of required pots and the
#' dimensions of the shelf-boards, calculate the required number of occupied
#' boards. Optionally assign custom labels to them.
#'
#' WARNING: This function does not support rectangular pots. For that, use
#' `PTR_generateBoardLayouts2`
#'
#' @param pots The number of pots you must place
#' @param board_width The width of a single board.
#' A board is defined as one of the 2 single elements used
#' @param board_height The height of a single board.
#' See 'board_width' for details
#' @param pot_radius The radius of a pot
#' @param distance The spacing distance between two pots.
#' @param lbls vector of length 'pots' consisting of custom names for each pot.
#' Will be assigned in from bottom-left to top-right
#' @return layouts - list of layout_objects, each containing a board-plot.
#' Additionally, the input data and a mapping of coordinates to pot-names
#' plotted is returned
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_length
#' @export
#' @name PTR_generateBoardLayouts
#' @example man/examples/PTR_generateBoardLayouts.R
PTR_generateBoardLayouts <- function(pots = NA, board_width = NA, board_height = NA, pot_radius = NA, distance = NA, lbls = FALSE) {
  inputs <- list(
    pots = pots,
    board_width = board_width,
    board_height = board_height,
    pot_radius = pot_radius,
    distance = distance,
    lbls = lbls
  )
  validate1(inputs)
  ret <- get_boards(board_width, board_height, pot_radius, distance, pots)
  pots_in_tray <- ret$pots_in_tray
  boards_required <- ret$boards_required
  layouts <- list() ## init holding var


  #* 1. Define layout
  #*    1. plot the confines of a Tray
  #*    2. generate the ruling on how to place trays
  #*       (cf. assets/layout_scheme_example.png)
  #*    3. generate rules on how to place pots within trays
  #*       1. no overlap between trays, so each tray must contain the
  #*          exact number of pots defined
  #*       2. trays are numbered front>back, top>bottom, right>left
  #*       3.
  #*
  pots_total <- pots
  pots_plottable <- pots_in_tray$n_circles
  label_start <- end_index <- plotted_pots <- 0
  start_index <- 1
  # TODO:  how to determine that on runs 2+
  # , maybe not the full number of pots must be plotted?
  for (board_INDEX in 1:1:boards_required) {
    rectangle_x <- c(0, board_width)
    rectangle_y <- c(0, board_height)
    x <- pots_in_tray$pos$p_x[1:pots_plottable]
    y <- pots_in_tray$pos$p_y[1:pots_plottable]
    x <- stats::na.omit(x) # clean out out-of-bounds elements
    y <- stats::na.omit(y)
    if (isFALSE(as.logical(lbls))) {
      # generate automatic labels if labels were not supplied
      labels <- str_c(
        "pot_",
        pad_to_length(
          label_start + 1:(pots_in_tray$n_circles),
          str_length(pots)
        )
      )
      inputs$fed_labels <- inputs$lbls
      inputs$lbls <- labels
    } else {
      K <- pots_in_tray$n_circles
      V <- lbls
      # Calculate start and end indices for the subset to be used
      if (end_index != 0) {
        start_index <- end_index + 1
      }
      end_index <- start_index + K - 1
      if (end_index > length(V)) {
        end_index <- length(V)
      }
      # Extract the subset from vector V
      labels <- V[start_index:end_index]
      inputs$fed_labels <- inputs$lbls
      inputs$lbls <- labels
    }
    # guard against length inequalities arising from
    # generalised population of vectors in previous step
    if (length(labels) > (pots - plotted_pots)) {
      labels <- labels[1:(pots - plotted_pots)]
    }
    if (length(x) > (pots - plotted_pots)) {
      x <- x[1:(pots - plotted_pots)]
    }
    if (length(y) > (pots - plotted_pots)) {
      y <- y[1:(pots - plotted_pots)]
    }

    circle <- function(x, y, pot_radius) {
      data.frame(
        x = x,
        y = y,
        r = pot_radius
      )
    }

    df_points <- data.frame(x,
      y,
      Label_ = labels[1:1:length(x)]
    )
    limits_y <- c(-1, board_height)
    limits_x <- c(-1, board_width)
    board_plot <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df_points,
        ggplot2::aes(
          x = x,
          y = y
        ),
        color = "black"
      ) +
      ggplot2::geom_text(
        data = df_points,
        ggplot2::aes(
          x = x,
          y = y,
          label = Label_
        ),
        vjust = -0.5
      ) + # add labels
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = rectangle_x[1],
          xmax = rectangle_x[2],
          ymin = rectangle_y[1],
          ymax = rectangle_y[2]
        ),
        alpha = 0,
        color = "black"
      ) +
      ggforce::geom_circle(
        data = circle(
          x,
          y,
          pot_radius
        ),
        ggplot2::aes(
          x0 = x,
          y0 = y,
          r = r
        ),
        color = "blue",
        fill = NA
      ) +
      ggplot2::scale_x_continuous(
        name = "[units]",
        n.breaks = 2 * length(unique(x)) + 1,
        limits = limits_x
      ) +
      ggplot2::scale_y_continuous(
        name = "[units]",
        n.breaks = 2 * length(unique(y)) + 1,
        limits = limits_y
      ) +
      ggplot2::ggtitle(
        label = paste("Board ", board_INDEX)
      ) +
      ggplot2::theme_minimal()
    rownames(df_points) <- df_points$Label_
    df_points <- subset(df_points, select = c("Label_"))
    packaged_output <- list(
      board_plot = board_plot,
      points = as.data.frame(df_points),
      input = inputs
    ) ## we package the mapping so we have easier access to it later on if we want to randomise shuffling in a controlled manner.
    layouts[[paste0("board_", pad_to_length(board_INDEX, str_length(boards_required)))]] <- packaged_output
    if (isFALSE(as.logical(lbls))) {
      label_start <- label_start + pots_in_tray$n_circles
      plotted_pots <- plotted_pots + pots_in_tray$n_circles
    } else {
      plotted_pots <- plotted_pots + pots_in_tray$n_circles
    }
  }
  return(layouts)
}
# suppress warnings
utils::globalVariables(c("prob", "section", "y", "Label_", "r", "x", "label"))
