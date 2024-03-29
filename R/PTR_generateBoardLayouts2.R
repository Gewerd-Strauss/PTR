#' Generate Board-Layouts for a number of pots.
#'
#' Given the dimensions of the pot, the number of required pots and the
#' dimensions of the shelf-boards, calculate the required number of occupied
#' boards. Optionally assign custom labels to them.
#'
#' @param pots The number of pots you must place.
#' @param board_width The width of a single board.
#' A board is defined as one of the 2 single elements used
#' @param board_height The height of a single board
#' A board is defined as one of the 2 single elements used
#' @param pot_radius The radius of a pot (for square and circular pots only)
#' @param pot_diameter The diameter of a pot (for square and circular pots only)
#' @param pot_rectangle_width The width of the pot, if it is a rectangular one.
#' @param pot_rectangle_height The height of the pot, if it is a rectangular one.
#' @param distance The spacing distance between two pots
#' @param lbls A vector of length 'pos' consisting of custom names for each pot
#' Will be assigned in from bottom-left to top-right, from the first to the last
#' board.
#' @param pot_type one of `c("circle","square","rectangle")`.
#' @param rectangle_enforce_given_orientation By default, the pot-fitting algorithm for
#' rectangular pots will try and fit as many pots onto a single board by testing
#' checking the result for the given pot_width/pot_height, and after flipping
#' the values for them, determining if flipping the pots by 90° will allow more
#' pots be fitted onto a single board.
#' @return layouts - list of layouts-objects, each containing a board-plot.
#'
#' @export
#' @name PTR_generateBoardLayouts2
#' @example man/examples/PTR_generateBoardLayouts2.R
PTR_generateBoardLayouts2 <- function(pots = NA, board_width = NA, board_height = NA, pot_radius = NA, pot_diameter = NA, pot_rectangle_width = NA, pot_rectangle_height = NA, distance = NA, pot_type = "circle", lbls = FALSE, rectangle_enforce_given_orientation = FALSE) {
  inputs <- list(pots = pots,
                 board_width = board_width,
                 board_height = board_height,
                 pot_radius = pot_radius,
                 pot_diameter = pot_diameter,
                 pot_rectangle_width = pot_rectangle_width,
                 pot_rectangle_height = pot_rectangle_height,
                 distance = distance,
                 lbls = lbls,
                 pot_type = pot_type,
                 rectangle_enforce_given_orientation =  rectangle_enforce_given_orientation
                 )
  validate2(inputs)
  if (pot_type == "circle") {
    if (!is.na(pot_radius) && is.na(pot_diameter)) {
      # Circular pots
      ret <- get_boards2(board_width, board_height, pot_radius, pot_radius * 2, NA, NA, pots, pot_type)
    }
  } else if (pot_type == "square") {
    if (!is.na(pot_radius) && !is.na(pot_diameter)) {
      # Square pots
      ret <- get_boards2(
        board_width,
        board_height,
        pot_radius,
        pot_diameter,
        pot_radius,
        pot_diameter,
        pots,
        pot_type
      )
    }
  } else if (pot_type == "rectangle") {
    if (!is.na(pot_rectangle_width) && !is.na(pot_rectangle_height)) {
      # Rectangular pots
      ret <- get_boards2(
        board_width,
        board_height,
        NA,
        NA,
        pot_rectangle_width,
        pot_rectangle_height,
        pots,
        pot_type,
        rectangle_enforce_given_orientation
      )
      pot_rectangle_width <- ret[[1]]$pot_rectangle_width
      pot_rectangle_height <- ret[[1]]$pot_rectangle_height
    }
  }

  layouts <- list() ## init holding var
  pot_counter <- 0 # Initialize pot counter

  for (board_INDEX in seq_along(ret)) {
    board <- ret[[board_INDEX]]
    pot_centers <- board$pot_centers

    if (length(pot_centers) == 0) {
      next # Skip this iteration if pot_centers is empty
    }

    # Prepare data frame for ggplot2::ggplot
    if (!isFALSE(is.vector(lbls)) && length(lbls) == pots) {
      df_points <- data.frame(
        x = sapply(pot_centers, function(pot) pot$x),
        y = sapply(pot_centers, function(pot) pot$y),
        Label_ = lbls[pot_counter + seq_along(pot_centers)]
      )
    } else {
      df_points <- data.frame(
        # Adjust x to center the label
        x = sapply(pot_centers, function(pot) pot$x + pot$width / 2),
        # Adjust y to center the label
        y = sapply(pot_centers, function(pot) pot$y + pot$height / 2),
        Label_ = paste0("pot_", pot_counter + seq_along(pot_centers))
      )
    }

    # Increment the pot counter for each pot on the board
    pot_counter <- pot_counter + length(pot_centers)

    # Create common ggplot2::ggplot object
    board_plot <- ggplot2::ggplot(
      df_points,
      ggplot2::aes(x = x, y = y)
    ) +
      # Center the label within each rectangle
      ggplot2::geom_text(
        ggplot2::aes(label = Label_),
        vjust = 0.5,
        hjust = 0.5
      ) +
      ggplot2::ggtitle(paste0("board_", board_INDEX)) +
      ggplot2::xlim(0, board$width) +
      ggplot2::ylim(0, board$height) +
      ggplot2::xlab("[units]") +
      ggplot2::ylab("[units]") +
      ggplot2::theme_minimal()

    # Add appropriate geom based on pot type
    if (pot_type == "circle") {
      board_plot <- board_plot + ggforce::geom_circle(
        ggplot2::aes(
          x0 = x,
          y0 = y,
          r = pot_radius
        ),
        color = "black",
        fill = NA
      )
    } else if (pot_type == "square") {
      board_plot <- board_plot + ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x - pot_radius,
          xmax = x + pot_radius,
          ymin = y - pot_radius,
          ymax = y + pot_radius
        ),
        color = "black",
        fill = NA
      )
    } else if (pot_type == "rectangle") {
      board_plot <- board_plot + ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x - pot_rectangle_width / 2,
          xmax = x + pot_rectangle_width / 2,
          ymin = y - pot_rectangle_height / 2,
          ymax = y + pot_rectangle_height / 2
        ),
        color = "black",
        fill = NA
      )
    }
    board_plot <- board_plot +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = 0,
          xmax = board$width,
          ymin = 0,
          ymax = board$height
        ),
        color = "red",
        fill = NA
      )

    layouts[[paste0("board_", board_INDEX)]] <- list(
      board_plot = board_plot,
      points = df_points,
      input = inputs,
      version = 2
    )
  }
  return(layouts)
}
