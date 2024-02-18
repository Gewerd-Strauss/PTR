#' Title
#' @inheritParams PTR_generateBoardLayouts2
#'
#' @return a 'board'-object containing
#' @keywords internal
#' @noRd
#
get_boards2 <- function(board_width, board_height, pot_radius, pot_diameter, pot_rectangle_width, pot_rectangle_height, pots, pot_type, rectangle_enforce_given_orientation = FALSE) {
  boards <- list()
  pots_placed_total <- 0 # Initialize total pots placed across all boards
  total_pots_available <- pots

  add_board <- function(board_width, board_height, pot_radius, pot_diameter, pot_rectangle_width, pot_rectangle_height) {
    board <- list(
      width = board_width,
      height = board_height,
      pot_radius = pot_radius,
      pot_diameter = pot_diameter,
      pot_rectangle_width = pot_rectangle_width,
      pot_rectangle_height = pot_rectangle_height,
      pot_count = 0,
      pot_centers = list()
    )
    return(board)
  }

  add_pot_rect <- function(board, pot_rectangle_width, pot_rectangle_height, total_pots_placed, total_pots_available) {
    # this function returns 3 centers even though 4 should fit.
    # do a set of regression tests using testthat to check against couple dimensions
    remaining_pots <- total_pots_available - total_pots_placed
    pots_to_add <- min(length(board$pot_centers) + remaining_pots, total_pots_available)
    while (length(board$pot_centers) < pots_to_add) { ## add the first pot
      if (board$pot_count == 0) {
        board$pot_count <- 1
        board$pot_centers[[1]] <- list(x = pot_rectangle_width / 2, y = pot_rectangle_height / 2, width = pot_rectangle_width / 2, height = pot_rectangle_height / 2)
      } else {
        x <- board$pot_centers[[board$pot_count]]$x ## get last coords
        y <- board$pot_centers[[board$pot_count]]$y

        if ((x + (pot_rectangle_width / 2) + pot_rectangle_width) <= board$width) { # we are on a an unfinished row that can still fit another pot to the right
          board$pot_count <- board$pot_count + 1
          board$pot_centers[[board$pot_count]] <- list(x = x + pot_rectangle_width, y = y, width = pot_rectangle_width / 2, height = pot_rectangle_height / 2)
        } else {
          ## we are at the end of the row.
          x <- 0
          y <- y + pot_rectangle_height
          if (y + (pot_rectangle_height / 2) <= board$height) {
            board$pot_count <- board$pot_count + 1
            board$pot_centers[[board$pot_count]] <- list(x = x + pot_rectangle_width / 2, y = y, width = pot_rectangle_width / 2, height = pot_rectangle_height / 2)
          } else {
              message("i Reached maximum height of the board")
            break
          }
        }
      }
    }
    return(board)
  }
  add_pot <- function(board, pot_radius, pot_diameter, total_pots_placed, total_pots_available) {
    remaining_pots <- total_pots_available - total_pots_placed
    pots_to_add <- min(length(board$pot_centers) + remaining_pots, total_pots_available)

    while (length(board$pot_centers) < pots_to_add) {
      if (board$pot_count == 0) {
        board$pot_count <- 1
        board$pot_centers[[1]] <- list(x = pot_radius, y = pot_radius, pot_radius = pot_radius, diameter = pot_diameter)
      } else {
        x <- board$pot_centers[[board$pot_count]]$x
        y <- board$pot_centers[[board$pot_count]]$y

        if (x + pot_diameter <= board$width && y + (pot_diameter / 2) <= board$height) {
          board$pot_count <- board$pot_count + 1
          board$pot_centers[[board$pot_count]] <- list(x = x + 2 * pot_radius, y = y, pot_radius = pot_radius, diameter = pot_diameter)
        } else {
          x <- pot_radius
          y <- y + 2 * pot_radius
          if (y + 1 * pot_radius <= board$height) {
            board$pot_count <- board$pot_count + 1
            board$pot_centers[[board$pot_count]] <- list(x = x, y = y, pot_radius = pot_radius, diameter = pot_diameter)
          } else {
            message("i Reached maximum height of the board")
            break
          }
        }
      }
    }
    return(board)
  }
  if (pot_type == "rectangle") {
    # 1. iterate once,
    board_for_normal_pot_dimensions <- add_board(
      board_width,
      board_height,
      pot_radius,
      pot_diameter,
      pot_rectangle_width,
      pot_rectangle_height
    )
    board_for_flipped_pot_dimensions <- add_board(
      board_width,
      board_height,
      pot_radius,
      pot_diameter,
      pot_rectangle_height,
      pot_rectangle_width
    )
    pots_placed_total_1 <- pots_placed_total_2 <- 0
    board_for_normal_pot_dimensions <- add_pot_rect(
      board_for_normal_pot_dimensions,
      pot_rectangle_width,
      pot_rectangle_height,
      pots_placed_total_1,
      pots
    )
    board_for_flipped_pot_dimensions <- add_pot_rect(
      board_for_flipped_pot_dimensions,
      pot_rectangle_height,
      pot_rectangle_width,
      pots_placed_total_1,
      pots
    )
  }

  while (pots_placed_total < pots) { # Check the total number of pots placed across all boards
    if (pot_type %in% c("circle", "square")) {
      board <- add_board(
        board_width,
        board_height,
        pot_radius,
        pot_diameter,
        pot_rectangle_width,
        pot_rectangle_height
      )
      board <- add_pot(
        board,
        pot_radius,
        pot_diameter,
        pots_placed_total,
        pots
      )
    } else if (pot_type == "rectangle") {
      # determine which pot-orientation yields most number of pots per board.
      if ((board_for_normal_pot_dimensions$pot_count > board_for_flipped_pot_dimensions$pot_count) || isTRUE( rectangle_enforce_given_orientation)) {
        # board <- add_pot_rect(board, pot_rectangle_width, pot_rectangle_height, pots_placed_total, pots)
        # Maximisation works by
        # - flipping board_width<>board_height in add_board & keeping original
        # - does not work when I flip pot dimensions though - BUG: Why?
        board <- add_board(
          board_width = board_width,
          board_height = board_height,
          pot_radius = pot_radius,
          pot_diameter = pot_diameter,
          pot_rectangle_width = pot_rectangle_width,
          pot_rectangle_height = pot_rectangle_height
        )
        board <- add_pot_rect(
          board = board,
          pot_rectangle_width = pot_rectangle_width,
          pot_rectangle_height = pot_rectangle_height,
          total_pots_placed = pots_placed_total,
          total_pots_available = pots
        )
      } else {
        message(
          paste0(
            "i Flipping pot orientation to fit ",
            board_for_flipped_pot_dimensions$pot_count,
            " instead of ",
            board_for_normal_pot_dimensions$pot_count,
            " pots per board."
          )
        )
        board <- add_board(
          board_width = board_width,
          board_height = board_height,
          pot_radius = pot_radius,
          pot_diameter = pot_diameter,
          pot_rectangle_width = pot_rectangle_height,
          pot_rectangle_height = pot_rectangle_width
        )
        board <- add_pot_rect(
          board = board,
          pot_rectangle_width = pot_rectangle_height,
          pot_rectangle_height = pot_rectangle_width,
          total_pots_placed = pots_placed_total,
          total_pots_available = pots
        )
      }
      # 2. get the number of pots placed,
      # 3. swap pot dimensions _AND_ board_dimensions,
      # 4. compare to determine most lucrative orientation
      # 5. rerun the entire while-loop for this orientation
    } else {
      stop(simpleError("Invalid pot type. Must be one of 'circle', 'square', or 'rectangle'."))
    }
    pots_placed_total <- pots_placed_total + board$pot_count # Update total pots placed across all boards
    boards[[paste0("board_", length(boards) + 1)]] <- board
  }
  return(boards)
}
