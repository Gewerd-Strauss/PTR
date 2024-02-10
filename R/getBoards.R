#' Title
#'
#' @param board_width The width of a single board.
#' A board is defined as one of the 2 single elements used
#' @param board_height The height of a single board.
#' See 'board_width' for details
#' @param pot_radius The radius of a pot
#' @param distance The spacing distance between two pots.
#' @param pots The number of pots you must place
#'
#' @return 'boards_required' denotes the number of boards required to
#' accommodate all pots.
#' @return 'pots_in_tray' denotes the number of pots that could be fitted into a
#' single board.
#' @keywords internal
#' @noRd
#'
#' @importFrom stringr str_c
#'
get_boards <- function(board_width, board_height, pot_radius, distance, pots) {
    boards_required <- pots_handled <- 0 ## record the number of pots we can fit per iteration
    if ((pot_radius * 2) > board_height) { # a bit of validation
        stop(simpleError(str_c("rectangle height '", board_height, "' does not facilitate a circle of diameter '", pot_radius * 2, "'.")))
    }
    if ((pot_radius * 2) > board_width) {
        stop(simpleError(str_c("rectangle width '", board_width, "' does not facilitate a circle of diameter '", pot_radius * 2, "'.")))
    }
    while (pots_handled < pots) { ## repeat process until we have all pots placed
        previous_pots_handled <- pots_handled ## collect the previously placed pots
        pots_in_tray <- get_circles_in_rectangle(board_width, board_height, pot_radius, distance) ## calculate
        pots_handled <- pots_handled + pots_in_tray$n_circles
        boards_required <- boards_required + 1
        if ((previous_pots_handled == pots_handled) && (pots_handled <- 0)) {
            simpleError(str_c("rectangle dimensions'", board_height, "x", board_width, "' does not facilitate a circle.\nBoth Dimensions must be"))
        }
        # TODO: implement warn_iterations: after first iteration, get the change received, then extrapolate the number of steps required. If > 24? (3 whole shelves), warn for high iteration count. Do a y/n on continuing.
    }
    if (pots_handled < pots) {
        boards_required <- boards_required + 1
    }
    return(list(pots_in_tray = pots_in_tray, boards_required = boards_required))
}
