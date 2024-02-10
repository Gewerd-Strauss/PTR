#' Title
#'
#' @param board_width The width of a single board. A board is defined as one of the 2 single elements used
#' @param board_height The height of a single board. See 'board_width' for details
#' @param radius The radius of a pot
#' @param distance The spacing distance between two pots.
#'
#' @return n_circles - number of circles that could be packaged into a board
#' @return pos the (x,y)-coordinate pairs of the circles packaged into the board
#' @keywords internal
#' @noRd
#'
get_circles_in_rectangle <- function(board_width, board_height, radius, distance = 0) {
    p_x <- p_y <- c()
    diameter <- radius * 2
    n_circles <- 0

    if (((diameter + distance) <= board_width) && (diameter + distance) <= board_height) {
        pos_x <- radius # initialise the X/Y-position of the first circle's center
        pos_y <- radius

        while ((pos_y + radius) <= board_height) { # check - for circle-origin (pos_x,pos_y), is the circle projected within the bounding restrictions?
            while ((pos_x + radius) <= board_width) {
                n_circles <- n_circles + 1 # we successfully managed to put a circle at a corner of the box
                pos_x <- pos_x + (diameter + distance) # move the x-pointer over by a full circle (plus spacing). This positions the next circle to be adjacent to the previous circle perpendicular to the x-axis
                p_x <- c(p_x, pos_x - (diameter + distance))
                p_y <- c(p_y, pos_y)
            }
            pos_x <- (diameter / 2)
            pos_y <- pos_y + (diameter + distance) # move the y-pointer over by a full circle, see comment above
        }
    }
    pos <- list(p_x = p_x, p_y = p_y)
    return(list(n_circles = n_circles, pos = pos))
}
