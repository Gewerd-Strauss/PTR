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
getCirclesInRect <- function(board_width, board_height, radius, distance = 0) {
    # pos = c()
    pX <- pY <- c()
    diameter <- radius * 2
    n_circles <- 0

    if (((diameter + distance) <= board_width) && (diameter + distance) <= board_height) {
        posX <- radius # initialise the X/Y-position of the first circle's center
        posY <- radius

        while ((posY + radius) <= board_height) { # check - for circle-origin (posX,posY), is the circle projected within the bounding restrictions?
            while ((posX + radius) <= board_width) {
                n_circles <- n_circles + 1 # we successfully managed to put a circle at a corner of the box
                posX <- posX + (diameter + distance) # move the x-pointer over by a full circle (plus spacing). This positions the next circle to be adjacent to the previous circle perpendicular to the x-axis
                # print(stringr::str_c("pot ",n_circles," (rad: ",radius,",spacing: ",distance,"): X/Y:(",posX-(diameter + distance),",",posY,"),X0:",posX - (diameter + distance+radius)," X1:",(posX-(diameter + distance) + radius + distance),",Y0:",posY-radius ," Y1:",posY+radius))
                pX <- c(pX, posX - (diameter + distance))
                pY <- c(pY, posY)
            }
            posX <- (diameter / 2)
            posY <- posY + (diameter + distance) # move the y-pointer over by a full circle, see comment above
        }
    }
    pos <- list(pX = pX, pY = pY)
    # colnames(pos)  <- c("pX","pY")
    # rownames(pos)  <- c(1:1:length(pX))
    return(list(n_circles = n_circles, pos = pos))
}
