library(ggforce) # Load ggforce library if not already loaded

getBoards2 <- function(board_width, board_height, pot_radius, pot_diameter, rect_width, rect_height, pots) {
    boards <- list()
    pots_placed_total <- 0  # Initialize total pots placed across all boards
    total_pots_available <- pots

    addBoard <- function(board_width, board_height, pot_radius, pot_diameter, rect_width, rect_height) {
        board <- list(
            width = board_width,
            height = board_height,
            pot_radius = pot_radius,
            pot_diameter = pot_diameter,
            rect_width = rect_width,
            rect_height = rect_height,
            pot_count = 0,
            pot_centers = list()
        )
        return(board)
    }

    addPot <- function(board, pot_radius, pot_diameter, rect_width, rect_height, total_pots_placed, total_pots_available) {
        remaining_pots <- total_pots_available - total_pots_placed
        pots_to_add <- min(length(board$pot_centers) + remaining_pots, total_pots_available)

        while (length(board$pot_centers) < pots_to_add) {
            if (board$pot_count == 0) {
                board$pot_count <- 1
                board$pot_centers[[1]] <- list(x = pot_radius, y = pot_radius, radius = pot_radius, diameter = pot_diameter, rect_width = rect_width, rect_height = rect_height)
            } else {
                x <- board$pot_centers[[board$pot_count]]$x
                y <- board$pot_centers[[board$pot_count]]$y

                if (x + 2 * pot_radius <= board$width && y + 2 * pot_radius <= board$height) {
                    board$pot_count <- board$pot_count + 1
                    board$pot_centers[[board$pot_count]] <- list(x = x + 2 * pot_radius, y = y, radius = pot_radius, diameter = pot_diameter, rect_width = rect_width, rect_height = rect_height)
                } else {
                    x <- pot_radius
                    y <- y + 2 * pot_radius
                    if (y + 2 * pot_radius <= board$height) {
                        board$pot_count <- board$pot_count + 1
                        board$pot_centers[[board$pot_count]] <- list(x = x, y = y, radius = pot_radius, diameter = pot_diameter, rect_width = rect_width, rect_height = rect_height)
                    } else {
                        print("Reached maximum height of the board")
                        break
                    }
                }
            }
        }
        return(board)
    }

    while (pots_placed_total < pots) {  # Check the total number of pots placed across all boards
        board <- addBoard(board_width, board_height, pot_radius, pot_diameter, rect_width, rect_height)
        board <- addPot(board, pot_radius, pot_diameter, rect_width, rect_height, pots_placed_total, pots)
        pots_placed_total <- pots_placed_total + board$pot_count  # Update total pots placed across all boards
        boards[[paste0("board_", length(boards) + 1)]] <- board
    }

    return(boards)
}


generateBoardLayouts2 <- function(pots = NA, board_width = NA, board_height = NA, pot_radius = NA, pot_diameter = NA, rect_width = NA, rect_height = NA, distance = NA, lbls = FALSE) {
    if (is.na(pots)) {
        stop(simpleError("generateBoardLayouts2: Required Argument 'pots' was not supplied. Must be an integer"))
    }
    if (is.na(board_width)) {
        stop(simpleError("generateBoardLayouts2: Required Argument 'board_width' was not supplied. Must be a numeric"))
    }
    if (is.na(board_height)) {
        stop(simpleError("generateBoardLayouts2: Required Argument 'board_height' was not supplied. Must be a numeric"))
    }
    if (!isFALSE(isFALSE(lbls))) {
        if (!isFALSE(is.vector(lbls))) {
            if (!isFALSE(length(lbls)==pots)) {
                stop(simpleError("generateBoardLayouts2: Optional Argument 'lbls' must be either ignored, FALSE, or a vector of length 'pots' declaring one label per plot."))
            }
        }
    }

    inputs <- list(pots = pots, board_width = board_width, board_height = board_height, pot_radius = pot_radius, pot_diameter = pot_diameter, rect_width = rect_width, rect_height = rect_height, distance = distance, lbls = lbls)

    if (!is.na(pot_radius) && is.na(pot_diameter)) {
        # Circular pots
        ret <- getBoards2(board_width, board_height, pot_radius, pot_radius * 2, NA, NA, pots)
    } else if (is.na(pot_radius) && !is.na(rect_width) && !is.na(rect_height)) {
        # Rectangular pots
        ret <- getBoards2(board_width, board_height, NA, NA, rect_width, rect_height, pots)
    } else if (!is.na(pot_radius) && !is.na(pot_diameter)) {
        # Square pots
        ret <- getBoards2(board_width, board_height, pot_radius, pot_diameter, pot_radius, pot_diameter, pots)
    } else {
        stop(simpleError("Invalid combination of arguments. Please provide the correct combination of arguments."))
    }

    layouts <- list() ## init holding var
    pot_counter <- 0  # Initialize pot counter

    for (board_INDEX in seq_along(ret)) {
        board <- ret[[board_INDEX]]
        pot_centers <- board$pot_centers

        if (length(pot_centers) == 0) {
            next  # Skip this iteration if pot_centers is empty
        }

        ## Prepare data frame for ggplot
        #df_points <- data.frame(
        #    x = sapply(pot_centers, function(pot) pot$x),
        #    y = sapply(pot_centers, function(pot) pot$y),
        #    label = paste0("pot_", pot_counter + seq_along(pot_centers))
        #)
        # Prepare data frame for ggplot
        if (!isFALSE(is.vector(lbls)) && length(lbls) == pots) {
            df_points <- data.frame(
                x = sapply(pot_centers, function(pot) pot$x),
                y = sapply(pot_centers, function(pot) pot$y),
                #label = lbls[(board_INDEX - 1) * length(pot_centers) + seq_along(pot_centers)]
                label = lbls[pot_counter + seq_along(pot_centers)]
            )
        } else {
            df_points <- data.frame(
                x = sapply(pot_centers, function(pot) pot$x),
                y = sapply(pot_centers, function(pot) pot$y),
                label = paste0("pot_", pot_counter + seq_along(pot_centers))
            )
        }

        # Increment the pot counter for each pot on the board
        pot_counter <- pot_counter + length(pot_centers)

        # Create common ggplot object
        board_plot <- ggplot(df_points, aes(x = x, y = y)) +
            geom_text(aes(label = label), vjust = -0.5) +
            ggtitle(paste("Board", board_INDEX)) +
            xlim(0, board$width) +
            ylim(0, board$height) +
            xlab("[units]") +
            ylab("[units]") +
            theme_minimal()

        # Add appropriate geom based on pot type
        if (!is.na(pot_radius) && is.na(pot_diameter)) {
            # Circular pots
            board_plot <- board_plot + ggforce::geom_circle(aes(x0 = x, y0 = y, r = pot_radius), color = "black", fill = NA)
        } else {
            # Rectangular pots or Square pots
            board_plot <- board_plot + geom_rect(aes(xmin = x - pot_radius, xmax = x + pot_radius, ymin = y - pot_radius, ymax = y + pot_radius), color = "black", fill = NA)
        }

        layouts[[paste0("board_", board_INDEX)]] <- list(board_plot = board_plot, points = df_points, input = inputs)
    }

    return(layouts)
}



# Example usage for circular pots
board_width <- 30
board_height <- 60
pots <- 64
pot_radius <- 5
distance <- 0

layouts_circle <- generateBoardLayouts2(
    pots = pots,
    board_width = board_width,
    board_height = board_height,
    pot_radius = pot_radius,
    distance = distance,
    lbls = paste0("test_", 1:64)

)

layouts_circle

## Example usage for square pots
board_width <- 30
board_height <- 60
pots <- 64
pot_radius <- 5
distance <- 0

layouts_square <- generateBoardLayouts2(
   pots = pots,
   board_width = board_width,
   board_height = board_height,
   #pot_radius = pot_radius,
   rect_width = pot_radius * 2,
   rect_height = pot_radius * 2,
   distance = distance
)

layouts_square


# Example usage for rectangular pots
board_width <- 30
board_height <- 60
pots <- 64
rect_width <- 10
rect_height <- 15
distance <- 0

# layouts_rect <- generateBoardLayouts2(
#     pots = pots,
#     board_width = board_width,
#     board_height = board_height,
#     rect_width = rect_width,
#     rect_height = rect_height,
#     distance = distance
# )
#
