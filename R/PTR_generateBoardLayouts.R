#' Title
#'
#' @param pots The number of pots you must place
#' @param board_width The width of a single board.
#' A board is defined as one of the 2 single elements used
#' @param board_height The height of a single board.
#' See 'board_width' for details
#' @param radius The radius of a pot
#' @param distance The spacing distance between two pots.
#' @param lbls vector of length 'pots' consisting of custom names for each pot.
#' Will be assigned in from bottom-left to top-right
#'
#' @return layouts - list of layout_objects, each containing a board-plot.
#' Additionally, the input data and a mapping of coordinates to pot-names
#' plotted is returned
#' @examples
#' \dontrun{
#' pots <- 34
#' board_width <- 31 # in [unit] - width of a board-section
#' board_height <- 30 # in [unit] - height of a board-section
#' pot_radius <- 3.625 # in [unit] - radius of a pot/the bounding circle/platter
#' distance <- 0 # in [unit] - spacing between adjacent pots
#' boardLayout <- PTR_generateBoardLayouts(pots, board_width, board_height, pot_radius, distance)
#' }
#' @export
#' @importFrom stringr str_c
#' @importFrom stringr str_length
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 annotate
#' @importFrom ggforce geom_circle
PTR_generateBoardLayouts <- function(pots = NA, board_width = NA, board_height = NA, radius = NA, distance = NA, lbls = FALSE) {
    if (is.na(pots)) {
        stop(simpleError(str_c("generateBoardLayouts: Required Argument 'pots' was not supplied. Must be an integer")))
    }
    if (is.na(board_width)) {
        stop(simpleError(str_c("generateBoardLayouts: Required Argument 'board_width' was not supplied. Must be a numeric")))
    }
    if (is.na(board_height)) {
        stop(simpleError(str_c("generateBoardLayouts: Required Argument 'board_height' was not supplied. Must be a numeric")))
    }
    if (is.na(radius)) {
        stop(simpleError(str_c("generateBoardLayouts: Required Argument 'radius' was not supplied. Must be a numeric")))
    }
    if (is.na(distance)) {
        stop(simpleError(str_c("generateBoardLayouts: Required Argument 'distance' was not supplied. Must be a numeric")))
    }
    if (isFALSE(isFALSE(lbls))) {
        if (isFALSE(is.vector(lbls))) {
            if (isFALSE(length(lbls)==pots))
            stop(simpleError(str_c("generateBoardLayouts: Optional Argument 'lbls' must be either ignored, FALSE, or a vector of length 'pots' declaring one label per plot.")))
        }
    }
    inputs <- list(pots = pots, board_width = board_width, board_height = board_height, radius = radius, distance = distance, lbls = lbls)
    ret <- getBoards(board_width, board_height, radius, distance, pots)
    potsInTray <- ret$potsInTray
    boardsRequired <- ret$boardsRequired
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
    potsPlottable <- potsInTray$n_circles
    labelStart <- end_index <- plotted_pots <- 0
    start_index <- 1
    # TODO:  how to determine that on runs 2+
    # , maybe not the full number of pots must be plotted?
    for (board_INDEX in 1:1:boardsRequired) {
        RectangleX <- c(0, board_width)
        RectangleY <- c(0, board_height)
        X <- potsInTray$pos$pX[1:potsPlottable]
        Y <- potsInTray$pos$pY[1:potsPlottable]
        X <- stats::na.omit(X) # clean out out-of-bounds elements
        Y <- stats::na.omit(Y)
        if (isFALSE(as.logical(lbls))) {
            # generate automatic labels if labels were not supplied
            Labels <- str_c("pot_", padToLength(labelStart + 1:(potsInTray$n_circles),str_length(pots)))
            inputs$fed_labels <- inputs$lbls
            inputs$lbls <- Labels
        } else {
            K <- potsInTray$n_circles
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
            Labels <- V[start_index:end_index]
            inputs$fed_labels <- inputs$lbls
            inputs$lbls <- Labels
            # lbls <- lbls[-c(1:end_index)]
            #            Labels <- Labels[1:potsInTray$n_circles]
        }
        # guard against length inequalities arising from
        # generalised population of vectors in previous step
        if (length(Labels) > (pots - plotted_pots)) {
            Labels <- Labels[1:(pots - plotted_pots)]
        }
        if (length(X) > (pots - plotted_pots)) {
            X <- X[1:(pots - plotted_pots)]
        }
        if (length(Y) > (pots - plotted_pots)) {
            Y <- Y[1:(pots - plotted_pots)]
        }
        # print(str_c(board_INDEX, "\n", Labels))

        circle <- function(x, y, radius) {
            data.frame(
                x = x,
                y = y,
                r = radius
            )
        }

        df_points <- data.frame(X,
            Y,
            Label_ = Labels[1:1:length(X)]
        )
        limitsY <- c(-1, board_height)
        limitsX <- c(-1, board_width)
        labelsX <- ""
        labelsY <- ""
        board_plot <- ggplot() +
            geom_point(data = df_points, aes(x = X, y = Y), color = "black") +
            geom_text(data = df_points, aes(x = X, y = Y, label = Label_), vjust = -0.5) + # add labels
            geom_rect(aes(xmin = RectangleX[1], xmax = RectangleX[2], ymin = RectangleY[1], ymax = RectangleY[2]), alpha = 0, color = "black") +
            geom_circle(data = circle(X, Y, radius), aes(x0 = x, y0 = y, r = r), color = "blue", fill = NA) +
            scale_x_continuous(name = "[units]", n.breaks = 2 * length(unique(X)) + 1, limits = limitsX) +
            scale_y_continuous(name = "[units]", n.breaks = 2 * length(unique(Y)) + 1, limits = limitsY) +
            ggtitle(label = "", subtitle = str_c("board {", board_INDEX, "}")) +
            theme_minimal()
        # b <- ggplot_build(board_plot)
        # map_ <- b$data[[2]]
        rownames(df_points) <- df_points$Label_
        df_points <- subset(df_points,select = c("Label_"))
        # map_ <- cbind(x = map_$x, map_$y)
        # colnames(map_) <- c("x", "y")
        # rownames(map_) <- b$data[[2]]$label
        # a <- rbind(map_$x, map_$y)
        # layouts[[str_c("board_", padToLength(board_INDEX, str_length(boardsRequired)))]] <- list(board_plot = board_plot, points = as.data.frame(map_), input = inputs) ## we package the mapping so we have easier access to it later on if we want to randomise shuffling in a controlled manner.
        layouts[[str_c("board_", padToLength(board_INDEX, str_length(boardsRequired)))]] <- list(board_plot = board_plot, points = as.data.frame(df_points), input = inputs) ## we package the mapping so we have easier access to it later on if we want to randomise shuffling in a controlled manner.
        if (isFALSE(as.logical(lbls))) {
            labelStart <- labelStart + potsInTray$n_circles
            plotted_pots <- plotted_pots + potsInTray$n_circles
            # potsInTray$n_circles <- pots - potsInTray$n_circles
        } else {
            plotted_pots <- plotted_pots + potsInTray$n_circles
        }
    }
    return(layouts)
}
# suppress warnings
utils::globalVariables(c("prob", "section", "y", "Label_", "r", "x"))
