#' Package boards into shelves based on a shelf_definition
#'
#' BETA.
#' Function can be used to transform a boards-object into a set
#' of ggplot-renders for several shelves, given the definitions
#' of said shelf.
#'
#' This function is a beta-test; issues might arise.
#' @param boards An existing board-object to place into shelves
#' @param shelf_definitions A list defining
#'
#' - `boards_per_level`: the number of boards per level
#'
#' - `boards_per_side`: the number of boards per side
#'
#' @importFrom stringr str_c
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr as_ggplot
#' @return shelves - list of layouts-objects, each containing a shelf-layout-plot.
#' @export
#'
#' @example man/examples/PTR_packShelves.R
PTR_packShelves <- function(boards="",shelf_definitions=list()) {
    # this step is not required, but necessary for this example.
    if (length(shelf_definitions)==0) {
        stop(simpleError(str_c("Shelves are not defined. Please define a list declaring 'boards_per_level', 'boards_per_side'.")))
    }
    # START ACTUAL FUNCTION
    shelf_layouts <- list()
    gg_objects <- lapply(boards, function(k) k$board_plot)
    IDs <- seq_along(boards)
    IDs <- keep_every_kth(IDs,shelf_definitions$boards_per_level)
    if (tail(IDs,1)!=length(boards)) {
        IDs <- c(IDs,length(boards))
    }
    start <- 1
    shelf_INDEX <- 1
    for (shelf in IDs) {
        gg_objects_ <- gg_objects[start:1:shelf]
        layout <- PTR_generateShelfLayout(gg_objects_, shelf_definitions)
        shelf_skeleton2 <-
        start <- start + IDs[[1]]
        shelf_layouts[[paste0("shelf_", shelf_INDEX)]] <- as_ggplot(
            arrangeGrob(
                grobs = gg_objects_,
                ncol = (shelf_definitions$boards_per_level/shelf_definitions$boards_per_side),
                nrow = shelf_definitions$boards_per_side,layout_matrix = layout
                )
            )
        shelf_INDEX <- shelf_INDEX + 1
    }
    return(shelf_layouts)
}
#' Title
#'
#' @param vector D
#' @param k D
#'
#' @return D
#' @keywords internal
#' @noRd
#'
#' @examples D
keep_every_kth <- function(vector, k) {
    # Subset the vector to keep only every k'th element
    result <- vector[seq(from = k, to = length(vector), by = k)]
    return(result)
}
#' Title
#'
#' @param gg_objects D
#' @param shelf_definitions D
#'
#' @return D
#' @keywords internal
#' @noRd
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#'
#' @examples D
PTR_generateShelfLayout <- function(gg_objects, shelf_definitions) {
    available <- c(1:1:length(gg_objects))
    if (length(available)<(shelf_definitions$boards_per_level/shelf_definitions$boards_per_side)*shelf_definitions$boards_per_side) {
        available <- c(1:1:((shelf_definitions$boards_per_level/shelf_definitions$boards_per_side)*shelf_definitions$boards_per_side))
    }
    if (str_count(shelf_definitions$labelling_order,"F2B")>0) {
        layout <- shifter(available,shelf_definitions$boards_per_level/shelf_definitions$boards_per_side)
        if (str_count(shelf_definitions$labelling_order,"L2R")>0) {
            ## do nothing
            layout2 <- rotate_elements(layout, (shelf_definitions$boards_per_level/shelf_definitions$boards_per_side))
            layout <- layout2
        } else {
            ## TODO: flip elements in direct pairs for even widths, and around the center pivot for uneven ones
            layout2 <- rotate_elements(layout, (shelf_definitions$boards_per_level/shelf_definitions$boards_per_side))
            layout <- layout2

        }
        layout[layout>length(gg_objects)] <- NA
        layout <- matrix(layout,ncol = 2,byrow = T)
    }
    return(layout)
}
#' Title
#'
#' @return D
#' @keywords internal
#' @noRd
#'
#' @examples D
private <- function() {

    #### CREATE SAMPLE SETUP ####
    N <- 8
    N2 <- 4
    labels <- c("UU", "UG", "ABAU", "ABAG")
    repeated_vector <- rep(labels, each = N)
    labels_calibration <- c("cUU","cUG","cABAU","cABAG")
    repeated_vector2 <- rep(labels_calibration,each = N2)
    # Create the indices vector
    indices <- rep(1:N, times = length(labels))
    indices2 <- rep(1:N2, times = length(labels_calibration))
    # Combine repeated_vector and indices using paste
    labels1 <- paste(repeated_vector, indices, sep = "_")
    labels2 <- paste(repeated_vector2, indices2, sep = "_")
    labels_ <- c(labels1,labels2)
    labels_ <- c(labels_,"E_1","E_2","E_3","E_4","E_5","E_6","E_7","E_8","E_9")
    boards <- PTR_generateBoardLayouts2(
        pots = 57, ## note that we increased the number of pots. In this example,
        board_width = 20,
        board_height = 40,
        pot_radius = 5,
        pot_diameter = 5 * 2,
        pot_rectangle_width = 5 * 2,
        pot_rectangle_height = 5 * 2,
        distance = 0,
        lbls = labels_,
        pot_type = "square"
    )
    # start of actual function-body
    shelf_definitions <- list(
        boards_per_level = 4,
        boards_per_side = 2,
        labelling_order = "L2R,F2B,D2T"
    )
    shelf_layouts <- PTR_packShelves(boards,shelf_definitions) # create shelves for old layout
    #### MOVE BOARDS TO NEXT SHELF ####
    # rotate boards 4 places (one entire board) || Remember that to move a board/pot to the next-higher-numbered position, you must use negative numbers
    boards_rotated <- PTR::PTR_rotateBoards(boards,-4)
    # as a result,
    # - boards$board_1 is now in position boards_rotated$board_5
    # - boards$board_2 is now in position boards_rotated$board_6
    #
    # if we now repackage, shelf2 should contain shelf1-boards
    shelf_layouts2 <- PTR_packShelves(boards_rotated,shelf_definitions) # repackage. boards of shelf1 should now be positioned on shelf2, 2>3, 3>1
    # as a result
    # - shelf_layouts2$shelf_1 contains shelf_layouts$shelf_3
    # - shelf_layouts2$shelf_2 contains shelf_layouts$shelf_1
    # - shelf_layouts2$shelf_3 contains shelf_layouts$shelf_2
    ## TODO: if we have unfilled shelfs (e.g. 7 boards and a 2x2 layout, so shelf2
    ## has a missing quadrant) that quadrant must be padded I think. because for
    ## this scenario, we have
    ##
    ## shelf_layouts$shelf_1 (board1-4)
    ## shelf_layouts$shelf_2 (board5-7)
    ##
    ## However, if we rotate the boards, then repack, we get for shelf_layouts2 (**positions indicate where the board was in layout1**)
    ## shelf_layouts2$shelf_1 (board4,board5,board6,board7)
    ## shelf_layouts2$shelf_2 (board1,board2,board3,blank)
    ##
    ## however, 'board4' should be part of 'shelf_layouts2$shelf2', not
    ## 'shelf_layouts2$shelf1', and instead shelf1 should contain an empty
    ## board-location.
    ## We cannot force this by creating a dummy-board (and it would be confusing),
    ## because if we fill a new board to place into the 4th position of shelf1, the
    ## labels will be filled into there as well. meaning we would have to create a
    ## **complete** dummy-board to prevent frame-shifting during PTR_rotatePots
    ##

    ##### MOVE POTS WITHIN A BOARD AFTER MOVING BOARDS TO NEXT SHELF ####
    # Now that we have moved the entire board to a new location,we also want to
    # rotate the pots within these "new" boards.
    shelf_layouts2_pots_rotated <- PTR_packShelves(SLPR_intermediate <- PTR::PTR_rotatePots(boards_rotated,1),shelf_definitions)
    # as a result
    # - shelf_layouts2_pots_rotated$shelf_1 contains
    # TODO: BUG:
    # there is a bug in the pot-rotation somewhere:
    # Pots are shifted across boards, instead of within them.
    #
    # take a closer look at
    # - ret$boards_rotated$board_1$input$lbls
    # - ret$SLPR_intermediate$board_1$input$lbls
    # the shift-factor for rotatePots is 1, so we would expect
    # to have groups of 8, each shifted down in index  by 1, with
    # position 1 going to position 8/-1. However, it seeems this is not the case
    #
    # labels are rotated out of place. The -4-shift to accommodate for board-
    # rotation is correct, but afterwards labels seem to get mangled. why?
    #
    # Can we start by verifying if 'boards_rotated' (output of
    # `PTR::PTR_rotateBoards(boards,-4)`)
    # is actually valid and what we want it to be? not
    #
    # Once this is verified, we can sit down and try and figure out if/why/how
    # PTR::PTR_packShelves() messes up the label ordering.
    #  Can we operate on a different mechanism? First rotate pots within the board,
    #  then rotate the boards and repack? It seems like rotateBoards _itself_
    #  works fine in isolation, and does not introduce frameshifts - even for
    #  unfilled boards
    if(!is.null(grDevices::dev.list())) grDevices::dev.off()
    ret <- list(
        boards = boards,
        boards_rotated = boards_rotated,
        shelf_layouts = shelf_layouts,
        shelf_layouts2 = shelf_layouts2,
        shelf_layouts2_pots_rotated = shelf_layouts2_pots_rotated,
        shelf_definitions = shelf_definitions,
        SLPR_intermediate = SLPR_intermediate
    )
    return(ret)

#print(shelf_layouts$shelf_1)
#print(shelf_layouts2$shelf_2)
#
#print(shelf_layouts$shelf_3)
#print(shelf_layouts2$shelf_1)
#print(shelf_layouts2_pots_rotated$shelf_1)
#print(shelf_layouts2_pots_rotated$shelf_2)

}
