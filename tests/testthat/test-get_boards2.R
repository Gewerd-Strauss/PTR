test_that("testing get_Boards2", {
    board30x60_pot12x10 <- get_boards2(30, 60, NA, NA, 12, 10, 17, "rectangle")
    board30x60_pot10x12 <- get_boards2(30, 60, NA, NA, 10, 11, 17, "rectangle")
    expect_true(length(board30x60_pot10x12$board_1$pot_centers)>length(board30x60_pot12x10$board_1$pot_centers))
    # {
    # return(PTR_generateBoardLayouts2(
    #     pots = 17,
    #     board_width = 30,
    #     board_height = 60,
    #     pot_rectangle_width = 12,
    #     pot_rectangle_height = 10,
    #     distance = 0,
    #     lbls = paste0("rectangle12x10_", 1:17),
    #     pot_type = "rectangle"
    # ))
    # return(PTR_generateBoardLayouts2(
    #     pots = 17,
    #     board_width = 30,
    #     board_height = 60,
    #     pot_rectangle_width = 10,
    #     pot_rectangle_height = 12,
    #     distance = 0,
    #     lbls = paste0("rectangle10x12_", 1:17),
    #     pot_type = "rectangle"
    # ))

})
