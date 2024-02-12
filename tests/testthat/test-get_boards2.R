test_that("getBoards2: fitting-equivalence for rectangular pots via rotation", {
    ## ENFORCE_ORIENTATION = FALSE: Ensure that given equal input, both boards
    ## get the same number of pots by flipping pot dimensions on one of them.
    board30x60_rectpot12x10 <- get_boards2(30, 60, NA, NA, 12, 10, 17, "rectangle")
    board30x60_rectpot10x12 <- get_boards2(30, 60, NA, NA, 10, 12, 17, "rectangle")
    expect_true(length(board30x60_rectpot10x12$board_1$pot_centers) == length(board30x60_rectpot12x10$board_1$pot_centers))
})
test_that("getBoards2: forced-disabled orientation for rectangular pots via non-rotation", {
    board30x60_rectpot12x10 <- get_boards2(30, 60, NA, NA, 12, 10, 17, "rectangle",TRUE)
    board30x60_rectpot10x12 <- get_boards2(30, 60, NA, NA, 10, 12, 17, "rectangle",TRUE)
    expect_true(length(board30x60_rectpot10x12$board_1$pot_centers) > length(board30x60_rectpot12x10$board_1$pot_centers))
})
