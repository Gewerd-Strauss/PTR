pots <- 34
board_width <- 31 # in [unit] - width of a board-section
board_height <- 30 # in [unit] - height of a board-section
pot_radius <- 3.625 # in [unit] - radius of a pot/the bounding circle/platter
distance <- 0 # in [unit] - spacing between adjacent pots
boardLayout <- PTR_generateBoardLayouts(pots, board_width, board_height, pot_radius, distance)
