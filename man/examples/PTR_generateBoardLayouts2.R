# Example usage for rectangular pots
layouts_rect <- PTR_generateBoardLayouts2(
pots = 17,
board_width = 30,
board_height = 60,
pot_rectangle_width = 12,
pot_rectangle_height = 10,
distance = 0,
lbls = paste0("rectangle_", 1:17),
pot_type = "rectangle"
)
layouts_rect
# Example usage for circular pots
layouts_circle <- PTR_generateBoardLayouts2(
pots = 64,
board_width = 30,
board_height = 60,
pot_radius = 5,
distance = 0,
lbls = paste0("circle_", 1:64),
pot_type = "circle"
)
layouts_circle
# Example usage for square pots
layouts_square <- PTR_generateBoardLayouts2(
pots = 64,
board_width = 30,
board_height = 60,
pot_radius = 5,
pot_diameter = 5 * 2,
pot_rectangle_width = 5 * 2,
pot_rectangle_height = 5 * 2,
distance = 0,
lbls = paste0("square_", 1:64),
pot_type = "square"
)
layouts_square
