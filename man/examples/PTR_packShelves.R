# Generate sample Data
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
# define the shelf-dimentions
shelf_definitions <- list(
    boards_per_level = 4,
    boards_per_side = 2,
    labelling_order = "L2R,F2B,D2T"
)
# generate the layouts.
shelf_layouts <- PTR_packShelves(boards,shelf_definitions) # create shelves for old layout
