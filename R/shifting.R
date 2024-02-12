#' Title
#'
#' @param x - vector to shift on
#' @param n - steps to shift. Positive integers shift left-to-right, while positive integers shift right-to-left
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stringr str_c
#' @return a shifted vector
#' @keywords internal
#' @noRd
#'
#' @example shifter(c(1,2,3,4,5),-2)
shifter <- function(x, n = 1) {
  if (!is.vector(x)) {
    stop(simpleError(str_c("'x' must be a vector")))
  }
  if (length(x)<n) { # only rotate the steps we have space for in the vector.
      n <- n - length(x)
  }
  if (!(n %% 1 == 0)) {
    stop(simpleError(str_c("'n' must be an integer")))
  } else if (n == 0) {
    return(x)
  } else {
    a <- c(tail(x, -n), head(x, n))
    return(a)
  }
}
#' rotate Boards forward or backwards through the list.
#'
#' To shift the pots themselves, consider using `PTR_rotatePots`
#' @param boards boards to shift around
#' @param shifts steps to shift. Positive integers shift a board to the next-up
#' position (last board becomes the first, first board becomes 2nd), while
#' negative integers move them backwards (e.g. the 2nd board becomes the first
#' board and the first board moves to the last position.
#'
#' @importFrom stringr str_c
#' @return a new set of boards containing shifted pots
#' @export
#'
#' @examples
#' \dontrun{
#' ob1 <- PTR_load_dummy_data(3) # load example dummy data for the `1`-functions
#' nb11 <- PTR_rotateBoards(ob2, -1) # board 1 goes to last position, board 2
#' # goes to first position
#' nb12 <- PTR_rotateBoards(ob2, +1) # board 1 goes to 2nd position, last board
#' # goes to first position
#' }
PTR_rotateBoards <- function(boards, shifts = -1) {
  ## 1. get Boards
  ## 2. duplicate to one variable dupl_
  ## 3. extract the pots from boards in dupl_
  ## 4. for each set, extract the labels
  ## 5. then resort the label-packs by adding integer <shifts>
  ## 5.1. must wrap around at the back
  ## 6. once labels are resorted, feed them to generateBoardLayout
  ## 7. generateBoardLayout shoudl then generate a new order
  ## 8. return a modified object that notes how many shifts were calculated
  if (abs(shifts) == length(boards)) {
    swarning <- simpleWarning(
      str_c(
        "rotateBoards(): ",
        "Your shift will be perfectly circular, putting every board in its previous place."
      )
    )
    warning(swarning)
  } else if (abs(shifts) > length(boards)) {
    swarning <- simpleWarning(
      str_c(
        "rotateBoards(): ",
        "You are trying to shift each board by more than a full rotation (",
        length(boards), " boards total). Consider shifting by only (",
        shifts - length(boards), ") places to obtain the same result in the future."
      )
    )
    shifts <- shifts - length(boards)
    warning(swarning)
  }
  curr_names <- names(boards)
  new_names <- shifter(curr_names, shifts)
  ret <- list()
  i <- 0
  for (name in new_names) {
    i <- i + 1
    object <- boards[[name]] ## get future board
    name_ <- curr_names[[i]] ## get key to current position
    object$board_plot$labels$title <- str_c(name_)
    ret[[name_]] <- object ## and save the object to its current name.
  }
  return(ret)
}

#' rotate Boards forward or backwards through the list.
#'
#' To shift the pots themselves, consider using `PTR_rotatePots2`
#' @param boards boards to shift around
#' @param shifts steps to shift. Positive integers shift a board to the next-up
#' position (last board becomes the first, first board becomes 2nd), while
#' negative integers move them backwards (e.g. the 2nd board becomes the first
#' board and the first board moves to the last position.
#'
#' @importFrom stringr str_c
#' @return a new set of boards containing shifted pots
#' @export
#'
#' @examples
#' \dontrun{
#' ob2 <- PTR_load_dummy_data(5) # load example dummy data for the `2`-functions
#' nb21 <- PTR_rotateBoards2(ob2, -1) # board 1 goes to last position, board 2
#' # goes to first position
#' nb22 <- PTR_rotateBoards2(ob2, +1) # board 1 goes to 2nd position, last board
#' # goes to first position
#' }
PTR_rotateBoards2 <- function(boards, shifts = -1) {
  if (abs(shifts) == length(boards)) {
    swarning <- simpleWarning(
      str_c(
        "rotateBoards(): ",
        "Your shift will be perfectly circular, putting every board in its previous place."
      )
    )
    warning(swarning)
  } else if (abs(shifts) > length(boards)) {
    swarning <- simpleWarning(
      str_c(
        "rotateBoards2(): ",
        "You are trying to shift each board by more than a full rotation (",
        length(boards), " boards total). Consider shifting by only (",
        shifts - length(boards),
        ") places to obtain the same result in the future."
      )
    )
    shifts <- shifts - length(boards)
    warning(swarning)
  }
  curr_names <- names(boards)
  new_names <- shifter(curr_names, shifts)
  ret <- list()
  i <- 0
  for (name in new_names) {
    i <- i + 1
    object <- boards[[name]] ## get future board
    name_ <- curr_names[[i]] ## get key to current position
    object$board_plot$labels$title <- str_c(name_)
    ret[[name_]] <- object ## and save the object to its current name.
  }
  return(ret)
}
#' Title
#'
#' To shift the boards themselves, consider using `PTR_rotateBoards`
#' @param boards boards in which pots are to be shifted within
#' @param shifts steps to shift. Positive integers shift  clockwise,
#' while negative integers shift left-to-right,bottom-to-top
#'
#' @return boards with shifted pots. The boards themselves remain at their location.
#' @export
#'
#' @examples
#' \dontrun{
#' consecutive_groups <- PTR_load_dummy_data(4) # load example dummy data for the `1`-functions
#' newBoards <- PTR_rotatePots(oldBoards, -2)
#' }
PTR_rotatePots <- function(boards, shifts = 2) {
  # Function will rotate pots within each boards
  # This is done by iterating over the currently assigned labels embedded
  # into a boards-object, and rotating the elements of its `lbls` by `shifts`
  ret <- list()
  # Print the sorted vector
  new_labels <- c()
  for (board in boards) {
    # lbls_rotated <- shifter(board$input$lbls[1:max(seq_len(dim(board$points)[[1]]))], shifts)
    lbls_rotated <- shifter(board$points$Label_, shifts)
    if (length(board$points$Label_) > length(lbls_rotated)) {
      ## we rotated a label-set of a board that was not fully filled.
      ## Insert a dummy-pot so that the next set of labels doesn't frame-
      ## shift to the left by `diff`
      lbls_rotated <- append(
        lbls_rotated,
        "PTR_DUMMY",
        length(board$input$lbls) - length(lbls_rotated)
      )
      lbls_rotated <- c(
        lbls_rotated,
        rep("PTR_DUMMY", length(board$input$lbls) - length(lbls_rotated))
      )
    }
    new_labels <- append(new_labels, lbls_rotated)
  }
  input <- boards$board_1$input
  input$lbls <- new_labels
  #input$lbls <- new_labels
  if (boards$board_1$version == 1) { ## regenerate with the appropriate function
      ret <- PTR_generateBoardLayouts(
        pots = input$pots,
        board_width = input$board_width,
        board_height = input$board_height,
        pot_radius = input$pot_radius,
        distance = input$distance,
        lbls = input$lbls
      )
  } else {
      ret <- PTR_generateBoardLayouts2(
          pots = input$pots,
          board_width = input$board_width,
          board_height = input$board_height,
          pot_radius = input$pot_radius,
          pot_diameter = input$pot_diameter,
          pot_rectangle_width = input$pot_rectangle_width,
          pot_rectangle_height = input$pot_rectangle_height,
          distance = input$distance,
          pot_type = input$pot_type,
          lbls = input$lbls,
          rectangle_enforce_given_orientation = input$rectangle_enforce_given_orientation
      )
  }

  return(ret)
}
#' Sort Labels of a board-object based on replicate-ID/replicate-**number**
#'
#'
#' Use this function after generating a board to create an ordered layout where
#' members of groups are equally distributed across all available boards.
#'
#' NOTE:
#' This requires that the number is separable from the group-name
#'  by a single `_`.
#'
#' @param boards boards in which pots are to be shifted within
#' @param shifts steps to shift. Positive integers shift  clockwise,
#' while negative integers shift left-to-right,bottom-to-top
#'
#' @importFrom utils hasName
#' @return boards with shifted pots. The boards themselves remain at their location.
#' @export
#'
#' @examples
#' \dontrun{
#' boards_with_consecutive_groups <- PTR_load_dummy_data(4) # load example dummy data
#' # with custom group-names `c("UU","UG","ABAU","ABAG")`, each repeated 8
#' # times (`UU1,UU2,UU3,...UG1,UG2,...ABAU1,ABAU2,...ABAG1,...ABAG8`)
#' newBoards <- PTR_rotatePots(oldBoards, -2)
#' }
PTR_sortPots_by_potindex <- function(boards, shifts = -1) {
  # Function will rotate pots within each boards
  # TODO: the current function contents does not do this. It does something, but I forgot what purpose _this_ code actually has.
  ret <- list()

  # Function to extract group indices
  get_group_index <- function(label) {
    l <- strsplit(label, "_")
    r <- unlist(l)
    if (length(r) == 2) {
      f <- r[2]
    } else if (length(r) == 1) {
      f <- r
    }
    return(f)
  }
  old_labels <- collect_labels(boards)
  # Extract group indices from old_labels
  group_indices <- sapply(old_labels, get_group_index)

  # Create a sorting index to sort by group indices
  sorting_index <- order(group_indices, old_labels)

  # Sort old_labels using the sorting index
  new_labels <- old_labels[sorting_index]
  ## 1:
  ## old_labels == unique(old_labels) == new_labels?
  # Print the sorted vector
  warning(
    simpleWarning(
      str_c(
        "Note: This function will also sort elements which share the",
        " same pot-index (e.g. ",
        "'UU_1/UG_1/ABAU_1/ABAG_1/UU_2/UG_2/...' alphabetically ",
        "within each set of indices.\nFor the example above, this ",
        "will return 'ABAG_1/ABAU_1/UG_1/UU_1/UG_2/UU_2/...'."
      )
    )
  )
  input <- boards$board_1$input
  input$lbls <- unique(new_labels)
  if (hasName(boards$board_1$input, "pot_type")) {
    ret <- PTR_generateBoardLayouts2(
      pots = input$pots,
      board_width = input$board_width,
      board_height = input$board_height,
      pot_radius = input$pot_radius,
      pot_diameter = input$pot_diameter,
      pot_rectangle_width = input$pot_rectangle_width,
      pot_rectangle_height = input$pot_rectangle_height,
      distance = input$distance,
      lbls = input$lbls,
      pot_type = input$pot_type
    )
  } else {
    ret <- PTR_generateBoardLayouts(
      pots = input$pots,
      board_width = input$board_width,
      board_height = input$board_height,
      pot_radius = input$pot_radius,
      distance = input$distance,
      lbls = input$lbls
    )
  }
  return(ret)
}
