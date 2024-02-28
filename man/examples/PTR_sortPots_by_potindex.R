#EXAMPLES
boards_with_consecutive_groups <- PTR::PTR_load_dummy_data(4) # load example dummy data
#' # with custom group-names `c("UU","UG","ABAU","ABAG")`, each repeated 8
#' # times (`UU1,UU2,UU3,...UG1,UG2,...ABAU1,ABAU2,...ABAG1,...ABAG8`)
newBoards <- PTR::PTR_rotatePots(boards_with_consecutive_groups, -2)
