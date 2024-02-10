#' pads an integer to length by prepending zeros
#'
#' @param number - an integer to pad
#' @param length_to_pad_to
#'
#' @return a padded number
#' @noRd
#' @keywords internal
#' @importFrom stringr str_c
#'
#' @examples num_new <- pad_to_length(1,3)
pad_to_length <- function(number, length_to_pad_to) {
    if (isFALSE(is.integer(as.integer(number)))) {
        swarning <- simpleWarning(str_c("<number> is not of type <integer>. This might be okay, but be aware"))
        warning(swarning)
    }
    cond <- str_length(number) < length_to_pad_to
        number[cond] <- str_c("0", number[cond])
    return(number)
}
#'
#'
#' @importFrom utils askYesNo
#' @noRd
#' @keywords internal
#' @importFrom stringr str_c
#'
warn_iterations <- function(it) {
    askYesNo(str_c("The number of required iterations is above ", it, ". This might take some time, so you might want to consider different restraints. Do you want to continue with current inputs?"))
}
#' Title
#'
#' @param boards
#'
#' @return all labels in consecutive order.
#' @export
#' @keywords internal
#'
collect_labels <- function(boards) {
    ret <- c()
    for (board in boards) {
        ret <- append(ret, board$input$lbls)
    }
    return(ret)
}
