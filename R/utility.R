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
#' @examples num_new <- padToLength(1,3)
padToLength <- function(number, length_to_pad_to) {
    if (isFALSE(is.integer(as.integer(number)))) {
        sW <- simpleWarning(str_c("<number> is not of type <integer>. This might be okay, but be aware"))
        warning(sW)
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
warnIterations <- function(it) {
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
collectLabels <- function(boards) {
    ret <- c()
    for (board in boards) {
        ret <- append(ret,board$input$lbls)
    }
    return(ret)
}
