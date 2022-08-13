# add_lipid_class----------------------------------------------------------------
#' Add custom lipid class to \code{lipid_classes}
#'
#' @description The \code{add_lipid_class()} function is used to extend the
#' \code{lipid_classes} vector. This is necessary, if custom lipid classes
#' needs to be evaluated and are not in the \code{lipid_classes} vector.
#'
#' @param lipid_class string. Lipid class abbreviation
#'
#' @return \code{lipid_classes} vector + lipid_class-argument
#' @export
#'
#' @examples
#' lipid_classes <- add_lipid_class("SGalCer")
add_lipid_class <- function(lipid_class){

  c(lipid_classes, lipid_class)

}
