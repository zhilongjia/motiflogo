#' generate AGCT attributes
#' 
#' internal function.
#'
#' @export
AGCT2col <- function(nt){
    nt = as.character(nt)
    AGCTcol <- list("A"="firebrick2", "G"="forestgreen", "C"="blue", "T"="darkorchid3")
    if (is.null(AGCTcol[[nt]])){
        "black"
    } else {
        AGCTcol[[nt]]
    }
}

#' @rdname AGCT2col
nts2col <- function(nts){
    nts <- as.character(nts)
    AGCTcol <- list("A\nA"="firebrick2", "G\nG"="forestgreen", "C\nC"="blue", "T\nT"="darkorchid3")
    if (is.null(AGCTcol[[nts]])){
        "black"
    } else {
        AGCTcol[[nts]]
    }
}

#' @rdname AGCT2col
nts2size <- function(nts){
    nts <- as.character(nts)
    AGCTcol <- list("A\nA"=14, "G\nG"=14, "C\nC"=14, "T\nT"=14)
    if (is.null(AGCTcol[[nts]])){
        20
    } else {
        AGCTcol[[nts]]
    }
}

#' @rdname AGCT2col
nts2font <- function(nts){
    nts <- as.character(nts)
    AGCTcol <- list("A\nA"="bold", "G\nG"="bold", "C\nC"="bold", "T\nT"="bold")
    if (is.null(AGCTcol[[nts]])){
        "bold.italic"
    } else {
        AGCTcol[[nts]]
    }
}