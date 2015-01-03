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
    AGCTcol <- list("A"="firebrick2", "G"="forestgreen", "C"="blue", "T"="darkorchid3")
    if (is.null(AGCTcol[[nts]])){
        "black"
    } else {
        AGCTcol[[nts]]
    }
}

#' @rdname AGCT2col
nts2size <- function(nts){
    nts <- as.character(nts)
    AGCTsize <- 20
    AGCTcol <- list("A"=AGCTsize, "G"=AGCTsize, "C"=AGCTsize, "T"=AGCTsize)
    if (is.null(AGCTcol[[nts]])){
        16
    } else {
        AGCTcol[[nts]]
    }
}

#' @rdname AGCT2col
nts2font <- function(nts){
    nts <- as.character(nts)
    AGCTbold = "bold"
    AGCTcol <- list("A"=AGCTbold, "G"=AGCTbold, "C"=AGCTbold, "T"=AGCTbold)
    if (is.null(AGCTcol[[nts]])){
        "bold.italic"
    } else {
        AGCTcol[[nts]]
    }
}