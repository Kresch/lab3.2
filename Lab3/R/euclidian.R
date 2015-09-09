#' @title euclidian
#' @param a:=scalar, b:=scalar
#' @description Finds GCD's of two arbitrary scalars.
#' @return GCD of a and b
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm


euclidian<-function(a,b){
        if ((!is.numeric(a))||(!length(a)==1)){stop("Non-numeric value or non-scalar!")}
        if ((!is.numeric(b))||(!length(b)==1)){stop("Non-numeric value or non-scalar!")}
        #stopifnot(is.numeric(b))
        while (b!=0){
                t<-b
                b<-a%%b
                a<-t
        }
        return(a)
}