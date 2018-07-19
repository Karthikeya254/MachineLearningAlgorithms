#Check if the matrix is not singular
is.not.singular <- function(A) class(try(solve(A),silent=T))=="matrix"