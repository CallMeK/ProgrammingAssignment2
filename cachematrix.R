## Put comments here that give an overall description of what your
## functions do

## To run it:
## x<-matrix(runif(100),10,10)
## x_cls <- makeCacheMatrix(x)
## cacheSolve(x_cls)

## This function acts like a factory function as in many other languages
## The form of this function is the same as the example in the class
## Enssetially it is just a class

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL;
    set <- function(x_val){
        x <<- x_val;
        x_inv <<- NULL; 
    }
    get <- function() x;
    set_inv <- function(x_inv_val) x_inv <<- x_inv_val;
    get_inv <- function() x_inv;
    list(set=set,get=get,set_inv=set_inv,get_inv=get_inv);
}


## This function is to compute the inverse of a matrix that is stored in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Try to get the value from x
        x_inv <- x$get_inv();
        if(!is.null(x_inv)){
            message("getting cached data");
            return(x_inv);
        }
        data <- x$get();
        x_inv <- solve(data,...);
        x$set_inv(x_inv);
        x_inv;
}
