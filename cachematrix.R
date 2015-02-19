## The functions above are used to compute the inverse of a matrix faster

## this function returns a list of functions used to cache the inverse of a matrix

makeCacheMatrix <- function(x= matrix()) { #by default, x is an empty matrix
        inv<-NULL # if x has an inv, reset inv
        
        set<-function(y) { # the set function assign the value y to x
                x<<-y
                inv<<-NULL # reset the inv variable in the makeCacheMatrix'x scope
        }
        get<-function() x # the set function returns x
        
        set_inv<-function(inverse) inv<<-inverse # changes the value of inv
        
        get_inv <-function() inv # get_inv access to the inv value
        
        ## return a list with all previous functions
        
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv) }


## This function computes the inverse matrix of x using cache

cacheSolve <- function(x, ...){ # with x a matrix created with makeCacheMatrix 
        
        inv<- x$get_inv() # set inv to the cached value of inv
        if(!is.null(inv)) # if inv has a cached value
                return(inv) #return inv
        
        my_matrix<- x$get() # my_matrix is a copy of x
        
        inv<-solve(my_matrix,...) #  #computes inv with the solve function
        inv # return inv the inverse matrix of x
}
