## When given a matrix, both of these programs in combination are going to figure out if that matrix already has 
## an inverse stored in the cache, in which case the program will display the cached inverse. If the inverse does
## not exist in the cache, the setinv function will be used to store it to cache for the next time it is needed

## The first function makeCacheMatrix is just a list that has individual functions to obtain data (matrix)
## and store the matrix as well as get and store its inverse in cache. 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y){
                x<<-y   # assign the value of y to x in environment outside this function
                inv<<- NULL # if an inverse was stored in cache, wipe it, because the value of x has changed
        }
        get<-function() x
        setinv<-function(inverse) inv<<-inverse #if a new inverse is given as argument to this function, it will set 
        #the inverse stored in the cache to this new inverse
        getinv<-function() {
                inv #extract value of inverse of a matrix from cache
        }
        
        list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve calls functions we wrote above to first find if inverse exists in cache, and display it, otherwise
## calculate inverse and store it in cache and also display the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()        # execute the getinverse() function to try to find inverse in cache
        if (!is.null(inv)){     # if inverse is not null ie exists in the cache..
                message("getting cached inverse")
                return(inv)     # ... return the inverse to cachSolve
        }
        mat<-x$get()            # this step obtains matrix (named mat) by executing function get()
        inv<-solve(mat,...)     # this step calculates inverse of matrix called mat
        x$setinv(inv)           # this step stores the value of the newly calculated inverse to cache
        inv                     # this step returns the inverse to cacheSolve
}
