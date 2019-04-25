source("Queue.R")

PriorityQueue <- setRefClass(
  "PriorityQueue",
  contains = "Queue",
  
  fields = list(
    priorities = "numeric",
    metadata = "numeric"
  ),
  
  methods = list(
    
    push = function(item, priority, otherval=0) {
      'Enqueue inserts element into the priority queue, reordering by priorities.'
      callSuper(item)
      priorities <<- c(priorities, priority)
      metadata <<- c(metadata, otherval)
      order <- order(priorities)
      data <<- data[order]
      priorities <<- priorities[order]
      metadata <<- metadata[order]
    },
    
    pop = function() {
      'Dequeues removes and returns head of the priority queue.'
      if (size() == 0) stop("queue is empty!")
      priorities <<- priorities[-1]
      metadata <<- metadata[-1]
      callSuper()
    },
    
    getTopPriority = function() {
      'Returns the priority value of the head of the priority queue.'
      return(priorities[1])
    },
    
    getMetaData = function() {
      'Returns metadata belonging to the head of the priority queue.'
      return(metadata[1])
    },
    
    pushHigher = function(item, priority, otherval=0) {
      'Replace item if the same item instance with higher priority is found.'
      isPush <- FALSE
      index <- match(item, data)
      if (priorities[index] > priority) {
        isPush <- TRUE
        priorities[index] <<- priority
        metadata[index] <<- otherval
        order <- order(priorities, decreasing=FALSE, partial=size():1)
        data <<- data[order]
        priorities <<- priorities[order]
        metadata <<- metadata[order]
      }
      return(isPush)
    }
  )
)
