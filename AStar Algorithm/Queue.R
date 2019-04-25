Queue <- setRefClass(
  Class = "Queue",
  fields = list(name = "character",
                data = "list"),
  methods = list(
    size = function() {
      'Returns number of items in queue'
      return(length(data))
    },
    isEmpty = function() {
      'Returns true if the queue is empty, false otherwise'
      return(size() == 0)
    },
    push = function(item) {
      'Enqueue inserts elements at back of queue'
      data[[size() + 1]] <<- item
    },
    pop = function()
    {
      'Dequeue removes and returns the head of the queue'
      if (size() == 0)
        stop("queue is empty!")
      value <- data[[1]]
      data[[1]] <<- NULL
      value
    }
  )
)