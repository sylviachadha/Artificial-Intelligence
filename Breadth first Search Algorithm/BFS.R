source("Queue.R")

BREADTH_FIRST_SEARCH <-
  function(input_graph,
           input_source_node,
           input_destination_node)
  {
    path_cost = 0L
    result_path <- c()
    explored <- c()
    #hash	if TRUE the environment will use a hash table.
    solution <- new.env(hash = T, parent = emptyenv())
    
    node <- input_source_node
    
    if (node == input_destination_node)
    {
      return(RETURN_SOLUTION(node, path_cost, node , solution))
    }
    
    frontier <- Queue$new()
    
    #push input_source_node
    frontier$push(node)
    
    
    #Loop until queue is empty
    while (!frontier$isEmpty())
    {
      #Assign node to popped node from Queue
      node <- frontier$pop()
      
      #Add node to explored node after pop operation
      explored <- append(explored, node)
      
      #Get corrsoponding elemnts of node from input graph.data.frame
      CHILD_NODES <- V(g)[neighbors(g, node)]
      
      # Loop through all the CHILD_NODES values
      for (child in CHILD_NODES$name)
      {
        assign(child, node, solution)
        
        if (!(child %in% explored | child %in% frontier$data))
        {
          if (child == input_destination_node)
          {
            temp <- input_destination_node
            while (temp != input_source_node)
            {
              path_cost <- path_cost + 1L
              result_path <- append(result_path, temp)
              temp <- get(temp, solution)
            }
            result_path <- append(result_path, input_source_node)
            return(RETURN_SOLUTION(rev(result_path), path_cost,explored, solution))
          }
          frontier$push(child)
        }
      }
      
    }
    return(RETURN_SOLUTION(NULL,NULL,explored,solution))
  }

RETURN_SOLUTION <-
  function(input_result_path,
           input_path_cost,
           input_explored_nodes,
           input_environment)
  {
    assign("result_path", input_result_path, input_environment)
    assign("explored_nodes", input_explored_nodes, input_environment)
    assign("path_cost", input_path_cost, input_environment)
    return(input_environment)
    
  }
