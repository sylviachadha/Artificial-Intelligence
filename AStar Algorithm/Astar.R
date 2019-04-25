source("PriorityQueue.R")

ASTAR_SEARCH <-
  function(input_graph, #maps
           input_source_node,
           input_destination_node,
           input_data_frame,
           heuristic_df)
  {
    # State of node = n
    # Initial Path Cost = 0
    # explored is an empty vector
    
    node <- input_source_node
    path_cost <- 0
    explored <- c()
    solution <- new.env(hash = T, parent = emptyenv())
    
    # frontier is a priority Queue    
    frontier <- PriorityQueue$new()
    
    # Push operation on Frontier priority Queue which takes input as node and path_cost    
    frontier$push(node,path_cost)
    
    # Return function will end the code so no need of else in this if loop  
    while(!frontier$isEmpty())
    {
      if(frontier$isEmpty())
      {
        updated_solution <- UPDATE_SOLUTION(NULL,NULL,explored,solution)
        return(updated_solution) #Failed case
      }
      
      #To get path cost
      pCost <- frontier$getTopPriority()
      node <- frontier$pop()
      
      # Add node to explored node after pop operation
      explored <- append(explored, node)
      
      if(node == input_destination_node)
      {
        path <- GET_RETURN_PATH(input_destination_node,
                                input_source_node,solution)
        
        updated_solution <- UPDATE_SOLUTION(path,
                                            pCost,
                                            explored,
                                            solution)
        return(updated_solution)
      }
      
      #Get corrsoponding elemnts of node from input graph.data.frame
      CHILD_NODES <- GET_CHILD_NODES(input_graph,node)$name
      
      # Loop through all the CHILD_NODES values
      for (child in CHILD_NODES)
      {
        
        if (!(child %in% explored | child %in% frontier$data))
        {
          assign(child,node,solution)
          final_dist <- GET_DISTANCE_FROM_SOURCE(input_data_frame,
                                                 child,input_source_node,
                                                 solution,heuristic_df)
          
          frontier$push(child,final_dist)
          
        }
        
        else if(child %in% frontier$data) {
          assign(child,node,solution)
          final_dist <- GET_DISTANCE_FROM_SOURCE(input_data_frame,
                                                 child,input_source_node,
                                                 solution,heuristic_df)
          
          ## For Display
          isPush <- frontier$pushHigher(child,final_dist)
        }
        
      }
      
    }
    updated_solution <- UPDATE_SOLUTION(NULL,NULL,explored,solution)
    return(updated_solution)
    
  }

GET_CHILD_NODES <- 
  function(input_graph,input_node)
  {
    return(V(g)[neighbors(input_graph, input_node)])
  }


GET_DISTANCE <- function(df,input_node,input_dest_node,cal_heuristic)
{
  colnames(df) <- c("from","to","dist")
  input_value <- paste(input_node,input_dest_node,sep = "")
  filter <- df$from == input_value | df$to == input_value
  if(cal_heuristic)
  {
    heuristic_value <- GET_HEURISTIC(heuristic_df,input_node)
    h_dist <- df[filter,3] + heuristic_value
  }
  else
  {
    h_dist <- df[filter,3]
  }
  
  return(h_dist)
}

GET_HEURISTIC <- function(heuristic_data_frame,node_name)
{
  colnames(heuristic_df) <- c("name","hvalue")
  row_num <- which(heuristic_df$name == node_name)
  return(heuristic_df[row_num,2])
}

GET_DISTANCE_FROM_SOURCE <- function(input_data_frame,
                                     input_from_node,
                                     source_node,
                                     input_env,
                                     heuristic_dis)
{
  
  
  cal_heuristic <- TRUE
  dist <- 0
  temp <- input_from_node
  while (temp != source_node)
  {
    parent_node <- get(temp, input_env)
    get_dist <- GET_DISTANCE(input_data_frame,temp,parent_node,cal_heuristic)
    cal_heuristic <- FALSE
    dist <- dist + get_dist
    temp <- parent_node
  }
  
  return(dist)
}


GET_RETURN_PATH <- function(input_from_node,source_node,input_env)
{
  temp <- input_from_node
  path <- c()
  while (temp != source_node)
  {
    parent_node <- get(temp, input_env)
    path <- append(path, temp)
    temp <- parent_node
  }
  path <- append(path, source_node)
  
  return(path)
}

UPDATE_SOLUTION <-
  function(input_result_path,
           input_path_cost,
           input_explored_nodes,
           input_environment) #Environment is solution
  {
    assign("result_path", input_result_path, input_environment)
    assign("explored_nodes", input_explored_nodes, input_environment)
    assign("path_cost", input_path_cost, input_environment)
    return(input_environment)
    
  }
