source("BFS.R")

# Create a vector with names of cities
name <- c("A", "B", "C", "D", "E", "F", "G")
name

#Create a data frame from vector name
cities <- data.frame(name)
cities

# Create another data frame maps with 3 verctors: from, to, dist

#from is charachter vector
#> typeof(c("A","A","A","B","C","D","G","G"))
#[1] "character"

#to is charachter vector
#> typeof(c("B","C","F","D","E","F","A","E"))
#[1] "character"

#dist is double vector
#> typeof(c(1,1,1,1,1,1,1,1))
#[1] "double"

maps <- data.frame(
  from = c("A", "A", "A", "B", "C", "D", "G", "G"),
  to = c("B", "C", "F", "D", "E", "F", "A", "E"),
  dist = c(1, 1, 1, 1, 1, 1, 1, 1)
)
maps

#creates an igraph graph from maps dataframe
g <- graph.data.frame(maps, directed = F, vertices = cities)
E(g)$weight <- c(maps$dist)
plot.igraph(g, edge.label = E(g)$weight)

#Assign return environment solution to variable bfsReturn
bfsReturn <- BREADTH_FIRST_SEARCH(input_graph=g, input_source_node="E", input_destination_node="F")

if(!is.null(bfsReturn$result_path))
{
  #Display output
  cat("Solution Path : ", bfsReturn$result_path, "\n")
  cat("Path Cost : ", bfsReturn$path_cost, "\n")
  
  #call function BREADTH-FIRST-SEARCH() here and display graph
  # with path or faliure
  
  result_graph <- graph.data.frame(maps, directed = T, vertices = cities)
  vertex_color = ifelse((cities$name %in% bfsReturn$result_path),"green","blue")
  plot.igraph(result_graph,vertex.color=vertex_color,edge.label.cex=1)
}else
  print("Fail Case: No path found from Source to Goal State!!!")

cat("Explored Nodes: ", bfsReturn$explored_nodes, "\n")
