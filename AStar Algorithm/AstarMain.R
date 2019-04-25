source("Astar.R")

SOURCE_NODE="A"
DESTINATION_NODE="P"

# Total 16 cities
name <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
name

hvalue <- c(16,17,13,16,16,20,17,11,10,8,4,7,10,7,5,0)
hvalue

#Create a data frame from vector name
heuristic_df <- data.frame(name,hvalue)
heuristic_df


maps <- data.frame(
  from = c("A", "B", "D", "M", "O", "L", "K", "P", "N", "G", "F", "E", "C","B", "D", "E", "C", "H", "I", "J", "D", "D", "D", "L", "K", "P"),
  to =   c("B", "D", "M", "O", "L", "K", "P", "N", "G", "F", "E", "C", "A","C", "C", "H", "H", "I", "J", "N", "H", "K", "L", "M", "N", "J"),
  dist = c( 5,   3,   14,   5,   4,   5,   4,   7,  12,   9,   4,   7,   5, 4,   7,    5,   8,    3,   4,   3,   11,  16,  13,  9,   7,   8)
)

new_data_frame <- data.frame(a=paste(maps[,1],maps[,2],sep = ""), 
                             b=paste(maps[,2],maps[,1],sep = ""),
                             c=maps[,3])
new_data_frame

g <- graph.data.frame(maps, directed = F, vertices = name)
E(g)$a <- c(maps$dist)
V(g)$b <- paste(name,hvalue,sep=':')
plot.igraph(g,edge.label = E(g)$a,
            vertex.label = V(g)$b,vertex.size=12)


sol <- ASTAR_SEARCH(g,SOURCE_NODE,DESTINATION_NODE,new_data_frame,heuristic_df)


if(!is.null(sol$result_path))
{
  cat("Explored: ", sol$explored_nodes,"\n")
  cat("Result: ", rev(sol$result_path),"\n")
  cat("Path Cost: ", sol$path_cost,"\n")
  
  result_graph <- graph.data.frame(maps, directed = F)
  
  vertex_color = ifelse((heuristic_df$name %in% sol$result_path),"green","white")
  E(result_graph)$a <- c(maps$dist)
  V(result_graph)$b <- paste(name,hvalue,sep=':')
  plot.igraph(g,vertex.color=vertex_color,edge.label = E(result_graph)$a,
              vertex.label = V(result_graph)$b,vertex.size=12)
}else
{
  cat("Failed State: No result found from:",SOURCE_NODE, "to", DESTINATION_NODE,"\n")
  cat("Explored: ", sol$explored_nodes,"\n")
}
