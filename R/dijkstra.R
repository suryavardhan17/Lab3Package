#' Dijkstra's Algorithm
#'
#' Computes the shortest path from an initial node to all other nodes in a graph.
#'
#' @param graph A data.frame containing edges with columns v1, v2, and w (weight).
#' @param init_node A numeric scalar representing the starting node.
#'
#' @return A numeric vector representing the shortest path to every other node.
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export
#'
#' @examples
#' wiki_graph <- data.frame(
#'   v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#' dijkstra(wiki_graph, 1)
dijkstra <- function(graph, init_node) {
  if (!is.data.frame(graph) || !all(c("v1", "v2", "w") %in% names(graph))) stop("Invalid graph.")
  if (!is.numeric(init_node) || length(init_node) != 1) stop("Invalid init_node.")

  vertices <- sort(unique(c(graph$v1, graph$v2)))
  if (!(init_node %in% vertices)) stop("init_node not in graph.")

  dist <- rep(Inf, length(vertices))
  names(dist) <- as.character(vertices)
  dist[as.character(init_node)] <- 0

  unvisited <- vertices

  while (length(unvisited) > 0) {
    u_dists <- dist[as.character(unvisited)]
    u <- unvisited[which.min(u_dists)]

    if (is.infinite(dist[as.character(u)])) break

    unvisited <- setdiff(unvisited, u)
    edges <- graph[graph$v1 == u & graph$v2 %in% unvisited, , drop = FALSE]

    if (nrow(edges) > 0) {
      for (i in seq_len(nrow(edges))) {
        v <- edges$v2[i]
        alt <- dist[as.character(u)] + edges$w[i]
        if (alt < dist[as.character(v)]) {
          dist[as.character(v)] <- alt
        }
      }
    }
  }
  return(unname(dist))
}
