#' Advanced Graph Layout Editor for Mixed Graphs
#'
#' @description
#' An interactive replacement for [ggm::plotGraph] using
#' `shiny` and `visNetwork`.
#' Allows dynamic node repositioning, handles multiple edge types,
#' and outputs node coordinates.
#'
#' @param ggm_matrix
#' * an adjacency matrix for mixed graphs,
#' * an `igraph` object, or
#' * a descriptive character vector.
#' @param dashed Logical. If `TRUE` (default), bidirected edges `<->`
#' are drawn as dashed lines.
#' @param bidirected_roundness Numeric. Curvature of bidirected edges
#' when multiple edges are present. Default is `0.25`.
#' @param Viewer character string specifying where to display the
#'  Shiny application.  Valid options are `"Rstudio"` (to open it
#'  in a native internal window), or `"browser"` (to open it
#'  in the default web browser). If not specified, it
#'  automatically detects the environment, defaulting to`"Rstudio"`
#'  inside RStudio and `"browser"` elsewhere.
#' @param coords Optional initial node positions. Either a numeric matrix
#'  with one row per node (in the order of the adjacency matrix) and two
#'  columns (`x`, `y`), a function such as [igraph::layout_nicely()]
#'  that takes an `igraph` graph and returns such a matrix, or one of the
#'  strings `"TB"`, `"BT"`, `"LR"`, `"RL"`. The graph passed to a function
#'  has the same nodes and adjacencies as the mixed graph (edge types are
#'  ignored). The strings request a layered layout
#'  ([igraph::layout_with_sugiyama()]) computed on the directed edges only,
#'  in the spirit of the `rankdir` option of GraphViz: arrows point from top
#'  to bottom (`"TB"`), bottom to top (`"BT"`), left to right (`"LR"`) or
#'  right to left (`"RL"`). Undirected and bi-directed edges do not
#'  contribute to the layering, so these layouts are most useful for graphs
#'  with many arrows. The positions are enlarged internally to fit
#'  the display, but the returned coordinates are in the same units as
#'  `coords` (for the strings, one unit between consecutive layers). If
#'  `NULL` (default), `visNetwork` chooses the initial positions and the
#'  returned coordinates are in display (pixel) units.
#' @return A matrix of `X` and `Y` coordinates for each node,
#' returned after terminating the Shiny application.
#'
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel titlePanel p br tags actionButton observeEvent stopApp runApp shinyApp
#' @importFrom visNetwork visNetworkOutput renderVisNetwork visNetwork visNodes visNetworkProxy visGetPositions visEdges
#'
#'@author Giovanni M. Marchetti
#'@note The author acknowledges the help of Google Gemini AI (2026) and
#'Posit Assistant (2026).
#' @examples
#' if (interactive()) {
#'   amat <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3,
#'                  dimnames = list(c("A","B","C"), c("A","B","C")))
#'   coords <- graphLayout(amat)
#'   print(coords)
#'
#'   # Generating the adjacency matrix from a vector and plotting
#'   exvec <- c(
#'     "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11,
#'     "a", 10, 8, "a", 11, 2, "a", 11, 9, "a", 11, 10,
#'     "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12
#'   )
#'   graphLayout(exvec)
#'
#'   # Plotting from a direct adjacency matrix
#'   amat2 <- matrix(c(0, 11, 0, 0, 10, 0, 100, 0, 0, 100, 0, 1, 0, 0, 1, 0), 4, 4)
#'   graphLayout(amat2)
#'
#'   # A graph with double and triple edges
#'   G <- structure(c(
#'       0, 101, 0, 0, 100, 0, 100, 100, 0, 100, 0, 100, 0, 111, 100, 0),
#'        dim = c(4L, 4L),
#'        dimnames = list(c("X", "Z", "Y", "W"), c("X", "Z", "Y", "W")))
#'   graphLayout(G)
#'
#'   # A graph with 4 edges between two nodes
#'   G4 <- matrix(0, 2, 2)
#'   G4[1, 2] <- 111
#'   G4[2, 1] <- 111
#'   graphLayout(G4)
#'
#'   # Starting from an igraph layout
#'   graphLayout(G, coords = igraph::layout_nicely)
#'
#'   # Layered layouts (as rankdir in GraphViz), best for graphs with
#'   # many arrows
#'   dag <- c("a", "A", "C", "a", "B", "C", "a", "C", "D",
#'            "a", "C", "E", "a", "D", "F", "a", "E", "F", "a", "B", "E")
#'   graphLayout(dag, coords = "TB")
#'   graphLayout(dag, coords = "LR")
#'   xy <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), 4, 2)
#'   graphLayout(G, coords = xy)
#' }
#' @export

graphLayout <- function(ggm_matrix, dashed = TRUE, bidirected_roundness = 0.25, Viewer = if (Sys.getenv("RSTUDIO") == "1") "Rstudio" else "browser", coords = NULL) {
  # 0. CONTROLLO DI SICUREZZA SULL'INPUT DEL VIEWER
  # Il controllo accetta solo queste due opzioni e gestisce le abbreviazioni
  Viewer <- match.arg(Viewer, choices = c("Rstudio", "browser"))

  # 1. NORMALIZZAZIONE DELL'INPUT
  ggm_matrix <- grMAT2(ggm_matrix)

  # 2. ESTRAZIONE DEI NODI
  n_nodes <- nrow(ggm_matrix)
  node_names <- rownames(ggm_matrix)
  if (is.null(node_names)) {
    node_names <- paste0("v", 1:n_nodes)
  }

  nodes_converted <- data.frame(
    id = 1:n_nodes,
    label = node_names,
    shape = "dot",
    size = 10,
    color = "#97C2FC",
    stringsAsFactors = FALSE
  )

  init <- resolve_coords(coords, ggm_matrix)
  # Factor used to display `coords` at pixel scale; undone on output
  coord_scale <- init$scale
  if (!is.null(init$xy)) {
    nodes_converted$x <- init$xy[, 1] * coord_scale
    # visNetwork's y axis points down; the returned coordinates are flipped back
    nodes_converted$y <- -init$xy[, 2] * coord_scale
  }

  edges_list <- list()
  edge_counter <- 1

  # 3. CALCOLO DELLA MOLTEPLICITÀ DEGLI ARCHI
  edge_count_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)

  for (i in 1:(n_nodes - 1)) {
    for (j in (i + 1):n_nodes) {
      val_str_ij <- sprintf("%03d", as.integer(ggm_matrix[i, j]))
      has_bid_ij <- substr(val_str_ij, 1, 1) == "1"
      has_und_ij <- substr(val_str_ij, 2, 2) == "1"
      has_dir_ij <- substr(val_str_ij, 3, 3) == "1"

      val_str_ji <- sprintf("%03d", as.integer(ggm_matrix[j, i]))
      has_dir_ji <- substr(val_str_ji, 3, 3) == "1"

      total_types <- sum(c(has_bid_ij, has_und_ij, has_dir_ij, has_dir_ji))

      edge_count_matrix[i, j] <- total_types
      edge_count_matrix[j, i] <- total_types
    }
  }

  # 4. ESTRAZIONE ARCHI ORIENTATI
  for (i in 1:n_nodes) {
    for (j in 1:n_nodes) {
      if (i == j) next

      cell_value <- ggm_matrix[i, j]
      if (cell_value == 0) next

      val_str_ij <- sprintf("%03d", as.integer(cell_value))
      has_directed_ij <- substr(val_str_ij, 3, 3) == "1"

      if (has_directed_ij) {
        val_str_ji <- sprintf("%03d", as.integer(ggm_matrix[j, i]))
        has_directed_ji <- substr(val_str_ji, 3, 3) == "1"

        if (has_directed_ji) {
          if (i < j) {
            edges_list[[edge_counter]] <- data.frame(
              from = i, to = j, arrows = "to", dashes = FALSE,
              smooth.enabled = TRUE, smooth.type = "curvedCW", smooth.roundness = 0.25,
              stringsAsFactors = FALSE
            )
            edge_counter <- edge_counter + 1

            edges_list[[edge_counter]] <- data.frame(
              from = j, to = i, arrows = "to", dashes = FALSE,
              smooth.enabled = TRUE, smooth.type = "curvedCW", smooth.roundness = 0.25,
              stringsAsFactors = FALSE
            )
            edge_counter <- edge_counter + 1
          }
        } else {
          # Controlla se tra i due nodi c'è un arco bidirezionale (struttura bow)
          val_str_ji <- sprintf("%03d", as.integer(ggm_matrix[j, i]))
          has_bid_ji <- substr(val_str_ji, 1, 1) == "1"
          has_und_ji <- substr(val_str_ji, 2, 2) == "1"

          # Se coesiste con un arco non orientato, si curva. Se c'è un bidirezionale (bow), RESTA DRITTA!
          is_multiple_with_undirected <- edge_count_matrix[i, j] > 1 && (has_und_ji || substr(val_str_ij, 2, 2) == "1")

          current_type <- if (is_multiple_with_undirected) "curvedCW" else "continuous"
          current_roundness <- if (is_multiple_with_undirected) 0.25 else 0.0

          edges_list[[edge_counter]] <- data.frame(
            from = i, to = j, arrows = "to", dashes = FALSE,
            smooth.enabled = TRUE, smooth.type = current_type, smooth.roundness = current_roundness,
            stringsAsFactors = FALSE
          )
          edge_counter <- edge_counter + 1
        }
      }
    }
  }

  # 5. ESTRAZIONE ARCHI SIMMETRICI
  for (i in 1:(n_nodes - 1)) {
    for (j in (i + 1):n_nodes) {
      cell_value <- ggm_matrix[i, j]
      if (cell_value == 0) next

      val_str <- sprintf("%03d", as.integer(cell_value))
      has_bidirectional <- substr(val_str, 1, 1) == "1"
      has_undirected <- substr(val_str, 2, 2) == "1"

      is_multiple <- edge_count_matrix[i, j] > 1

      if (has_undirected) {
        edges_list[[edge_counter]] <- data.frame(
          from = i, to = j, arrows = "", dashes = FALSE,
          smooth.enabled = TRUE, smooth.type = "continuous", smooth.roundness = 0.0,
          stringsAsFactors = FALSE
        )
        edge_counter <- edge_counter + 1
      }
      # --- Arco Bidirezionale (i <-> j) ---
      if (has_bidirectional) {
        # Nel bow, is_multiple è TRUE, quindi userà curvedCCW
        current_type <- if (is_multiple) "curvedCCW" else "continuous"
        current_roundness <- if (is_multiple) bidirected_roundness else 0.0
        edges_list[[edge_counter]] <- data.frame(
          from = i, to = j,
          arrows = "to, from",
          dashes = dashed,
          smooth.enabled = TRUE, smooth.type = current_type, smooth.roundness = current_roundness,
          stringsAsFactors = FALSE
        )
        edge_counter <- edge_counter + 1
      }
    }
  }

  if (length(edges_list) > 0) {
    edges_converted <- do.call(rbind, edges_list)
  } else {
    edges_converted <- data.frame(
      from = integer(), to = integer(), arrows = character(),
      dashes = logical(), smooth.enabled = logical(),
      smooth.type = character(), smooth.roundness = numeric(),
      stringsAsFactors = FALSE
    )
  }
  # 6. INTERFACCIA UTENTE SHINY (UI)
  ui <- shiny::fluidPage(
    title = "ggm Mixed Graph Layout Editor",
    shiny::tags$head(shiny::tags$style(".container-fluid { padding: 4px; }")),
    # Slim toolbar on top; the graph takes the rest of the window
    shiny::div(
      style = "display: flex; gap: 6px; padding-bottom: 4px;",
      shiny::actionButton("fit_btn", "Fit", class = "btn-sm btn-default"),
      shiny::tags$button(
        id = "snap_btn",
        class = "btn btn-sm btn-success",
        onclick = "
          var canvas = document.querySelector('#my_graph canvas');
          if(canvas) {
            var link = document.createElement('a');
            link.download = 'ggm_graph_layout.png';
            link.href = canvas.toDataURL('image/png');
            link.click();
          } else {
            alert('Error: Canvas element not found yet. Try again in a second.');
          }
        ",
        "Download PNG"
      ),
      shiny::actionButton("close_btn", "Close and save layout", class = "btn-sm btn-danger")
    ),
    visNetworkOutput("my_graph", height = "calc(100vh - 48px)")
  )

  # 7. LOGICA DEL SERVER SHINY
  server <- function(input, output, session) {

    output$my_graph <- renderVisNetwork({
      v <- visNetwork(nodes_converted, edges_converted)
      v <- visNodes(v, physics = FALSE)
      v <- visNetwork::visInteraction(v, zoomSpeed = 0.1)
      v$x$options$renderer <- "canvas"
      v$x$options$configure <- list(enabled = FALSE)
      v$x$options$rendering <- list(preserveDrawingBuffer = TRUE)
      v
    })

    shiny::observeEvent(input$fit_btn, {
      visNetwork::visFit(visNetworkProxy("my_graph"))
    })

    shiny::observeEvent(input$close_btn, {
      proxy <- visNetworkProxy("my_graph")
      visGetPositions(proxy)
    })

    shiny::observeEvent(input$my_graph_positions, {
      positions <- input$my_graph_positions
      ordered_ids <- nodes_converted$id
      coords_matrix <- matrix(0, nrow = length(ordered_ids), ncol = 2)
      rownames(coords_matrix) <- nodes_converted$label
      colnames(coords_matrix) <- c("X", "Y")

      for (i in seq_along(ordered_ids)) {
        current_id <- as.character(ordered_ids[i])
        if (!is.null(positions[[current_id]])) {
          coords_matrix[i, "X"] <- positions[[current_id]]$x / coord_scale
          coords_matrix[i, "Y"] <- -positions[[current_id]]$y / coord_scale
        }
      }

      shiny::stopApp(coords_matrix)
    })

    session$onSessionEnded(function() {
      shiny::stopApp(matrix(numeric(0), nrow = 0, ncol = 2, dimnames = list(NULL, c("X", "Y"))))
    })
  }

  # 8. AVVIO DELL'APPLICAZIONE CON SCELTA DEL VIEWER
  # Aumenta width e height (es. 1400x900) per dare più spazio al grafo
  current_viewer <- if (Viewer == "Rstudio") {
    shiny::dialogViewer("ggm Mixed Graph Layout Editor", width = 1100, height = 850)
    # Usa il pannello integrato invece della finestra pop-up bloccata
    # shiny::paneViewer(minHeight = 500)
  } else {
    shiny::browserViewer()
  }
  shiny::runGadget(shinyApp(ui = ui, server = server), viewer = current_viewer)
}


# Layered (Sugiyama) layout based on the directed edges only: arrows point
# from the first layer to the last. `rankdir` as in GraphViz: "TB" puts
# sources on top, "BT" at the bottom, "LR" on the left, "RL" on the right.
# Returns an n x 2 matrix (y axis pointing up).
layered_layout <- function(adj, rankdir) {
  directed <- (adj %% 10) == 1
  diag(directed) <- FALSE
  g <- igraph::graph_from_adjacency_matrix(+directed, mode = "directed")
  lay <- igraph::layout_with_sugiyama(g)$layout
  pos <- lay[, 1]
  lev <- lay[, 2]

  # igraph's orientation of the layer axis is not guaranteed: make arrows
  # point towards increasing `lev`
  ed <- which(directed, arr.ind = TRUE)
  if (nrow(ed) > 0 && mean(lev[ed[, 2]] - lev[ed[, 1]]) < 0) lev <- -lev

  xy <- switch(rankdir,
    TB = cbind(pos, -lev),
    BT = cbind(pos, lev),
    LR = cbind(lev, -pos),
    RL = cbind(-lev, -pos)
  )
  unname(xy)
}

# Validate `coords` and return list(xy, scale): `xy` is the n x 2 matrix
# (original units) or NULL, and `scale` the factor that maps it to a
# display size of about `half_width` pixels (1 when `coords` is NULL).
resolve_coords <- function(coords, adj, half_width = 300) {
  if (is.null(coords)) return(list(xy = NULL, scale = 1))
  n <- nrow(adj)

  if (is.character(coords) && is.null(dim(coords))) {
    if (length(coords) != 1L || !coords %in% c("TB", "BT", "LR", "RL")) {
      stop("'coords' must be a numeric matrix with ", n,
           " rows and 2 columns, a layout function, or one of ",
           "\"TB\", \"BT\", \"LR\", \"RL\".", call. = FALSE)
    }
    coords <- layered_layout(adj, coords)
  } else if (is.function(coords)) {
    g <- igraph::graph_from_adjacency_matrix(
      +(adj != 0), mode = "max", diag = FALSE
    )
    coords <- coords(g)
  }

  coords <- tryCatch(as.matrix(coords), error = function(e) NULL)
  if (is.null(coords) || !is.numeric(coords) ||
      !identical(dim(coords), c(n, 2L))) {
    stop("'coords' must be a numeric matrix with ", n,
         " rows (one per node) and 2 columns, a layout function, or one of ",
         "\"TB\", \"BT\", \"LR\", \"RL\".", call. = FALSE)
  }
  if (any(!is.finite(coords))) {
    stop("'coords' must not contain missing or infinite values.",
         call. = FALSE)
  }

  m <- max(abs(coords))
  list(xy = unname(coords), scale = if (m > 0) half_width / m else 1)
}

#' Graph to adjacency matrix
#'
#' `grMAT2` converts graph objects to a mixed adjacency matrix.
#'
#' @details Support for `graphNEL` objects requires the `graph` package
#'   from Bioconductor, which is a suggested dependency.
#'   If the package is missing, passing a `graphNEL` object will
#'   trigger an informative error.
#'
#' @param agr A graph object. This can be:
#' * a `graphNEL` object,
#' * an [igraph::igraph()] object, or
#' * a character vector of length \eqn{3e}{3e}, where \eqn{e}
#'   is the number of edges. If it is a vector, it must
#'   be a sequence of triples (`type`, `node1label`, `node2label`).
#'
#'   The type of edge can be:
#'   * `"a"` (arrow from `node1` to `node2`),
#'   * `"b"` (bi-directed arc),
#'   * `"l"` (undirected line), or
#'   * `"*"` (for an isolated node with `node1 == node2`).
#'
#' @return A matrix consisting of 4 different integers
#' representing the \eqn{ij}{ij}-elements:
#' * 0 for a missing edge between \eqn{i} and \eqn{j},
#' * 1 for an arrow from \eqn{i} to \eqn{j},
#' * 10 for a full line between \eqn{i} and \eqn{j}, and
#' * 100 for a bi-directed arrow between \eqn{i} and \eqn{j}.
#' These numbers are added when multiple edges of different types
#' are present. The matrix is symmetric with respect to full lines
#' and bi-directed arrows.
#'
#' @author Kayvan Sadeghi, Giovanni Marchetti
#' @keywords graphs adjacency matrix mixed-graph vector
#'
#' @examples
#' ## Generating the adjacency matrix from a vector
#' exvec <- c(
#'   "b", 1, 2, "b", 1, 14, "a", 9, 8, "l", 9, 11, "a", 10, 8,
#'   "a", 11, 2, "a", 11, 10, "a", 12, 1, "b", 12, 14, "a", 13, 10, "a", 13, 12)
#' grMAT2(exvec)
#'
#' ## Example with graphNEL (requires the 'graph' package from Bioconductor)
#' if (requireNamespace("graph", quietly = TRUE)) {
#'   V <- c("a", "b", "c")
#'   g <- graph::graphNEL(nodes = V, edgemode = "undirected")
#'   grMAT2(g)
#' }
#'
#' g <- c("a", 3, 1, "l", 1, 2, "*", 4, 4)
#' grMAT2(g)
#' @importFrom igraph as_adjacency_matrix
#' @export
grMAT2 <- function(agr) {
    # 0. Controllo sicuro per matrici e array (se è già una matrice di adiacenza valida)
    if (is.matrix(agr) || is.array(agr)) {
      if (nrow(agr) != ncol(agr)) {
        stop("La matrice di adiacenza deve essere quadrata.", call. = FALSE)
      }
      return(agr)
    }

    # 1. Safe check for graphNEL objects (S4 compatible for Bioconductor)
    if ("graphNEL" %in% class(agr) || inherits(agr, "graph")) {
      if (!requireNamespace("graph", quietly = TRUE)) {
        stop(
          "Package 'graph' (Bioconductor) is required to convert graphNEL objects.\n",
          "It can be installed using: BiocManager::install('graph')",
          call. = FALSE
        )
      }
      agr <- methods::as(agr, "matrix")
      return(agr)
    }

  # 2. Safe check for igraph objects (using direct namespace call)
  if (inherits(agr, "igraph")) {
    return(igraph::as_adjacency_matrix(agr, sparse = FALSE))
  }

  # 3. Processing character vectors representing mixed graphs
  if (inherits(agr, "character")) {
    if (length(agr) %% 3 != 0) {
      stop("'The character object' is not in a valid form", call. = FALSE)
    }

    seqt <- seq(1, length(agr), 3)
    b <- agr[seqt]
    agrn <- agr[-seqt]

    # Updated check: Now allowing '*' to declare isolated nodes
    if (!all(b %in% c("a", "l", "b", "*"))) {
      stop("'The numeric object' is not in a valid form", call. = FALSE)
    }

    # Sort unique nodes once at the beginning (includes isolated nodes defined via '*')
    Ragr <- sort(unique(agrn))
    ma <- length(Ragr)
    mat <- matrix(0, nrow = ma, ncol = ma)
    rownames(mat) <- Ragr
    colnames(mat) <- Ragr

    # Vectorized assignment of edge weights (isolated placeholder '*' gets weight 0)
    bn <- numeric(length(b))
    bn[b == "l"] <- 10
    bn[b == "a"] <- 1
    bn[b == "b"] <- 100
    bn[b == "*"] <- 0

    # Extract all source and destination nodes at once
    origins <- agrn[seq(1, length(agrn), 2)]
    destinations <- agrn[seq(2, length(agrn), 2)]

    # Match node positions instantly without any SPl helper function
    row_indices <- match(origins, Ragr)
    col_indices <- match(destinations, Ragr)

    # Loop to accumulate single and multiple edge weights
    for (i in seq_along(bn)) {
      r <- row_indices[i]
      c <- col_indices[i]
      val_bn <- bn[i]

      # If it's a designated isolated node marker or an explicit self-loop placeholder,
      # do nothing and skip to the next iteration to keep the diagonal at 0.
      if (b[i] == "*" || r == c) {
        next
      }

      current_val <- mat[r, c]

      # Historical checks for modulo remainders to accumulate overlapping edges
      if ((val_bn == 1   && current_val %% 10 != 1) ||
          (val_bn == 10  && current_val %% 100 < 10) ||
          (val_bn == 100 && current_val < 100)) {

        mat[r, c] <- current_val + val_bn

        # Apply symmetry for undirected ('l') and bi-directed ('b') edges
        if (val_bn == 10 || val_bn == 100) {
          mat[c, r] <- mat[c, r] + val_bn
        }
      }
    }
    return(mat)
  }

  # 4. Fallback for unsupported object classes
  stop("Input 'agr' must be a graphNEL, igraph, or character object.", call. = FALSE)
}
