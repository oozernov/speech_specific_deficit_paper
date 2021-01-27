#install.packages("DiagrammeR")
library(DiagrammeR)

med_diagram <- function(data, height = .75, width = 2, graph_label = NA, node_text_size = 12, edge_text_size = 12, color = "black", ranksep = .2, minlen = 3){
  
  require(glue)
  require(DiagrammeR)
  
  data$height  <- height   # node height
  data$width   <- width    # node width
  data$color   <- color    # node + edge border color
  data$ranksep <- ranksep  # separation btwn mediator row and x->y row
  data$minlen  <- minlen   # minimum edge length
  
  data$node_text_size  <- node_text_size
  data$edge_text_size  <- edge_text_size
  
  data$graph_label <- ifelse(is.na(graph_label), "", paste0("label = '", graph_label, "'"))
  
  diagram_out <- glue::glue_data(data,
                                 "digraph flowchart {
      fontname = Helvetica
      <<graph_label>>
      graph [ranksep = <<ranksep>>]

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>, height = <<height>>, fontsize = <<node_text_size>>, color = <<color>>]        
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      # edge definitions with the node IDs
      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, color = <<color>>]
        mm -> yy [label = '<<coef_my>>'];
        xx -> mm [label = '<<coef_xm>>'];
        xx -> yy [label = '<<coef_xy>>'];
      
      { rank = same; mm }
      { rank = same; xx; yy }
      
      }

      ", .open = "<<", .close = ">>")  
  
  
  DiagrammeR::grViz(diagram_out)  
}

#anotating the diagram
toAnnot <- function (str,ind){
  if (str$pvalue[ind]< 0.05 ) {
    return (paste(toString(round(str$est[ind],digits = 2)),'*',sep=''))
  }
  else {
    return (toString(round(str$est[ind],digits = 2)))
  }
}


