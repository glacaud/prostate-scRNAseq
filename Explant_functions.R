

### Function to plot by gene
### -------------------------

plotUMAP_colByGene <- function(OBJ=sci, UMAP = sci.UMAP, GENE="Runx1", ptsize = 0.8, alpha = 0.8, legend="yes"){
  
  # Variables
  seu <- OBJ
  g <- GENE
  df.UMAP = UMAP
  pt.size = ptsize
  alph = alpha
  
  g.exp <- seu[["RNA"]]@data[g,]
  g.exp[g.exp == 0] <- NA # replace 0 with NA
  
  df_plot <- data.frame(UMAP_1 = df.UMAP[,1],
                        UMAP_2 = df.UMAP[,2],
                        Gene = g.exp)
  
  if (legend == "yes") {
    pUMAPgene <-
      ggplot(df_plot %>% arrange(Gene), aes(x = UMAP_1, y = UMAP_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        na.value = "grey80",
        end = 0.8
      ) +
      xlab("UMAP 1") +
      ylab("UMAP 2") +
      theme_bw() +
      theme(
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")
      )
    
    print(pUMAPgene)
  } else if (legend == "no") {
    pUMAPgene <-
      ggplot(df_plot %>% arrange(Gene), aes(x = UMAP_1, y = UMAP_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        na.value = "grey80",
        end = 0.8
      ) +
      xlab("UMAP 1") +
      ylab("UMAP 2") +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")
      )
    
    print(pUMAPgene)
  } else if (legend == "minimal") {
    pUMAPgene <-
      ggplot(df_plot %>% arrange(Gene), aes(x = UMAP_1, y = UMAP_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        na.value = "grey80",
        end = 0.8
      ) +
      xlab("UMAP 1") +
      ylab("UMAP 2") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
    print(pUMAPgene)
  }
  
}

### Function to plot by gene in FA space / for genesets/AUCell
### -------------------------

plotFA_colByGene <- function(OBJ=sci, FA = sci.FA, GENE="Runx1", ptsize = 0.8, alpha = 0.8, legend="yes", assay="AUC", lim=NULL){
  
  # Variables
  seu <- OBJ
  g <- GENE
  df.FA = FA
  pt.size = ptsize
  alph = alpha
  #if (is.null(lim)) {lim <- NULL}
    
  g.exp <- seu[[assay]]@data[g,]
  #g.exp[g.exp == 0] <- NA # replace 0 with NA
  
  df_plot <- data.frame(FA_1 = df.FA[,1],
                        FA_2 = df.FA[,2],
                        Gene = g.exp)
  
  min.gene <- min(df_plot$Gene)
  max.gene <- max(df_plot$Gene)
  
  if (legend == "yes") {
    pFAgene <-
      ggplot(df_plot %>% arrange(Gene),
             aes(x = FA_1, y = FA_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        #na.value = "grey80",
        end = 0.9, limits = lim
      ) +
      xlab("FA 1") +
      ylab("FA 2") +
      theme_bw() +
      theme(
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")
      )
    
    print(pFAgene)
  } else if (legend == "no") {
    pFAgene <-
      ggplot(df_plot %>% arrange(Gene), aes(x = FA_1, y = FA_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        #na.value = "grey80",
        end = 0.9, limits = lim
      ) +
      xlab("FA 1") +
      ylab("FA 2") +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")
      )
    
    print(pFAgene)
  } else if (legend == "minimal") {
    pFAgene <-
      ggplot(df_plot %>% arrange(Gene), 
             aes(x = FA_1, y = FA_2, colour = Gene)) +
      geom_point(size = pt.size, alpha = alph) +
      viridis::scale_color_viridis(
        name = g,
        option = "plasma",
        #na.value = "grey80",
        end = 0.9, limits = lim
      ) +
      xlab("FA 1") +
      ylab("FA 2") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
    print(pFAgene)
  }
  
}
