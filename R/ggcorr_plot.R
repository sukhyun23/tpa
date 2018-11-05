ggcorr_plot <- function(cor_matrix){
  lower_idx <- lower.tri(cor_matrix)
  cor_matrix[lower_idx] <- 0
  gdat <- reshape2::melt(cor_matrix)

  if(nrow(gdat) == 0){
    return(NA)
  } else {
    gdat$text_col <- ifelse(c(lower_idx), '0', '1') # black or white
    gdat[gdat$Var1==gdat$Var2,]$value <- NA
    gdat$text_col <- ifelse(abs(gdat$value)>=0.6, '0', gdat$text_col) # if abs(r) is more than 0.6, then text col is white

    ggplot(gdat) +
      geom_tile(aes(x=Var1, y=Var2, fill=value)) +
      geom_text(aes(x=Var1, y=Var2, label=round(value,2), color=text_col),
                size=4.5, fontface='bold') +
      guides(color=F) + scale_color_manual(values = c('white', 'black')) +
      scale_fill_gradientn(colours = c('red', 'lightpink', 'white', 'dodgerblue2', 'blue'),
                           limits=c(-1,1), na.value = 'grey35') +
      labs(x='', y='') +
      theme(
        axis.text.x = element_text(angle=90, vjust = 0.35, hjust = 1),
        panel.background = element_rect(fill='white')
      )
  }
}

