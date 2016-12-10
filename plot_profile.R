plot_profile <- function(plot.data, xcut, plot.labels, save.type, save.name, xlimits=NULL, categorical=TRUE){

  if (categorical){
    plot.data <- plot.data %>%
                 filter(plot_flg == 'Y') %>%
                 filter_(paste(xcut, " != 'other'", sep = "")) %>%
                 filter_(paste(xcut, " != ''", sep = ""))
    
    plot.out <- ggplot(data = plot.data) +
                geom_bar(aes_string(x = xcut, weight = 'value', fill = 'pgm_grp', linetype = 'significant', colour = 'significant', stat = "'identity'"),
                         position = 'dodge') +                                  
                facet_grid(type~., scales='free_y') +
                xlab(plot.labels[1]) +
                ylab('') 
                labs(fill = 'Program group') + 
                ggtitle(plot.labels[2]) +
                theme_slr() +
                scale_fill_slr() +
                scale_colour_manual(values = c('black', unname(colors$chart$zSalmon['tint100'])), guide = FALSE) +
                scale_linetype_manual(values = c(0,1), guide = FALSE) +
                theme(axis.text.x=element_text(size=7, angle = 45, hjust = 1),
                      strip.text.y = element_text(size=7)) +
                theme(legend.position = 'bottom',
                      legend.direction = 'horizontal',
                      legend.text=element_text(size=8))
      
  }else{
    
    plot.out <- ggplot(data = plot.data) +
                geom_density(aes_string(x = xcut, fill = 'pgm_grp'), alpha = 0.4, adjust = 5) +
                geom_vline(aes(xintercept = eval(paste('median(', xcut, ')', sep='')))) +
                xlim(xlimits[1], xlimits[2]) +
                xlab(plot.labels[1]) +
                ylab('Density') +
                labs(fill = 'Program group') + 
                ggtitle(plot.labels[2]) +
                theme_slr() +
                scale_fill_manual(values = unname(c(colors$chart$zTeal['tint100'],
                                                    colors$chart$zLemon['tint100'],
                                                    colors$chart$zSalmon['tint100']))) +
                theme(axis.text.x=element_text(size=10)) +
                theme(legend.position = 'bottom',
                      legend.direction = 'horizontal',
                      legend.text=element_text(size=8))
              
  }
  
  if (save.type == 'pdf'){
    pdf(save.name)
    plot(plot.out)
    dev.off()
  } else if (save.type == 'png'){
    png(save.name, width=960, height=960, res=144)
    plot(plot.out)
    dev.off()    
  }
}