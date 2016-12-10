plot_wc <- function(plot.data, filter.str, ylim.list, labels.list, title, plot.type, save.type, plotname){
  
  pplr.data <- plot.data[plot.data$pol_cut == filter.str & plot.data$key != 'Manual premium',]
  
  if (plot.type == 'bar'){
    pplr.plot <-  ggplot(data = pplr.data, aes(x = x_cut, y = value, group = 1)) +
                  geom_bar(stat = 'identity', fill = colors$secondary$zSkyBlue['tint100']) +
                  theme_slr() +
                  ylim(ylim.list[1], ylim.list[2]) +
                  xlab(labels.list[1]) +
                  ylab(labels.list[2]) +
                  labs(colour = labels.list[3], shape = labels.list[3]) +
                  scale_colour_slr(scheme = 'wc') + 
                  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.text=element_text(size=8)) + 
                  ggtitle(title)
  }else if (plot.type == 'line'){
    pplr.plot <-  ggplot(data = pplr.data, aes(x = x_cut, y = value, group = key)) +
                  geom_line(aes(colour = factor(key))) + geom_point(aes(colour = factor(key)))+
                  theme_slr() +
                  ylim(ylim.list[1], ylim.list[2]) +
                  xlab(labels.list[1]) +
                  ylab(labels.list[2]) +
                  labs(colour = labels.list[3], shape = labels.list[3]) +
                  scale_colour_slr(scheme = 'wc') + 
                  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.text=element_text(size=8)) + 
                  ggtitle(title)
  }

  
  prem.data <- plot.data[plot.data$pol_cut == filter.str & plot.data$key == 'Manual premium',]
    
  prem.plot <-  ggplot(data = prem.data, aes(x = x_cut, y = value, group = 1)) +
                geom_bar(stat = 'identity', fill = colors$primary['zBlue2']) +
                theme_slr() +
                xlab(labels.list[4]) +
                ylab(labels.list[5]) +
                scale_y_continuous(labels = finance_vec)
  
  if (save.type == 'pdf'){
    pdf(plotname)
    multiplot(pplr.plot, prem.plot, cols = 1)
    dev.off()
  } else if (save.type == 'png'){
    png(plotname, width=960, height=960, res=144)
    multiplot(pplr.plot, prem.plot, cols = 1)
    dev.off()    
  }
}
