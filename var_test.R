# Function for binning variables and plotting vs loss ratio

var_test <- function(df, var_nm, var_lab, grp_nm = 'grp_dflt', nbins, save_type = 'both', save_nm = NULL){
  
  #---------------------------

  if (is.null(save_nm)){
    save_nm = var_nm
  }
  
  # create cut name
  cut_name <- paste(deparse(substitute(df)), '$', var_nm, sep = '')
  
  # determine if column to group by is already a factor
  factor_bool <- is.factor(eval(parse(text = cut_name)))

  #---------------------------
 
  if (!factor_bool) {
    
    quantile_wgt <- eval(parse(text = paste(deparse(substitute(df)), '$ernd_prem', sep = '')))
    bin_breaks <- unique(wtd.quantile(eval(parse(text = cut_name)), weights = quantile_wgt, normwt = TRUE, probs = seq(0, 1, 1/nbins), na.rm = TRUE))
    
    df_out <- df %>%
              mutate_(bin = interp(~ cut(var, breaks = bin_breaks), var = as.name(var_nm))) %>%
              group_by_('bin', grp_nm) %>%
              summarise(lr = sum(tilossunlimpa_l + tilossunlimpa_n + tilossunlimpa_m)/sum(lk_mel + nl_mel + mo_mel),
                        ernd_prem_sum = sum(ernd_prem),
                        pol_cnt = n())
    
    names(df_out)[names(df_out) == 'bin'] <- paste(var_nm)
    
    df_out %<>% gather(key, value, 3:5)
    df_out$key <- factor(df_out$key,
                         levels = c('lr', 'ernd_prem_sum', 'pol_cnt'),
                         labels = c('Loss ratio', 'Earned premium', 'Policy term count'))
    
    df_out_lr <- df_out %>% filter(key == 'Loss ratio')
   
    lr_plot <- ggplot(data = df_out_lr) +
               geom_line(aes_string(x = var_nm, y = 'value', group = 'key', colour = 'key')) +
               facet_grid(paste('. ~', grp_nm, sep = '')) +
               geom_text(aes_string(x = var_nm, y = 'value', group = 'key', label = 'round(value, 2)'),
                         position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
               scale_colour_zurich(scheme = 'wc', guide = guide_legend(title = NULL)) +
               theme_zurich() +
               theme(legend.position = 'none') +
               xlab(var_lab) +
               ylab('Loss ratio')
  
  #---------------------------
      
  } else {
    
      df_out <- df %>%
                group_by_(var_nm, grp_nm) %>%
                summarise(lr = sum(tilossunlimpa_l + tilossunlimpa_n + tilossunlimpa_m)/sum(lk_mel + nl_mel + mo_mel),
                          ernd_prem_sum = sum(ernd_prem),
                          pol_cnt = n())
      
      df_tmp <- df_out %>%
                ungroup() %>%
                group_by_(var_nm, grp_nm) %>%
                summarise(tot_ernd_prem_sum = sum(ernd_prem_sum)) %>%
                top_n(nbins, tot_ernd_prem_sum) %>%
                select_(var_nm, grp_nm)
       
      df_out %<>% semi_join(df_tmp, grp_nm) %>% gather(key, value, 3:5)
      df_out$key <- factor(df_out$key,
                           levels = c('lr', 'ernd_prem_sum', 'pol_cnt'),
                           labels = c('Loss ratio', 'Earned premium', 'Policy term count'))
   
      df_out_lr <- df_out %>% filter(key == 'Loss ratio')
      
      lr_plot <- ggplot(data = df_out_lr) +
                 geom_bar(aes_string(x = var_nm, y = 'value', group = 'key', fill = 'key'),
                          stat = 'identity',
                          position = 'dodge') +
                 facet_grid(paste('. ~', grp_nm, sep = '')) +
                 geom_text(aes_string(x = var_nm, y = 'value', group = 'key', label = 'round(value, 2)'),
                           position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
                 scale_fill_zurich(scheme = 'wc', guide = guide_legend(title = NULL)) +
                 theme_zurich() +
                 theme(legend.position = 'none') +
                 xlab(var_lab) +
                 ylab('Loss ratio')
  }
  
  #---------------------------

  df_out_prem <- df_out %>%
                 filter(key == 'Earned premium' | key == 'Policy term count') %>%
                 spread(key, value)
  
  names(df_out_prem)[3:4] <- c('ernd_prem_sum', 'pol_cnt')

  prem_plot <- ggplot(data = df_out_prem) +
               geom_bar(aes_string(x = var_nm, y = 'ernd_prem_sum'),
                        stat = 'identity',
                        fill = zurich_colors$primary['zBlue2']) +
               # geom_text(aes_string(x = var_nm, y = 'ernd_prem_sum', label = 'prettyNum(round(ernd_prem_sum, -6), big.mark = ",", scientific = FALSE)'),
               #           position = position_dodge(width = 0.9), vjust = -0.25, size = 4) +
               geom_text(aes_string(x = var_nm, y = 'ernd_prem_sum', label = 'prettyNum(pol_cnt, big.mark = ",")'),
                         position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
               facet_grid(paste('. ~', grp_nm, sep = '')) +
               theme_zurich() +
               theme(axis.text.x = element_text(size=7)) +
               xlab(var_lab) +
               ylab('Earned premium')
  
  #---------------------------
  # save the plot
  
  if (save_type == 'pdf'){
    
    pdf(paste(save_nm, save_type, sep = '.'))
    multiplot(lr_plot, prem_plot, cols = 1)
    dev.off()
    
  } else if (save_type == 'png'){
    
    png(paste(save_nm, save_type, sep = '.'), width=960, height=960, res=144)
    multiplot(lr_plot, prem_plot, cols = 1)
    dev.off()
    
  } else {
    
    pdf(paste(save_nm, 'pdf', sep = '.'))
    multiplot(lr_plot, prem_plot, cols = 1);
    dev.off()
    
    png(paste(save_nm, 'png', sep = '.'), width=960, height=960, res=144)
    multiplot(lr_plot, prem_plot, cols = 1)
    dev.off()  
    
  }
  
  write_csv(df_out, paste(save_nm, 'csv', sep = '.'))
  df_out
  
}
