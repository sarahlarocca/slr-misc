profile_data <- function(data, filt_init, xcut, xcutbreaks=NULL, nbars){
  
  data <- data %>%
          filter_(filt_init) %>% 
          mutate(pgm_grp = ifelse(pgm == 'pgm_a' | pgm == 'pgm_b', 'A & B',
                                     ifelse(pgm == 'na', 'C', 'Other')))
          
  
  # true/false indicator for categorical cut
  categorical <- is.factor(eval(parse(text = paste('data$', xcut, sep = ''))))
  nlevels <- length(levels(eval(parse(text = paste('data$', xcut, sep = '')))))
  
  if (categorical){
    
    if (nlevels > nbars){
      
      topn <- data %>%
              group_by_('clm_grp', 'pgm_grp', xcut) %>%
              summarise(num_clms = n(),
                        ult_loss = sum(ult_ol_trd_loss_st_lvn_rpt)) %>%
              group_by(clm_grp, pgm_grp) %>%
              mutate(rank_cnt = min_rank(desc(num_clms)),
                     rank_loss = min_rank(desc(ult_loss))) %>%
              filter(rank_cnt <= nbars | rank_loss <= nbars) %>%
              ungroup() %>%
              select_(xcut) %>%
              distinct() %>%
              mutate(topn_flg = 'Y')

      totals <- data %>%
                group_by(clm_grp, pgm_grp) %>% 
                summarise(num_clms_grp = n(),
                          ult_loss_tot_grp = sum(ult_ol_trd_loss_st_lvn_rpt),
                          ult_loss_med_grp = median(ult_ol_trd_loss_st_lvn_rpt))
      
      other <-  data %>%
                left_join(topn, by = c(xcut)) %>%
                filter(is.na(topn_flg)) %>%
                group_by(clm_grp, pgm_grp) %>% 
                summarise(num_clms = n(),
                          ult_loss_tot = sum(ult_ol_trd_loss_st_lvn_rpt),
                          ult_loss_med = median(ult_ol_trd_loss_st_lvn_rpt)) %>%
                mutate(xcut = as.factor('other')) %>%
                select(clm_grp, pgm_grp, xcut, num_clms, ult_loss_tot, ult_loss_med)
      names(other)[names(other) == 'xcut'] <- paste(xcut)
        
      profile <-  data %>%
                  left_join(topn, by = c(xcut)) %>%
                  filter(topn_flg == "Y") %>%
                  group_by_('clm_grp', 'pgm_grp', xcut) %>%
                  summarise(num_clms = n(),
                            ult_loss_tot = sum(ult_ol_trd_loss_st_lvn_rpt),
                            ult_loss_med = median(ult_ol_trd_loss_st_lvn_rpt))
      
      profile <- rbind(profile, other)
      
      profile <- profile %>%
                 left_join(totals, by = c('clm_grp', 'pgm_grp')) %>%
                 mutate(pct_clms = num_clms/num_clms_grp,
                        pct_ult_loss = ult_loss_tot/ult_loss_tot_grp)
    }else{

      totals <- data %>%
                group_by(clm_grp, pgm_grp) %>% 
                summarise(num_clms_grp = n(),
                          ult_loss_tot_grp = sum(ult_ol_trd_loss_st_lvn_rpt),
                          ult_loss_med_grp = median(ult_ol_trd_loss_st_lvn_rpt))
      
      profile <-  data %>%
                  group_by_('clm_grp', 'pgm_grp', xcut) %>%
                  summarise(num_clms = n(),
                            ult_loss_tot = sum(ult_ol_trd_loss_st_lvn_rpt),
                            ult_loss_med = median(ult_ol_trd_loss_st_lvn_rpt)) %>%
                  left_join(totals, by = c('clm_grp', 'pgm_grp')) %>%
                  mutate(pct_clms = num_clms/num_clms_grp,
                         pct_ult_loss = ult_loss_tot/ult_loss_tot_grp)
    }
  }else{
    
    totals <- data %>%
              group_by(clm_grp, pgm_grp) %>% 
              summarise(num_clms_grp = n(),
                        ult_loss_tot_grp = sum(ult_ol_trd_loss_st_lvn_rpt),
                        ult_loss_med_grp = median(ult_ol_trd_loss_st_lvn_rpt))
            
    xcutbins <- as.character(cut(eval(parse(text=paste('data$', xcut, sep=''))), breaks = xcutbreaks))
    xcutbins[is.na(xcutbins)] <- "other"
    xcutbins <- as.factor(xcutbins)
    
    profile <-  cbind(data, xcutbins) %>%
                group_by(clm_grp, pgm_grp, xcutbins) %>%
                summarise(num_clms = n(),
                          ult_loss_tot = sum(ult_ol_trd_loss_st_lvn_rpt),
                          ult_loss_med = median(ult_ol_trd_loss_st_lvn_rpt)) %>%
                left_join(totals, by = c('clm_grp', 'pgm_grp')) %>%
                mutate(pct_clms = num_clms/num_clms_grp,
                       pct_ult_loss = ult_loss_tot/ult_loss_tot_grp)
  }
  
  profile <-  gather(profile, type, value, num_clms:pct_ult_loss) %>%
              mutate(plot_flg = ifelse(type == 'pct_clms' |
                                       type == 'pct_ult_loss' |
                                       type == 'ult_loss_med', "Y", "N")) %>%
              mutate(pval = NA)
  
  for (i in 1:length(profile$pval)){
    if (categorical){
      profile$pval[i] <- getpval(profile, xcut, i)
    }else{
      profile$pval[i] <- getpval(profile, 'xcutbins', i)
    }
  }
  
  profile <- profile %>%
             mutate(significant = ifelse(pval < 0.05, "Y", "N")) %>%
             mutate(significant = ifelse(is.na(pval), "N", significant))
              
  profile$type <- factor(profile$type,
                         levels=c('num_clms',
                                  'ult_loss_tot',
                                  'ult_loss_med',
                                  'num_clms_grp',
                                  'ult_loss_tot_grp',
                                  'ult_loss_med_grp',
                                  'pct_clms',
                                  'pct_ult_loss'),
                         labels=c('Number of claims',
                                  'Ultimate loss, total (USD)',
                                  'Ultimate loss, median (USD)',
                                  'Number of claims (group)',
                                  'Ultimate loss, total (group)',
                                  'Ultimate loss, median (group)',
                                  'Percent claim count',
                                  'Percent ultimate loss, total'),
                         ordered=TRUE)
  
  return(profile)
  
}