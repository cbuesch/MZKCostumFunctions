histogram_numeric_variables <- function(df., grp){
  # df.: Data set
  # grp: group variable

  # changing variables saved as difftime to numeric
  if (sum(sapply(df., function(x){inherits(x, "difftime")}))>0){
    df.[, names(df.)[which(sapply(df., function(x){inherits(x, "difftime")}))]] <- as.numeric(df.[, names(df.)[which(sapply(df., function(x){inherits(x, "difftime")}))]][[1]])
  }

  # Vector of numeric variables in df.
  numeric_variables <- names(df.)[which(sapply(df., is.numeric))]

  # number of columns in final histogram
  n_coloumn_his <- ifelse(sum(is.na(df.[,grp])) > 0, # is a "NA"-value in the grp Variable?
                          length(levels(df.[,grp, drop = TRUE])) + 1,
                          length(levels(df.[,grp, drop = TRUE])))

  # for loop (n_row numeric Variable maximal at once)
  n_row  <- 3
  p_hist <- rep(list(NA), ceiling(length(numeric_variables)/n_row))
  for(i in 1:ceiling(length(numeric_variables)/n_row)){
    # Using pivot_longer to summarize nummeric variables in coloumns "NumericVariables" and "Value"
    if(i!=ceiling(length(numeric_variables)/n_row)){
      p_hist[[i]] <- df. %>%
        dplyr::select(!!numeric_variables[((i-1)*n_row+1) : (i*n_row)], !!grp) %>%
        pivot_longer(cols = !!numeric_variables[((i-1)*n_row+1) : (i*n_row)],
                     names_to = "NumericVariables", values_to = "Value") %>%
        # renaming group variable (so that it can be used in facet_wrap)
        rename(Group = !!grp) %>%
        # plotting
        ggplot(aes(x = Value)) +
        geom_histogram() +
        facet_wrap(NumericVariables ~ Group, ncol = n_coloumn_his, scales = "free", labeller = label_both)
    }else{
      p_hist[[i]] <- df. %>%
        dplyr::select(!!numeric_variables[((i-1)*n_row+1) : length(numeric_variables)], !!grp) %>%
        pivot_longer(cols = !!numeric_variables[((i-1)*n_row+1) : length(numeric_variables)],
                     names_to = "NumericVariables", values_to = "Value") %>%
        # renaming group variable (so that it can be used in facet_wrap)
        rename(Group = !!grp) %>%
        # plotting
        ggplot(aes(x = Value)) +
        geom_histogram() +
        facet_wrap(NumericVariables ~ Group, ncol = n_coloumn_his, scales = "free", labeller = label_both)
    }
  }
  return(p_hist)
}
