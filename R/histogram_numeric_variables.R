histogram_numeric_variables <- function(df., grp){
  # df.: Data set
  # grp: group variable

  # Vector of numeric variables in df.
  numeric_variables <- names(sapply(df., is.numeric))[which(sapply(df., is.numeric))]

  # number of coloumns in final histogram
  n_coloumn_his <- ifelse(sum(is.na(df.[,grp])) > 0, # is a "NA"-value in the grp Variable?
                          length(levels(df.[,grp])) + 1,
                          length(levels(df.[,grp])))

  # Using pivot_longer to summarize nummeric variables in coloumns "NumericVariables" and "Value"
  df. %>% dplyr::select(!!numeric_variables, !!grp) %>%
    pivot_longer(cols = !!numeric_variables,
                 names_to = "NumericVariables", values_to = "Value") %>%
    # renaming group variable (so that it can be used in facet_wrap)
    rename(Group = !!grp) %>%
    # plotting
    ggplot(aes(x = Value)) +
    geom_histogram() +
    facet_wrap(NumericVariables ~ Group, ncol = n_coloumn_his, scales = "free", labeller = label_both)
}
