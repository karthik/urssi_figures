urssi_analysis1 <- function(dat, x, fill) {
  # function is written so you can give it
  # variable names x and fill (in quote) within dataframe dat
  # e.g. x = "Q1.1", fill = "Q5.2"
  
  
  # The goal of this function is to create a booktabs style table where
  # variable x is broken down by the fill variable for:
  #   counts, (n)
  #   the proportion within x excluding missing values, (Proportion_of_Answered)
  #   and proportion within x including missing values (Proportion_of_Total)
  
  # first we make i which is a dataframe storing the counts
  
  i <-  dat %>%
    # next line filter the data to exclude any NA
    # in the variables of interest
    # since variables are written in quotations,
    # we use get() to tell R they're variables in the filter function
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    # now we group by x
    # similar to get(), sym() is used in the group_by function
    # in order to tell R that x is a variable name
    # in this case, !! sym() unquotes x and tells R
    # that x is a variable
    group_by(!!sym(x)) %>%
    tally()
  
  # next we make j, a dataframe storing the proportions
  # of each x by fill answer, this proportion will have the total
  # including missing fill values
  
  
  j <-  dat %>%
    filter(!is.na(get(x))) %>%
    group_by(!!sym(x)) %>%
    tally() %>%
    mutate(n.total = n) %>%
    select(-n)
  
  # finally we create the proportion of x by fill excluding missing values
  # so now when we filter, we filter our missing values of both
  # x and fill
  
  dat %>%
    select(!!sym(x),!!sym(fill)) %>%
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    group_by(!!sym(x),!!sym(fill)) %>%
    tally() %>%
    
    # now that we have all 3 of the pieces of our table,
    # we'll join them all together
    left_join(i, by = x) %>%
    left_join(j, by = x) %>%
    # for clarity, we mutate the names to be make sense
    # and create percentages for the proportions
    mutate(
      n = n.x,
      Proportion_of_Answered = percent(n.x / n.y),
      Proportion_of_Total = percent(n.x / n.total)
    ) %>%
    select(-c(n.x, n.y, n.total)) %>%
    # now we call flextable
    flextable() %>%
    # we combine cells where possible,
    # so our x variable won't repeat titles
    merge_v(j = ~ .) %>%
    # correct the fit
    autofit() %>%
    # add horizontal borders for the booktabs look
    border_inner_h(border = officer::fp_border(color = "black", width = 1),
                   part = "body") %>%
    # correct the border issues
    fix_border_issues()
}

urssi_analysis2 <- function(dat, x, fill) {
  # The goal of this function is to create a bar plot in ggplot
  # showing the frequencies of x and fill.
  # Variable x is on the x axis and bars are filled in with color
  # according to the fill variable.
  
  # tmp is a dataframe storing the counts
  # this isn't used, but is added in in case you want to modify
  # the code to add count numbers in a caption or as annotations
  # directly on the graph
  tmp <-  dat %>%
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    group_by(!!sym(x)) %>%
    tally()
  
  dat %>%
    # start by selecting the 2 variables of interest, x and fill
    select(!!sym(x),!!sym(fill)) %>%
    # filter out missing values for both
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    # create the ggplot
    ggplot(aes(x = get(x), fill = get(fill))) +
    # dodge makes the bars side by side
    geom_bar(position = "dodge") +
    # change the label of the y axis to Frequency
    labs(x = '', y = 'Frequency') +
    # make the xaxis text labels angled to fit better
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      # put the legend on the bottom
      legend.position = "bottom",
      # get rid of the legend title
      legend.title = element_blank()
    )
}
urssi_analysis3 <- function(dat, x, fill) {
  # The goal of this function is to create a bar plot in ggplot
  # showing the proportiond of x and fill.
  # Variable x is on the x axis and bars are filled in with color
  # according to the fill variable.
  
  # tmp is a dataframe storing the counts
  # this isn't used, but is added in in case you want to modify
  # the code to add count numbers in a caption or as annotations
  # directly on the graph
  tmp <-  dat %>%
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    group_by(!!sym(x)) %>%
    tally()
  
  dat %>%
    # start by selecting the 2 variables of interest, x and fill
    select(!!sym(x),!!sym(fill)) %>%
    # filter out missing values for both
    filter(!is.na(get(x)) & !is.na(get(fill))) %>%
    # create the ggplot
    ggplot(aes(x = get(x), fill = get(fill))) +
    # fill makes the bars stacked
    geom_bar(position = 'fill') +
    # change the label of the y axis to Proportion
    labs(x = '', y = 'Proportion') +
    # make the xaxis text labels angled to fit better
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      # put the legend on the bottom
      legend.position = "bottom",
      # get rid of the legend title
      legend.title = element_blank()
    )
}
