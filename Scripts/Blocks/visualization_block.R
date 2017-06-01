source('./Library/visualization.R', echo=F)
source('./Scripts/config.R', echo=F)
source('./Scripts/Blocks/export_block.R', echo=F)
source('./Scripts/Blocks/column_selection_block.R', echo=F)

vizualization_block <- function(data) {
  is_expert <- winDialog('yesno', "Rezhim experta ?") == 'YES'
  menu_fields <- c('Boxplot', 'Barplot', 'Hist', 'Piechart', 'Scatterplot', 'Vverh')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite graphic')
    switch(user_action,
           boxplot_block(data, is_expert),
           barplot_block(data, is_expert),
           hist_block(data, is_expert),
           piechart_block(data, is_expert),
           scatter_block(data, is_expert),
           break
           )
  }
}

boxplot_block <- function(data, is_expert) {
  horizontal <- (winDialog("yesno", "Horizontalnoe raspolozhenie?") == "YES")
  first_column <- columns_selection_block(data, 'Vvedite 1 kolonku')
  second_column <- columns_selection_block(data, 'Vvedite 2 kolonku')
  title <- gettextf('Boxplot for "%s", "%s"', first_column, second_column)
  if (is_expert)
    if (winDialog("yesno", "Hotite vvesti zagolovok?") == "YES")
      title <- winDialogString("Vvedite zagolovok", default = title)
  plot_boxplot(data, horizontal, first_column, second_column, title)
  export_block(is_expert)
}

barplot_block <- function(data, is_expert) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  horizontal = (winDialog("yesno", "Horizontal?") == "YES")
  xlabel <- 'X'
  ylabel <- 'Y'
  title <- gettextf('Barplot for "%s"', column)
  if (is_expert)
    if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
      ylabel <- winDialogString("Nazvanie Y-osi", default = ylabel)
      xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
      title <- winDialogString("Zagolovok", default = title)
    }
  plot_barplot(data, horizontal, column, xlabel, ylabel, title)
  export_block(is_expert)
}

hist_block <- function(data, is_expert) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  xlabel <- 'X'
  title <- gettextf('Histogram of "%s"', column)
  if (is_expert)
    if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
      xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
      title <- winDialogString("Zagolovok", default = title)
    }
  plot_hist(data, column, xlabel, title)
  export_block(is_expert)
}

piechart_block <- function(data, is_expert) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  title <- gettextf('Piechart for "%s"', column)
  if (is_expert)
    if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES") 
      title <- winDialogString("Zagolovok", default = title)
  plot_piechart(data, column, title)
  export_block(is_expert)
}

scatter_block <- function(data, is_expert) {
  first_column <- columns_selection_block(data, 'Vvedite 1 kolonku')
  second_column <- columns_selection_block(data, 'Vvedite 2 kolonku')
  ylabel <- 'Y'
  xlabel <- 'X'
  title <- gettextf('Scatter plot for "%s", "%s"', first_column, second_column)
  if (is_expert)
    if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
      ylabel <- winDialogString("Nazvanie Y-osi", default = ylabel)
      xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
      title <- winDialogString("Zagolovok", default = title)
    }
  
  plot_scatterplot(data, first_column, second_column, xlabel, ylabel, title)
  export_block(is_expert)
}