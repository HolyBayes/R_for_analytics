source('./Library/visualization.R', echo=F)
source('./Scripts/config.R', echo=F)

vizualization_block <- function(data) {
  menu_fields <- c('Boxplot', 'Barplot', 'Hist', 'Piechart', 'Scatterplot', 'Otmena')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite graphic')
    switch(user_action,
           boxplot_block(data),
           barplot_block(data),
           hist_block(data),
           piechart_block(data),
           scatter_block(data),
           break
           )
  }
}

columns_selection_block = function(data, title){
  columns <- c(colnames(data))
  user_action <- menu(columns, graphics=T, title=title)
  return(columns[user_action])
}

export_block <- function() {
  if (winDialog('yesno', "Sohranit' graphic?") == "YES") {
    name <- winDialogString("Vvedite nazvanie graphika (Nazvanie ne dolzhno byt' pustym!)", default = "")
    menu_fields <- c('jpeg','pdf','png')
    user_action <- menu(menu_fields, graphics=T, title='Vyberite format')
    switch(user_action,
           {image <- jpeg; image_type <- 'jpeg'},
           {image <- pdf; image_type <- 'pdf'},
           {image <- png; image_type <- 'png'}
      )
    export_dir <- GRAPHICS_EXPORT_DIR
    if (winDialog("yesno", gettextf("Vy hotite pereopredelit' tekushuyu papku sohraneniya graphikov (%s) ?", export_dir)) == "YES") export_dir <- choose.dir()
    dev.copy(image, paste(export_dir, '/', name, '.', image_type, sep=''))
    dev.off()
  }
}

boxplot_block <- function(data) {
  horizontal <- (winDialog("yesno", "Horizontalnoe raspolozhenie?") == "YES")
  first_column <- columns_selection_block(data, 'Vvedite 1 kolonku')
  second_column <- columns_selection_block(data, 'Vvedite 2 kolonku')
  title <- ''
  if (winDialog("yesno", "Hotite vvesti zagolovok?") == "YES")
    title <- winDialogString("Vvedite zagolovok", default = "")
  plot_boxplot(data, horizontal, first_column, second_column, title)
  export_block()
}

barplot_block <- function(data) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  horizontal = (winDialog("yesno", "Horizontal?") == "YES")
  xlabel <- 'X'
  ylabel <- 'Y'
  title <- ''
  if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
    ylabel <- winDialogString("Nazvanie Y-osi", default = ylabel)
    xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
    title <- winDialogString("Zagolovok", default = title)
  }
  plot_barplot(data, column, horizontal, xlabel, ylabel, title)
  export_block()
}

hist_block <- function(data) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  xlabel <- 'X'
  title <- ''
  if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
    xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
    title <- winDialogString("Zagolovok", default = title)
  }
  plot_hist(data, column, xlabel, title)
  export_block()
}

piechart_block <- function(data) {
  column <- columns_selection_block(data, 'Vyberite kolonky')
  title <- ''
  if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES") 
    title <- winDialogString("Zagolovok", default = title)
  plot_piechart(data, column, title)
  export_block()
}

scatter_block <- function(data) {
  first_column <- columns_selection_block(data, 'Vvedite 1 kolonku')
  second_column <- columns_selection_block(data, 'Vvedite 2 kolonku')
  ylabel <- 'Y'
  xlabel <- 'X'
  title <- ''
  if (winDialog("yesno", "Vvesti podpisi k grafiky?") == "YES"){
    ylabel <- winDialogString("Nazvanie Y-osi", default = ylabel)
    xlabel <- winDialogString("Nazvanie X-osi", default = xlabel)
    title <- winDialogString("Zagolovok", default = title)
  }
  
  plot_scatterplot(data, first_column, second_column, xlabel, ylabel, title)
  export_block()
}