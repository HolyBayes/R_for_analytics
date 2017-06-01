source('./Library/data_analysis.R', echo=F)
source('./Scripts/Blocks/export_block.R', echo=F)
source('./Scripts/Blocks/column_selection_block.R', echo=F)

ml_block <- function(data) {
  is_expert <- winDialog('yesno', "Rezhim experta ?") == 'YES'
  menu_fields <- c('Regression analysis', 'Cluster analysis', 'Vverh')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite deystvie')
    switch(user_action,
           regression_block(data, is_expert),
           cluster_block(data, is_expert),
           break       
    )    
  }
}

regression_export_block <- function(fit, is_expert) {
  if (winDialog('yesno', "Sohranit' model?") == "YES") {
    name <- winDialogString("Vvedite nazvanie faila", default = "")
    export_dir <- MODELS_EXPORT_DIR
    if (is_expert)
      if (winDialog("yesno", gettextf("Vy hotite pereopredelit' tekushuyu papku sohraneniya modeley (%s) ?", export_dir)) == "YES") export_dir <- choose.dir()
    export_path = paste(export_dir, name, sep='/')
    dump_regression_model(fit, export_path)
  }
}

plot_regression <- function(data, fit, train_column, target_column, is_expert) {
  title <- gettextf('Regression of "%s" from "%s"', target_column, train_column)
  xlabel <- train_column
  ylabel <- target_column
  if (is_expert && (winDialog("yesno", "Hotite vvesti zagolovok?") == "YES")) {
    title <- winDialogString("Vvedite zagolovok", default = title)
    xlabel <- winDialogString("Vvedite X-label", default = xlabel)
    ylabel <- winDialogString("Vvedite Y-label", default = ylabel)
  }
  with(data, {
    plot(data[[train_column]],data[[target_column]], main=title, xlab=xlabel, ylab=ylabel)
    abline(fit)
    export_block(is_expert)
    })
}

regression_block <- function(data, is_expert) {
  target_column <- columns_selection_block(data, "Vvedite nazvanie kolonki, kotoruyu hotite predskazyvar'")
  if (!is_expert || (winDialog('yesno', 'Binarnaya regressia?') == 'YES')) {
    train_column <- columns_selection_block(data, 'Vvedite nazvanie kolonki dlya predskazaniya')
    fit <- linear_regression(data, train_column, target_column)
    plot_regression(data, fit, train_column, target_column, is_expert)
  } else {
    i <- 0
    train_columns <- c()
    while((i == 0) || (winDialog('yesno', 'Hotite vvesti eshe odnu kolnku dlya obucheniya?') == "YES")) {
      new_column <- columns_selection_block(data, 'Vvedite nazvanie kolonki dlya predskazaniya')
      train_columns <- append(train_columns, new_column)
      i <- i + 1
    }
    fit <- linear_regression(data, train_columns, target_column)
  }
  regression_export_block(fit, is_expert)
}

clusters_vizualization_block <- function(data, clusters, is_expert) {
  columns <- c(colnames(data))
  if (length(columns) != 2) print("Dlya vizualizacii dolzhno byt' 2 kolonki!")
  else {
    first_component <- columns[0]
    second_component <- columns[1]
    title <- 'Clusterization'
    xlabel <- first_component
    ylabel <- second_component
    if (is_expert && (winDialog('yesno', "Vy hotite pereopredelit' podpisi?") == "YES")) {
      title <- winDialogString('Vvedite nazvanie zagolovka: ', default = title)
      xlabel <- winDialogString("Vvedite podpis' k X: ", default = xlabel)
      ylabel <- winDialogString("Vvedite podpis' k Y: ", default = ylabel)
    }
    plot(data, col = clusters$cluster, xlab=xlabel, ylab=ylabel, main=title)    
  }
}

cluster_block <- function(data, is_expert) {
  clusters_cnt <- strtoi(winDialogString('Vvedite chislo clustesov: ', default = '2'))
  columns <- c()
  if (is_expert) {
    i <- 0
    while((i==0) || (winDialog('yesno', 'Vy hotite vvesti esche odin priznak dlya clusterizacii?') == "YES")) {
      i <- i + 1
      new_column <- columns_selection_block(data, 'Vyberite noviy priznak')
      columns <- append(columns, new_column)
    }
  } else {
    first_column <- columns_selection_block(data, 'Vyberite perviy priznak dlya clusterizacii')
    second_column <- columns_selection_block(data, 'Vyberite vtoroy priznak dlya clusterizacii')
    columns <- c(first_column, second_column)
  }
  clusters <- clusterize(data, columns, clusters_cnt)
  if (winDialog('yesno', "Vy hotite visualizirovat' clusterizaciyu ?") == "YES") {
    if (is_expert) {
      first_column <- columns_selection_block(data, 'Vyberite pervuyu glavnuyu komponentu dlya vizualizacii: ')
      second_column <- columns_selection_block(data, 'Vyberite vtoruyu glavnuyu komponentu dlya vizualizacii: ')
    }
    clusters_vizualization_block(data[, c(first_column, second_column)], clusters, is_expert)
    export_block(is_expert)
  } 
}