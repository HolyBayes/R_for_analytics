columns_selection_block = function(data, title){
  columns <- c(colnames(data))
  user_action <- menu(columns, graphics=T, title=title)
  return(columns[user_action])
}