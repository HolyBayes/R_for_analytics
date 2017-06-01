plot_boxplot <- function(data, horizontal, first_column, second_column, title = '') {
  columns <- as.formula(paste(first_column, second_column, sep="~"))
  with(data, boxplot(columns, data = data, main = title, horizontal = horizontal))
}

plot_barplot = function(data, horizontal, column, xlabel, ylabel, title){
  with(data,
       {counts <- table(data[[column]])
       barplot(counts, main = title, xlab = xlabel, ylab = ylabel, horiz = horizontal)
       })
}

plot_hist = function(data, column, xlabel, title){
  with(data, hist(data[[column]], xlab=xlabel, main=title))
}

plot_piechart = function(data, column, title){
  with(data, 
       pie(table(data[[column]]), 
           labels = paste(paste(names(table(data[[column]])), 
                                round(table(data[[column]])/nrow(data)*100, 2), 
                                sep=' - '),'%',sep=''), main=title))
}

plot_scatterplot = function(data, first_column, second_column, xlabel, ylabel, title){
  with(data, { plot(data[[first_column]], data[[second_column]], main = title , xlab = xlabel, ylab = ylabel)})
}

