source('./Scripts/Blocks/visualization_block.R')
source('./Scripts/Blocks/ml_block.R')

data_analysis_block <- function(data) {
  menu_fields <- c('Vizualization', 'Analyz dannyx', 'Otmena')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite deystvie')
    switch(user_action,
      vizualization_block(data),
      ml_block(data),
      break       
    )    
  }
}