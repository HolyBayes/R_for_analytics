source('./Scripts/Blocks/io_block.R', echo = F)
source('./Scripts/Blocks/data_analysis_block.R', echo = F)

not_defined <- 'Not defined'

start_menu <- function(){
  data <- not_defined
  menu_fields <- c('Instructsiya', "Zagruzit' dannye", 'Viyti iz programmy')
  while (data == not_defined) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite deystvie')
    switch(
      user_action,
      # Instruction
      {
        instruction <- paste0(getwd(), insruction_path)
        shell.exec(instruction)
      },
      # Data upload block
      {
        data <- read_block()
      },
      # Exit
      break
    )
  }
  return(data)
}

main_menu <- function(data){
  menu_fields <- c('Instructsiya', 'Menu analyza', "Obnovit' dannye", 
                   "Exportirovat' dannye", "Redactirovat' dannye", 'Viyti iz programmy')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite deystvie')
    switch(
      user_action,
      # Instruction
      {
        instruction <- paste0(getwd(), insruction_path)
        shell.exec(instruction)
      },
      # Data analysis block
      data_analysis_block(data),
      # Data update block
      data <- read_block(),
      # Data export block
      {
        if (data == not_defined) print('Data not defined')
        else save_block(data)
      },
      # Data edit block
      {
        if (data == not_defined) print('Data not defined')
        else edit(data)
      },
      # Exit
      break
    )
  }
}





