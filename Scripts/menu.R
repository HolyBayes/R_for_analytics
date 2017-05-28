source('./Scripts/Blocks/io_block.R', echo = F)

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
        edit(data)
      },
      # Exit
      break
    )
  }
  return(data)
}

main_menu <- function(data){
  menu_fields <- c('Instructsiya', "Obnovit' dannye", 
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

analysis_menu <- function(data){
  menu_fields <- c('Data analysis', 'Data visualization', 'Exit')
  while(T) {
    user_action <- menu(menu_fields, graphics = T, title = 'Vyberite deystvie')
    switch(
      user_action,
      
      break     
           )
  }
}







