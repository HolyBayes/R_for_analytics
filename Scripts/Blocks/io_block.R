read_block <- function(){
  filepath <- file.choose()
  extension <- menu(c(".csv", ".tsv", ".xlsx"), graphics = T, title = 'Vyberite tip faila')
  switch(extension, 
         df <- read.csv(filepath),
         df <- read.csv(filepath, sep='\t'),
         df <- read.xlsx(filepath)
  )
  return(df)
}

save_block <- function(data){
  filepath <- winDialogString("Vvedite put'", default = '')
  extension <- menu(c(".csv", ".tsv", ".xlsx"), graphics = T, title = 'Vyberite tip faila')
  switch(extension, 
         write.csv(data, file = filepath),
         write.csv(data, file = filepath, sep='\t'),
         write.xlsx(data, file = filepath)
  )
}