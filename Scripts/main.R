# clear working space

source('./Scripts/config.R')

source('./Scripts/paths.R', echo=F)
source(menu_package, echo=F)
source(data_analysis_package, echo=F)
source(io_package, echo=F)
source(visualization_package, echo=F)

data = start_menu()
main_menu(data)