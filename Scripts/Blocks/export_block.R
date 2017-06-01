source('./Scripts/config.R', echo=F)

export_block <- function(is_expert) {
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
    if (is_expert)
      if (winDialog("yesno", gettextf("Vy hotite pereopredelit' tekushuyu papku sohraneniya graphikov (%s) ?", export_dir)) == "YES") export_dir <- choose.dir()
    dev.copy(image, paste(export_dir, '/', name, '.', image_type, sep=''))
    dev.off()
  }
}