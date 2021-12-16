# https://support.rstudio.com/hc/en-us/articles/206382178?version=2021.09.0%2B351&mode=desktop
edit_rstudio_config_windows <- function() {

  usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/addins.json")
  usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/rstudio_bindings.json")
  usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/editor_bindings.json")
}

set_rstudio_keybindings_windows <- function(overwrite = FALSE) {

  fs::file_copy(
    path = fs::path_package("efun", "templates/addins.json"),
    new_path = "~/AppData/Roaming/RStudio/keybindings/addins.json",
    overwrite = overwrite
  )

  fs::file_copy(
    path = fs::path_package("efun", "templates/rstudio_bindings.json"),
    new_path = "~/AppData/Roaming/RStudio/keybindings/rstudio_bindings.json",
    overwrite = overwrite
  )

  # TODO: editor_bindings.json
  # usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/editor_bindings.json")

}
