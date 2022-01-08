# https://support.rstudio.com/hc/en-us/articles/206382178?version=2021.09.0%2B351&mode=desktop
# https://www.rstudio.com/blog/rstudio-1-3-preview-configuration/

edit_rstudio_config_windows <- function() {

  usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/addins.json")
  usethis::edit_file(
    "~/AppData/Roaming/RStudio/keybindings/rstudio_bindings.json"
  )
  usethis::edit_file(
    "~/AppData/Roaming/RStudio/keybindings/editor_bindings.json"
  )
}

set_rstudio_keybindings_windows <- function(overwrite = FALSE) {

  keys_dir <- fs::dir_create("~/AppData/Roaming/RStudio/keybindings/")

  fs::file_copy(
    path = fs::path_package("efun", "templates/addins.json"),
    new_path = fs::path(keys_dir, "addins.json"),
    overwrite = overwrite
  )

  fs::file_copy(
    path = fs::path_package("efun", "templates/rstudio_bindings.json"),
    new_path = fs::path(keys_dir, "rstudio_bindings.json"),
    overwrite = overwrite
  )

  # TODO: editor_bindings.json
  # usethis::edit_file("~/AppData/Roaming/RStudio/keybindings/editor_bindings.json")

}


set_rstudio_prefs_windows <- function(overwrite = FALSE) {

  prefs_dir <- fs::dir_create("~/AppData/Roaming/RStudio/")

  fs::file_copy(
    path = fs::path_package("efun", "templates/rstudio-prefs.json"),
    new_path = fs::path(prefs_dir, "rstudio-prefs.json"),
    overwrite = overwrite
  )

}


