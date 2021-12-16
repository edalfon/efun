
#' Set usethis description defaults, just to make it easy to set this up
#' in a new machine
#' @noRd
set_e_description_defaults <- function() {

  options(
    usethis.description = list(
      `Authors@R` = 'person(
        given = "Eduardo",
        family = "Alfonso-Sierra",
        email = "edalfon@gmail.com",
        role = c("aut", "cre", "cph"),
        comment = c(ORCID = "0000-0001-6430-5135"))',
      License = "MIT + file LICENSE",
      Language =  "en"
    )
  )

  #options(usethis.full_name = "Eduardo Alfonso-Sierra")
}


#' usethis boilerplate
#' @noRd
usethis_boilerplate <- function() {

  set_e_description_defaults()

  # usethis::use_description()
  # usethis::use_tidy_description()

  usethis::use_package_doc()
  usethis::use_pipe()

  usethis::use_readme_rmd()

  usethis::use_testthat()
  usethis::use_spell_check()
  usethis::use_test(basename(usethis::proj_get()))

  usethis::use_vignette(basename(usethis::proj_get()))

  usethis::use_pkgdown()

  usethis::use_mit_license()

  #usethis::use_git()


}
