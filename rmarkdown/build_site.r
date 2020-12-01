save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
} 

navbar_info <- carsurvey2::read_site_yml("rmarkdown/_site.yml")
navbar_page <- carsurvey2::build_navbar(navbar_info)
save_navbar(navbar_page, "rmarkdown")
rmarkdown::clean_site("rmarkdown")
testvar = "TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST "
rmarkdown::render_site("rmarkdown")

