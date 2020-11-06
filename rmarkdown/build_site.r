read_yml <- function(filename) {
  tryCatch(
    {
      yml <- yaml::read_yaml(filename)
    },
    error = function(e) {
      stop("File does not exist.")
    }
  )

  navbar_info <- list()
  
  tryCatch(
    {
      navbar_info$title <- yml$navbar$title
      navbar_info$pages <- yml$navbar$left
    }, 
    error = function(e) {
      stop("Required fields missing in yaml file.")
    },
    finally = return(navbar_info)
  )
}

build_navbar <- function(navbar_info) {
  tags <- c(
    '
    <div class="navbar navbar-default  navbar-fixed-top" role="navigation">
      <div class="container">
        <div class="navbar-header">
          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-controls="nav-primary" aria-label="Display main menu">
            Main menu
          </button>
          <a class="navbar-brand" href="index.html">',
    navbar_info$title, 
    '     </a>
            </div>
              <div id="navbar" class="navbar-collapse collapse">
                <ul class="nav navbar-nav">
    '
  )
  
  tryCatch (
    {
    nav_tags <- 
      sapply(navbar_info$pages, function(info) {
        paste(
          c(
            '                  <li>\n',
            '                    <a href="',
            info["href"],
            '">',
            info["text"],
            '  </a>\n',
            '                  </li>\n'
          ),
          collapse = ""
        )
      })
    },
    error = function(e) {
      stop("Enter valid navbar info")
    }
  )
  
  close_tags <- '                </ul>
              </div><!--/.nav-collapse -->
            </div><!--/.container -->
          </div><!--/.navbar -->
  '
  
  paste(c(tags, nav_tags, close_tags), collapse = "")
}

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
} 

navbar_info <- read_yml("rmarkdown/_site.yml")
navbar_page <- build_navbar(navbar_info)
save_navbar(navbar_page, "rmarkdown")
rmarkdown::clean_site("rmarkdown")
rmarkdown::render_site("rmarkdown")