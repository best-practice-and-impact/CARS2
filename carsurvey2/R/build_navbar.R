#'@title build rmarkdown site navbar
#'
#'@description Build rmarkdown site navbar using arguments from the site yaml file (use the output from read_site_yml() as the navbar_info argument)
#'
#'@param navbar_info list of arguments - use output from read_site_yml()
#'
#'@return navbar html as string
#'
#'@export

build_navbar <- function(navbar_info) {
  
  if (!is.list(navbar_info)) {
    stop("Unexpected input - navbar_info should be a list")
  }
  
  # Navbar header HTML
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
    '</a>
          </div>
            <div id="navbar" class="navbar-collapse collapse">
              <ul class="nav navbar-nav">
    '
  )
    
  # Navbar main contents HTML
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
   
  # Closing HTML tags 
  close_tags <- '                </ul>
            </div><!--/.nav-collapse -->
          </div><!--/.container -->
        </div><!--/.navbar -->
  '
    
  paste(c(tags, nav_tags, close_tags), collapse = "")
  
}