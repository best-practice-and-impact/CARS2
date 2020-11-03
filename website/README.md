# CARS wave 2 website template

The contents of this folder can be used to generate a template site to present results from the second wave of the Coding in Analysis and Research Survey (CARS). 

## Adding/modifying content

The files needed to render the site can be found in the rmarkdown/ folder. To amend pages, make modifications to the .rmd files. To add pages, create new .rmd files and place them in the rmarkdown/ folder. For example, to create a new page called "page 2" add the file page2.rmd. 

To make changes to the navigation bar, edit the _navbar.html file. When adding new pages, make sure to add links to those pages in the navigation bar html. For example, to add the link to page2 in the navigation bar, add the follwing lines:

```html
<li>
  <a href="page1.html">Page 1</a>
</li>
```

This should be inside the navigation bar div. Example:

```html
<div id="navbar" class="navbar-collapse collapse">
  <ul class="nav navbar-nav">
    <li>
      <a href="index.html">Home</a>
    </li>
    <li>
      <a href="page1.html">Page 1</a>
    </li>
  </ul>
</div><!--/.nav-collapse -->
```

Style changes can be made by amending style.css. 

## Rendering the site

The site can be rendered from R. Ensure you have RMarkdown installed. 

To render the site, first set the working directory to the rmarkdown folder. Run the following command to remove any previous versions in the doc folder:

```r
rmarkdown::clean_site()
```

Run the following command to render the site:

```r
rmarkdown::render_site()
```

The site will render in the docs/ folder. To view the site open index.html in a web browser.