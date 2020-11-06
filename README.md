# Coding in Analysis and Research Survey

The contents of the rmarkdown folder can be used to generate a template site to present results from the second wave of the Coding in Analysis and Research Survey (CARS). 

## Adding/modifying content

The files needed to render the site can be found in the rmarkdown/ folder. To amend pages, make modifications to the .rmd files. To add pages, create new .rmd files and place them in the rmarkdown/ folder. For example, to create a new page called "page 2" add the file page2.rmd. 

To make changes to the navigation bar, edit the _site.yml file. When adding new pages, make sure to refer to them in the .yml file. For example: 

```yaml 
navbar:
  title: "My Website"
  left:
    - text: "Home"
      href: index.html
    - text: "Page 1"
      href: page1.html
```

The text for each link does not have to be the exact title as the page. 

Style changes can be made by amending style.css. 

## Rendering the site

The site can be rendered from R or RStudio. Ensure you have RMarkdown and yaml (the R package) installed. 

To render the site, run the build_site.r script. The script will use the R project root as the working directory.
