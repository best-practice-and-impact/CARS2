# Contributing

When contributing to this repository, please first discuss the change you wish
to make via issue, email, or any other method with the owners before making a change.

## Pull/merge request process

1. Branch from the `dev` branch ONLY. If you are implementing a feature name it
`feature/name_of_feature` such as `feature/rap_opinions_table`, if you are implementing a bugfix name it
`bug/issue_name`, such as `big/005`. The `005` refers to the GitHub issue which has additional details.
2. Update the README.md and other documentation with details of major changes
to the interface, this includes new environment variables, useful file
locations or function arguments/usage.
3. Once you are ready for review please open a pull/merge request to the
`dev` branch.
4. You may merge the Pull/Merge Request in once you have the sign-off of two
maintainers.
5. If you are merging `dev` to `master`, you must increment the version number
in the VERSION file to the new version that this Pull/Merge Request would
represent. The versioning scheme we use is [SemVer](http://semver.org/).


## Code style

For the most part, follow the guidelines from [R packages](https://r-pkgs.org/) by Hadley Wickham. You can differ but you will need to explain the reason in the PR. The unit tests are performed with testthat, the documentation is built with roxygen2 and the package follows the standard structure. 

* Please follow the [Hadley style guide](http://adv-r.had.co.nz/Style.html) for writing code.
* All functions show be tested and documented.
* R functions are grouped according to logic. 
* The site is built from `.rmd` files in rmarkdown folder. Try to keep code to a minimal in markdown files. It is preferred to write any logic in the carsurvey2 package and call this, as it is harder to debug and test code in rmarkdown.

## Review process

When you have written code that you would like to add to the project you should submit it for a review, this is done using a GitHub Pull Request (PR). The code should be on an individual branch and be merged into the dev branch

### Guide for submitting code
To make a pull request as clear as possible, it should include an appropriate checklist of relevant information about the proposed code changes (instead of a single line summary), such as:

* How a bug was fixed and a description for the solution
* A description or summary of a new feature.
* The unit test cases covered.
* Whether this code breaks existing functionalities.

### Guide for Guide for Reviewers

First- Is the PR request itself sufficient, (look at Guide for submitting code). If not then ask the author to meet the requirements as specified above BEFORE starting the review.

* Does the code follow the [Hadley style guide](http://adv-r.had.co.nz/Style.html)?
* Are there improvements that could be made to the code style?
* Is there sufficient testing and documentation? 
* Is there any disclosure issues? (Check that API keys have not be accidentally uploaded)
* Is there code duplication in the package that should be reduced?
* Is the documentation (installation instructions/vignettes/examples/demos) clear and sufficient? 

If you have any issues or questions then ask the author for further details. Use the GitHub PR process (rather than email/Skype UNLESS disclosure is an issue) as this keeps a public record of the reasons for decisions. 

