## Start version controlling it
usethis::use_git()

## Use the gh-pages branch in order for
## GitHub pages https://pages.github.com/ to
## host our website.
usethis::git_default_branch_rename(to = "gh-pages")

## Create a .nojekyll file
writeLines("", here::here(".nojekyll"))

## Share it via GitHub with the world
usethis::use_github()
