Sys.setenv(R_INSTALL_STAGED = FALSE)

source("renv/activate.R")

if(!require("rteu")){
  devtools::install_git( 
    url = "https://git.plecso.de/R/rteu_package.git",
    branch = "rteu_pkg_refactoring",
    credentials = git2r::cred_user_pass(Sys.getenv("GITLAB_USER"), Sys.getenv("GITLAB_TOKEN"))
  )
}

print("radiotracking.eu")

