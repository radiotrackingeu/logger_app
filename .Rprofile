local({
  repos <- list("CRAN" = "https://cloud.r-project.org")
  
  options(repos = repos, download.file.method = 'curl')
  
  glob_deps <- c("git2r", "devtools", "usethis", "renv")
  .First <- function(){
    utils::install.packages("devtools", repos = repos)
    utils::install.packages("git2r", repos = repos)
    utils::install.packages("usethis", repos = repos)
    utils::install.packages("renv", repos = repos)    
    
    library(devtools)
    library(git2r)
    library(usethis)
    library(renv)
  }
  
  options(remotes.git_credentials = git2r::cred_user_pass(Sys.getenv("GITLAB_USER"), Sys.getenv("GITLAB_TOKEN")))
  
  source("renv/activate.R")
  
  print("radiotracking.eu logger app")
})

