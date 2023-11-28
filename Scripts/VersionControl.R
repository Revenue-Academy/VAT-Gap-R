library(versions)

# Function similar to `pip freeze`
freeze <- function() {
  package_mat <- installed.packages()[,c("Package","Version")]
  requirement_lst <- apply(package_mat, 1, function(pp) {
    paste0(pp["Package"],"==",pp["Version"])
  })
  
  requirements <- unname(sort(unlist(requirement_lst)))
  (requirements)
}

# Use the function
req <- freeze()
print(req)
