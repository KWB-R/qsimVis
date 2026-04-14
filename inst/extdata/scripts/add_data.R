# 1. write data and save as .rda file in the package/data folder
# --> can be done by usethis::use_data()
# (the filename should be the same as the variable name)

# 2. add the variable to the global variables of the package in the
# package/R/zzz.R file to avoid the note about missing global bindings by the
# devtool check

# 3. Document the dataset in the file data_properties_names.R in the package/R
# folder
# NULL can be assigned to the documentation to avoid the devtools warning
# message "Variables with usage in documentation ... but not in code"


var # define variable
usethis::use_data(var, overwrite = TRUE) # save variable as rda

