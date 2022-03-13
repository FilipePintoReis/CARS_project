
#library(reticulate, quietly = TRUE)

# This file imports all python functions required by this package
# The file name starts with a 1 so that it is the first in alphabetical order.
# This way, the python functions are loaded to memory before any other script runs.

for (file_name in list.files(paste('inst','python', sep = .Platform$file.sep))){
    source_python(paste('inst','python', file_name, sep = .Platform$file.sep))
}

