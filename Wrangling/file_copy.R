# see working directory
getwd()

# change your working directory
#setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)


#use URL to download a file
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
download.file(url,"vimal.dat")
getwd()
#use read_lines to check the content
read_lines('vimal.dat',n_max=3)
read_csv('vimal_dat',col_names=FALSE)