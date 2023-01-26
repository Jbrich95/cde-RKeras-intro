require(keras)
require(tensorflow)

version <- "3.8.10"
#Download and install python 3.8.10 from https://www.python.org/downloads/macos/
#Create a virtual envionment 'myenv' with Python 3.8.10. Install tensorflow  within this environment.
reticulate::virtualenv_create(envname = 'myenv',
                              python="/usr/local/bin/python3",
                              version=version)

path<- paste0(reticulate::virtualenv_root(),"/myenv/bin/python")
Sys.setenv(RETICULATE_PYTHON = path) #Set Python interpreter to that installed in myenv

reticulate::use_virtualenv("myenv", required = T)
reticulate::virtualenv_install("myenv",packages = "tensorflow") #Install latest version of tensorflow in virtual environment
install_keras(method = c("virtualenv"), envname = "myenv") #Install keras


is_keras_available() #Check if keras is available
