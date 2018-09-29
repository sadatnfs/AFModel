
packageStartupMessage('Compiling model (this will take a couple minutes...)')

dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

packageStartupMessage(paste('Writing model to:', dest))
packageStartupMessage(paste('Compiling using binary:', R.home('bin')))

model.src <- file.path(R_PACKAGE_SOURCE, 'cppmodel', 'AFModel.cpp')
model.o <- file.path(R_PACKAGE_SOURCE, 'cppmodel', 'AFModel')

TMB::compile(model.src)
# dyn.load(TMB::dynlib(model.o))


packageStartupMessage('------ Model successfully compiled!')
packageStartupMessage('You can ignore any compiler warnings above.')
