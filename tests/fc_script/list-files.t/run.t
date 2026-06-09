  $ frama-c-script list-files
  # Paths as seen by a makefile inside subdirectory '.frama-c':
  SRCS=\
  ../does_not_exist.c \
  ../main.c \
  ../main2.c \
  ../main3.c \
  
  warning: could not read file 'does_not_exist.c', mentioned in 'compile_commands.json'. Skipping check of 'main' function.
  
  # Possible definition of main function in the following file(s), as seen from '.frama-c':
  ../main.c
  ../main3.c
