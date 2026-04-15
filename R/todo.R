#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(argparse)
})

TASK_FILE <- "test_list.txt"

# remove spaces and empty lines
clean_todo_list <- function() {
  todo_list_file <- readLines(TASK_FILE, warn = FALSE)

  todo_list_file <- trimws(todo_list_file)
  todo_list_file <- todo_list_file[todo_list_file != ""]

  return(todo_list_file)
}

# counts how many tasks are in the file
count_todo_list <- function() {
  todo_list_file <- clean_todo_list()
  task_count <- length(todo_list_file)

  return(task_count)
}

add_task <- function(task) {
  cat(task, file = TASK_FILE, append = TRUE, sep = "\n") # adds 'task' to the file
  #  cat("You have added a new task, you have", paste(count_todo_list()), "tasks to do!", "\n")
}

list_tasks <- function() {
  todo_list_file <- clean_todo_list()

  if (length(todo_list_file) == 0) { # checks if there are tasks in the file
    output <- "Your todo list is empty, it's time to relax!"
    cat(output, "\n")
  } else {
    line_numbers <- 1:length(todo_list_file) # make sequence of numbers the same length as the list
    numbered_list <- paste(line_numbers, todo_list_file, sep = ". ") # combine numbers and list

    output <- paste(numbered_list, collapse = "\n") # for the test

    # for user
    #  cat("You have", count_todo_list(), "tasks. Here is your todo list:\n")
    #  cat(numbered_list, sep = "\n")
  }
  return(output) # for test
}

remove_task <- function(index) {
  todo_list_file <- clean_todo_list()

  # Check if the index is valid
  if (index > length(todo_list_file) | index <= 0) {
    stop("Error: Task number", index, "does not exist.\n")
  } else {
    # Remove the task using negative indexing
    updated_list <- todo_list_file[-index]

    # updates file
    writeLines(updated_list, TASK_FILE)

    #    cat("Task", index, "removed successfully!\n")
    #    cat("You have", count_todo_list(), "tasks left to go!")
  }
}

main <- function(args) {
  if (!is.null(args$add)) {
    add_task(args$add)
  } else if (args$list) {
    tasks <- list_tasks()
    print(tasks)
  } else if (!is.null(args$remove)) {
    remove_task(as.numeric(args$remove))
  } else {
    print("Use --help to get help on using this program")
  }
}


if (sys.nframe() == 0) {
  # main program, called via Rscript
  parser <- ArgumentParser(description = "Command-line Todo List")
  parser$add_argument("-a", "--add",
    help = "Add a new task"
  )
  parser$add_argument("-l", "--list",
    action = "store_true",
    help = "List all tasks"
  )
  parser$add_argument("-r", "--remove",
    help = "Remove a task by index"
  )

  args <- parser$parse_args()
  main(args)
}
