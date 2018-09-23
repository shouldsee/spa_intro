##### Function to remove "knitr::" statements from source script
# buf <- paste0(readLines(fname), collapse ="\n")
# # pattern <-"\nknitr::(?s:.)+?\n\)\n"
# pattern <-"\nknitr::(?s:.)+?\n\\)\n"
# buf <- gsub(pattern,"",buf, perl = T)
# cat(buf,file = fname)
