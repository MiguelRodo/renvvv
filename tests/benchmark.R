# Benchmark test
start_time <- Sys.time()
installed_now <- c()
for (i in 1:100) {
  installed_now <- rownames(installed.packages())
}
end_time <- Sys.time()
print(paste("Time with rownames(installed.packages()):", end_time - start_time))

start_time <- Sys.time()
installed_now <- c()
for (i in 1:100) {
  installed_now <- c(installed_now, as.character(i))
}
end_time <- Sys.time()
print(paste("Time with c(installed_now, x):", end_time - start_time))
