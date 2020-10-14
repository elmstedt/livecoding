my_var <- function(x, sample = TRUE) {
  sum((x - mean(x))^2) / (length(x)  - sample)
}
set.seed(123)
x <- sample(100, 20, TRUE)
var(x)
my_var(x, sample = TRUE)
my_var(x, sample = FALSE)

# Raw moments
mean(x^1)
n <- 4
set.seed(123)
x <- rnorm(20)
get_raw_moment <- function(x, n = 1) {
  vapply(seq_len(n),
         function(x, k) {
           mean(x^k)
         },
         numeric(1),
         x = x)
}
m <- get_raw_moment(x, 5)

central_moment <- function(x, k) {
  mean((x - mean(x))^k)
}
central_moment(x, 2)
get_raw_moment(x, 1:3)
mean((x - mean(x))^2)
my_var(x, sample = FALSE)
raw_m <- m
central_m <- raw_m - raw_m[1]
central_m[2] <- raw_m[2] - raw_m[1]^2
central_m[3] <- raw_m[3] - 3 * raw_m[1] * raw_m[2] + 2 * raw_m[1]^3
central_m[4] <- 
  raw_m[4] -
  4 * raw_m[1] * raw_m[3] + 
  6 * raw_m[1]^2 * raw_m[2] - 
  3 * raw_m[1]^4
central_m
# choose(6, 4)
# choose(6, 4)
# 6!/(4! * (6 - 4)!)
n <- 6
my_factorial <- function(n) {
  vapply(n,
         function(k) {
           prod(seq_len(k))
         },
         numeric(1))
}
my_factorial(1:10)
# choose(6, 4)
# 6!/(4! * (6 - 4)!)
# n! / (k! * (n - k)!)
my_choose <- function(n, k) {
  my_factorial(n) / (my_factorial(k) * my_factorial(n - k))
}
my_choose(6, 0:6)

pascal <- function(n) {
  my_choose(n, seq(0, length.out = n + 1))
}
pascal(0)
pascal(1)
pascal(2)
pascal(3)
pascal(5)
alt <- c(1, -1)
n <- 4
rep(alt, length.out = n + 1) * pascal(n)

pascal <- function(n, alt = FALSE) {
  a <- rep(c(1, -1)^alt, length.out = n + 1)
  a * my_choose(n, seq(0, length.out = n + 1))
}


pascal_v <- function(n, alt = FALSE) {
  vapply(seq(0, length.out = n + 1),
         function (m) {
           a <- rep(c(1, -1)^alt, length.out = m + 1)
           c(rep(0, n - m), a * my_choose(m, seq(0, length.out = m + 1)))
         },
         numeric(n + 1)
         )
  
}
t(pascal_v(10))
pascal(5, TRUE)
pascal(5)

central_m[5] <- raw_m[5] - 5 * raw_m[1] * raw_m[4] + 10 * raw_m[1]^2 * raw_m[3] - 10 * raw_m[1]^3 * raw_m[2] + 5 * raw_m[1]^4 * raw_m[1] - raw_m[1]^5 * 1

central_m[5] <-
   1 * raw_m[1]^0 * raw_m[5] - 
   5 * raw_m[1]^1 * raw_m[4] +
  10 * raw_m[1]^2 * raw_m[3] -
  10 * raw_m[1]^3 * raw_m[2] +
   5 * raw_m[1]^4 * raw_m[1] -
   1 * raw_m[1]^5 * 1

central_m[5] <- sum(pascal(5, TRUE) * raw_m[1]^seq(0, length(raw_m)) * c(rev(raw_m), 1))

raw_m <- get_raw_moment(x, 8)
un <- function(raw_m, n) {
  sum(pascal(n, TRUE) * raw_m[1]^seq(0, n) * c(rev(raw_m[seq_len(n)]), 1))
}
un(raw_m, 1)
central_m
un(raw_m, 1:5)
my_var(x, sample = FALSE)

centralize_raw_moments <- function(raw_m) {
  vapply(seq_along(raw_m),
         function(m, n) {
           sum(pascal(n, TRUE) * m[1]^seq(0, n) * c(rev(m[seq_len(n)]), 1))
         },
         numeric(1),
         m = raw_m)
}
raw_m <- get_raw_moment(x, 5)
centralize_raw_moments(raw_m)
central_m

get_central_moments <- function(x, k) {
  vapply(seq_len(k),
         function(x, k) {
           mean((x - mean(x))^k)
         },
         numeric(1),
         x = x)
}
round(get_central_moments(x, 5), 16)
centralize_raw_moments(raw_m)
central_m

skew <- function(x) {
  sqrt(length(x)) * sum((x - mean(x))^3) / sum((x - mean(x))^2)^(3/2)
}
skew(x)
central_m[3] / central_m[2]^(3/2)

standardize_moments <- function(m) { # central moments
  m / sqrt(m[2])^seq_along(m)
}
standard_m <- standardize_moments(central_m)

kurt <- function(x) {
  mean((x - mean(x))^4) / mean((x - mean(x))^2)^2 - 3
}
kurt(x) - (standard_m[4] - 3)

get_standard_moments <- function(x, n = 2) {
  raw_m <- get_raw_moment(x, n)
  central_m <- centralize_raw_moments(raw_m)
  standardize_moments(central_m)
}
get_standard_moments(x, 8)


get_std_moments <- function(x, k) {
  m <- vapply(seq_len(k),
         function(x, k) {
           mean((x - mean(x))^k)
         },
         numeric(1),
         x = x)
  m / sqrt(m[2])^seq_along(m)  
}
get_std_moments(x, 8)
