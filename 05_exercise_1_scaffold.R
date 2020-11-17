# Import data
x = mtcars$gear

# With loops
func1 = function(x){
  unique_values <- sort(unique(x))
  result <- rep(NA,length(unique_values))
  for (i in 1:length(result)) {
    result[i] = length(x[x==unique(x)[i]])
  }
  gear_number = sort(unique_values)
  df = data.frame(gear_number,result)
  return(df)
}
func1(mtcars$gear)

# Without loops
func2 = function(x){
  unique_values <- sort(unique(x))
  result <- rep(NA,length(unique_values))
  result = sapply(1:length(unique_values), function(i) {length(x[x==unique(x)[i]])})
  gear_number = sort(unique_values)
  df = data.frame(gear_number,result)
  return(df)
}
func2(mtcars$gear)

# Without unique() but with loops
func3 = function(x){
value_check = rep(NA,length(x))
  for (i in 1:length(x)) {
    if (!(x[i] %in% value_check)) {
      value_check[i] = x[i]
    }
  }
  unique_values = sort(value_check[!is.na(value_check)])
  result <- rep(NA,length(unique_values))
  for (i in 1:length(result)) {
    result[i] = length(x[x==unique(x)[i]])
  }
  gear_number = sort(unique_values)
  df = data.frame(gear_number,result)
  return(df)
}
func3(mtcars$gear)

# Without unique() and without loops
func4 = function(x){
  value_check = rep(NA,length(x))
  for (i in 1:length(x)) {
    if (!(x[i] %in% value_check)) {
      value_check[i] = x[i]
    }
  }
  unique_values = sort(value_check[!is.na(value_check)])
  result <- rep(NA,length(unique_values))
  result = sapply(1:length(unique_values), function(i) {length(x[x==unique(x)[i]])})
  gear_number = sort(unique_values)
  df = data.frame(gear_number,result)
  return(df)
}
func4(mtcars$gear)




















