calculate_percent_match_tolerance <- function(rater1, rater2) {
  # Initialize a vector to store results
  match_results <- logical(length(rater1))
  
  # Check if each value in rater 1 has a match within tolerance in rater 2
  for (i in 1:length(rater1)) {
    value_to_match <- rater1[i]
    
    # Check if there is a match within ±1 row (±1 second) in rater 2
    if (value_to_match %in% rater2[(i - 1):(i + 1)]) {
      match_results[i] <- TRUE
    }
  }
  
  # Calculate the percentage of matches
  percent_match <- sum(match_results) / length(match_results) * 100
  
  return(percent_match)
}
