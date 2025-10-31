## The primary objective of this task was to develop a method for identifying possible cheater teachers within the dataset. Manually identifying cheaters would be both inaccurate and time-consuming due to the magnitude of the data. Therefore, we employed statistical techniques using R to automate this process.

### The code we have used is R-based 

## Data cleaning and classification

### Read ChildScore.csv and separate teachers as cheaters or non-cheaters using CheatID.

### Used mutate() and ifelse() to replace cheater teachers’ student scores with NA.

### Computed average scores per student with rowMeans().

## Residual calculation

### Reshaped data to long format using gather().

### Calculated residuals = (actual score − average score).

## MLE model

### Applied Maximum Likelihood Estimation to estimate parameters (mean μ, sd σ) for cheater vs. non-cheater distributions.

### Assumed both groups follow normal distributions.
