The sample size calculation functions are derived from the work of Foody, G. M. (2009). Sample size determination for image classification accuracy assessment and comparison. International Journal of Remote Sensing, 30(20), 5273-5291. https://doi.org/10.1080/01431160903130937

Each of the three sample size caclulation functions relates to a particular approach. genSample1() is an implementation of the typical sample size calcuation, using only the target accuracy (p0), the half-width of the Confidence Interval (h), and the tolerance for Type I error (alpha).

genSample2() is used when it is desired to be able to reliably test for a result being a certain distance from the target accuracy. It requires the minimum detectable difference (min_diff), and also that tolerance for Type II errors be specified (beta).

genSample3() is used when a confidence interval is of more interest than testing against a target accuracy. See eq. 22-25. This function requires the specification of the target or expected accuracy (p0), alpha and beta, and the difference in accuracy that can be detected with a power of 1 - beta (d).

The function contCorrect() performs a continuity correction on sample size estimates. A continuity correction may be necessary when using equations that assume a continious distribution (samples are discrete), which results in a slight underestimate of the necessary sample size. It is most appropriate to apply to the estimate of sample size produced by genSample2().

The function powerEst() can be used to estimate the power of a sample size, given the minimum difference to be detected (min_diff), the expected accuracy (p0), and alpha.

Function to calculate optimal sample size for an expected confusion matrix Taken from the Excel spreadsheet provided in: Wagner, J. E., & Stehman, S. V. (2015). Optimizing sample size allocation to strata for estimating area and map accuracy. Remote Sensing of Environment, 168, 126–133. https://doi.org/10.1016/j.rse.2015.06.027

Test data and a function to produce a confusion matrix from two factors are also included. 