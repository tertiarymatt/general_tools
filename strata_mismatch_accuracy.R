#' ---
#' title: "Functions for Area Estimation w/ Mismsatched Strata"
#' author: "MS Patterson, matthewpatterson@usda.gov; Paolo Arevalo"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' This script can be knit in R into a github formatted markdown file for easy
#' reading.

#' This script contains the functions required for the calculation of the
#' unbiased areas of map classes when the samples were produced using the
#' strata of a different map:  

#' 1. Function to calculate strata for any given pair of years  
#' 1. Calculate unbiased area proportions and variance of reference classes  
#' 1. Calculate standard error of unbiased area proportion  
#' 1. Calculate unbiased areas, their confidence interval and margin of error  
#' 
#' The functions in this script are based on Stehman S. V. 2014. 
#' *Estimating area and map accuracy for stratified random sampling when the
#'  strata are different from the map classes.* 
#'  Int. J. Remote Sens. 35: 4923-4939.  
#'
#' ###calcPropsAndVars  
#' Calculate proportions and variances for area and accuracies calculation per 
#' original strata for a given year/map.  
#' 
#' **orig_strata** Vector with numeric codes representing the original 
#' stratification of each sample.  
#' 
#' **ref_label** Vector with numeric codes representing the reference label 
#' for that year/map, for each sample.  
#' 
#' **map_label** Vector with numeric codes representing the map labels, for
#' each sample.  
#' 
#' **strata_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification and the second must 
#' have the total number of PIXELS of each class in that original strata map.  
#' 
#' **sample_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS of each class collected from that 
#' original strata map.  
#' 
#' **rfcodes** Vector with numeric values representing the reference codes
#' present in ALL of the periods.  
#' 
#' **Returns** List with: dataframe with proportion (mean) of reference labels 
#' present on each sample strata class (ref_prop) and dataframe with its 
#' variance (ref_var), dataframe with proportion (mean) of map labels present 
#' on each sample strata class (map_prop), and dataframe with its variance 
#' (map_var), dataframe with proportion (mean) of matching reference and map 
#' labels present on each sample strata class (map_and_ref_prop) and dataframe 
#' with its variance (map_and_ref_var), dataframe with proportion of all map 
#' labels equal to reference labels present in stratum (overall_acc_prop) and 
#' dataframe with its variance (overall_acc_var) and dataframes associated with 
#'  users's and producer's covariance (users_cov, producers_cov), vector of area 
#' proportions per class (class_prop).  

calcPropsAndVars <- function(orig_strata, ref_label, map_label, 
															 strata_totals, sample_totals, rfcodes){
	
	# Obtain unique values in the orig_strata field and get a sequence. We will 
	# use ALL codes even if they are not present for a year, in which case we 
	# would obtain values of 0. This facilitates all the other calculations
	str_codes <-  sort(unique(orig_strata))
	str_seq <-  seq_along(str_codes)
	
	# Get a sequence for the reference codes
	ref_seq <-  seq_along(rfcodes)
	
	# Initialize empty df for proportions per orig_strata, and for sample variance
	# per orig_strata
	ref_prop <- data.frame()
	ref_var <- data.frame()
	map_prop <- data.frame()
	map_var <- data.frame()
	map_and_ref_prop <- data.frame()
	map_and_ref_var <- data.frame()
	overall_acc_prop <- vector()
	overall_acc_var <- vector()
	users_cov <- data.frame()
	producers_cov <- data.frame()
	
	# Compare the fields, iterate over "orig_strata" and "ref_label" classes
	for (s in str_seq) {
		
		# Get location of rows for the current orig_strata
		str_bool <- orig_strata == str_codes[s]
		str_ind <- which(str_bool)
		
		for (r in ref_seq) {
			# Compare reference vs stratum and get TRUE or FALSE on each row
			ref_vs_str_bool <- str_bool & ref_label == rfcodes[r]
			# Get row numbers that meet that condition
			ref_vs_str_ind <- which(ref_vs_str_bool)
			
			# Compare map vs stratum and get TRUE or FALSE on each row
			map_vs_str_bool <- str_bool & map_label == rfcodes[r]
			# Get row numbers that meet that condition
			map_vs_str_ind <- which(map_vs_str_bool)
			
			# Get places where the current reference and map are the same in the 
			# current stratum class
			ref_vs_map_bool <- ref_vs_str_bool & map_vs_str_bool
			# Get row numbers that meet that condition
			ref_vs_map_ind <- which(ref_vs_map_bool)
			
			# Get place where ANY reference and map labels are the same in the current
			#  stratum class 
			ref_vs_map_all_bool <- str_bool & (ref_label == map_label)
			ref_vs_map_all_ind <- which(ref_vs_map_all_bool)
			
			# Calculate proportion (mean) of reference present in stratum
			ref_prop[s, r] <- length(ref_vs_str_ind) / 
											sample_totals[,2][sample_totals[,1] == str_codes[s]]
			# Calculate SAMPLE variance of reference in stratum.
			ref_var[s, r] <- var(ref_vs_str_bool[str_ind])
			
			# Calculate proportion (mean) of map present in stratum
			map_prop[s, r] <- length(map_vs_str_ind) / 
												sample_totals[,2][sample_totals[,1] == str_codes[s]]
			# Calculate SAMPLE variance of map in stratum.
			map_var[s, r] <- var(map_vs_str_bool[str_ind])
			
			# Calculate proportion (mean) of map == reference present in stratum for 
			# one particular label
			map_and_ref_prop[s, r] <- length(ref_vs_map_ind) / 
												sample_totals[,2][sample_totals[,1] == str_codes[s]]
			# Calculate SAMPLE variance of map == reference.
			map_and_ref_var[s, r] <- var(ref_vs_map_bool[str_ind])
			
			# Calculate proportion of map == ref present in stratum for ALL labels
			overall_acc_prop[s] <- length(ref_vs_map_all_ind) / 
												sample_totals[,2][sample_totals[,1] == str_codes[s]]
			overall_acc_var[s] <- var(ref_vs_map_all_bool[str_ind])
			
			# Calculate covariances associated with users and producers accuracy
			users_cov[s, r] <- cov(ref_vs_map_bool[str_ind], map_vs_str_bool[str_ind])
			producers_cov[s, r] <- cov(ref_vs_map_bool[str_ind], ref_vs_str_bool[str_ind])
			
		}
	}
	
	# Helper function to assign row and colnames
	assign_names <- function(df, rnames, cnames){
		rownames(df) <- rnames
		colnames(df) <-  cnames
		return(df)
	}
	
	# Assign column and row names
	strata_rownames <- paste0("strat_",str_codes)
	ref_prop <- assign_names(ref_prop, strata_rownames, paste0("ref_", rfcodes))
	ref_var <- assign_names(ref_var, strata_rownames, paste0("ref_", rfcodes))
	map_prop <- assign_names(map_prop, strata_rownames, paste0("map_", rfcodes))
	map_var <- assign_names(map_var, strata_rownames, paste0("map_", rfcodes))
	map_and_ref_prop <- assign_names(map_and_ref_prop, strata_rownames, 
														paste0("mapandref_", rfcodes))
	map_and_ref_var <- assign_names(map_and_ref_var, strata_rownames, 
														paste0("mapandref_", rfcodes))
	users_cov <- assign_names(users_cov, strata_rownames, paste0("ucov_", rfcodes))
	producers_cov <- assign_names(producers_cov, strata_rownames, 
														paste0("pcov_", rfcodes))
	
	# Calculate total number of pixels in original strata map. Also calculated
	# in the main script, so it's not being returned here.
	totalarea_pix <- sum(strata_totals[,2])
	class_prop <- vector()
	
	# Calculate ref_label class proportions (i.e. by columns) using total, 
	# original orig_strata areas.
	for (r in 1:ncol(ref_prop)) {
		# totalarea_pix is REQUIRED here even if there are no reference counts for 
		# a given stratum
		class_prop[r] <- sum(strata_totals[,2] * ref_prop[,r]) / totalarea_pix
	}
	
	return(list(ref_prop = ref_prop, ref_var = ref_var, map_prop = map_prop, 
							map_var = map_var, map_and_ref_prop = map_and_ref_prop, 
							map_and_ref_var = map_and_ref_var, 
							overall_acc_prop = overall_acc_prop, 
							overall_acc_var = overall_acc_var,
							users_cov = users_cov, producers_cov = producers_cov,
							class_prop = class_prop))
}

#' ###calcPropSE  
#' Function to calculate std error of unbiased area proportions of reference
#' classes for a given year/map  
#' 
#' **strata_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification and the second must 
#' have the total number of PIXELS of each class in that original strata map. 
#'  
#' **sample_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS  of each class collected from that 
#' original strata map.  
#' 
#' **ref_var** Dataframe with reference class variance for (column) per 
#' original strata class (row).  
#' 
#' **rfcodes** Vector with all the unique numerical classes present in the 
#' REFERENCE data . This is required to facilitate the calculations for multiple
#' maps/years when not all the reference classes are present in every map.  
#' 
#' **totarea_pix** Integer with the total number of pixels present in the 
#' original stratification map.  
#' 
#' **Returns** Vector with standard error of unbiased area proportions per reference
#' class.  

calcPropSE = function(strata_totals, sample_totals, ref_var, 
												rfcodes, totarea_pix){
	
	# Initialize vector to store results
	se <- vector(mode = "numeric", length = length(rfcodes))
	
	# Iterate over reference classes
	for (c in 1:length(rfcodes)) {
		v <- 1 / totarea_pix^2 * (sum(strata_totals[,2]^2 * 
									(1 - sample_totals[,2]/strata_totals[,2]) * 
										(ref_var[,c] / sample_totals[,2])))
		se[c] <- sqrt(v)
	}
	return(se)
}

#' ###calcUnbiasedArea  
#' Function to calculate unbiased area, confidence interval and 
#' margin of error.  
#' 
#' This function takes the area proportions obtained from the function 
#' calc_area_prop and calculates the areas (in ha) as well as the outputs 
#' described below.  
#' **totarea_pix** Integer with the total number of pixels present in the 
#' original stratification map.  
#' 
#' **class_prop** Vector of area proportions per reference class.  
#' 
#' **se** Vector with standard error of unbiased area proportions per 
#' reference class.  
#' 
#' **pixel** Pixel size in square meters; for landsat, this is 30.  
#' 
#' **return** List with vector of areas in ha (area), vector of HALF the width 
#' confidence interval (ci), vector of higher and lower confidence interval 
#' limits (upper_ci, lower_ci) and margin of error (me).  

calcUnbiasedArea = function(totarea_pix, class_prop, se, pixel){
	# Total area in ha
	N_ha <- totarea_pix * pixel^2 / 100^2
	# Calculate area in ha from area proportions
	area <- class_prop * N_ha
	# Calculate confidence interval in ha
	ci <- se * 1.96 * N_ha
	#Upper and lower CI
	upper_ci <- area + ci
	lower_ci <- area - ci
	me <- ci / area 
	return(list(area = area, ci = ci, upper_ci = upper_ci, lower_ci = lower_ci,
							me = me))
}

#' ###calcAccuracies  
#' Function to calculate accuracies and their 95% confidence intervals.  
#'
#' **strata_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification and the second must 
#' have the total number of PIXELS of each class in that original strata map.
#'   
#' **sample_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS of each class collected from that 
#' original strata map.  
#' 
#' **rfcodes** Vector with numeric values representing the reference codes
#' present in ALL of the periods.  
#' 
#' **totarea_pix** Integer with the total number of pixels present in the 
#' original stratification map.  
#' 
#' The following inputs must be contained in a list of data frames
#' that are input as the parameter **props_and_vars**. They are produced
#' by `calcPropsAndVars()` function.
#' 
#' **ref_prop** Dataframe with proportions (mean) of reference labels 
#' (columns) present on each sample strata class (rows).  
#' 
#' **ref_var** Dataframe of variances of reference labels (columns)
#' present on each sample strata class (rows).  
#' 
#' **map_prop** Dataframe of proportions (mean) of map labels (columns)
#' present on each sample strata class (rows).  
#' 
#' **map_var** Dataframe of variances of map labels (columns) present on each
#' sample strata class (rows).  
#' 
#' **map_and_ref_prop** Dataframe of proportions (mean) of matching reference
#' and map labels (columns) present on each sample strata class (rows).  
#' 
#' **map_and_ref_var** Dataframe of variances of matching reference and 
#' map labels (columns) present on each sample strata class (rows) .  
#' 
#' **overall_acc_prop** Vector of proportions of all map labels equal to 
#' reference labels present in stratum.  
#' 
#' **overall_acc_var** Vector of variances of all map labels equal to 
#' reference labels present in stratum.  
#' 
#' **users_cov** Dataframe of user's accuracy covariances per strata.  
#' 
#' **producers_cov** Dataframe of producer's accuracy covariances per strata
#' with its variance.  

calcAccuracies <-  function(strata_totals, sample_totals, rfcodes, totarea_pix,
													 props_and_vars){
	
	#Unpack props and vars
	ref_prop <- props_and_vars$ref_prop
	ref_var <- props_and_vars$ref_var
	map_prop <- props_and_vars$map_prop
	map_var <- props_and_vars$map_var
	map_and_ref_prop <- props_and_vars$map_and_ref_prop
	map_and_ref_var <- props_and_vars$map_and_ref_var
	overall_acc_prop <- props_and_vars$overall_acc_prop
	overall_acc_var <- props_and_vars$overall_acc_var
	users_cov <- props_and_vars$users_cov
	producers_cov <- props_and_vars$producers_cov
	
	# Initialize vector to store results
	se_overall <- vector(mode = "numeric", length = length(rfcodes))
	se_usr <- vector(mode = "numeric", length = length(rfcodes))
	se_prod <- vector(mode = "numeric", length = length(rfcodes))
	
	# Overall accuracy
	overall_acc <- sum(strata_totals[,2] * overall_acc_prop) / totarea_pix
	
	# User's and producers accuracies, common parameter
	param1 <- colSums(strata_totals[,2] * map_and_ref_prop)
	
	# Users's accuracy
	uparam <- colSums(strata_totals[,2] * map_prop)
	users_acc <- param1 / uparam
	names(users_acc) <- rfcodes
	
	# Producer's accuracy
	pparam <- colSums(strata_totals[,2] * ref_prop)
	producers_acc <- param1 / pparam
	names(producers_acc) <- rfcodes
	
	# Finite population correction term
	corr_term <- (1 - sample_totals[,2]/strata_totals[,2])
	
	# Standard error of accuracies
	# Overall accuracy
	vro <- 1/totarea_pix^2 * (sum(strata_totals[,2]^2 * corr_term  * 
						overall_acc_var / sample_totals[,2])) 
	se_overall <- sqrt(vro)
	
	for (c in 1:length(rfcodes)) {
		
		# User's accuracy
		vru <- 1/uparam[c]^2 * (sum(strata_totals[,2]^2 * corr_term  * 
						(map_and_ref_var[,c] + (users_acc[c]^2) * 
							(map_var[,c]) - 2 * users_acc[c] * users_cov[,c]) / 
								sample_totals[,2]))
		se_usr[c] <- sqrt(vru)
		
		# Producer's accuracy, higher than Stehman's paper by 0.002
		vrp <- 1/pparam[c]^2 * (sum(strata_totals[,2]^2 * corr_term * 
						(map_and_ref_var[,c] + (producers_acc[c]^2) * 
							(ref_var[,c]) - 2 * producers_acc[c] * producers_cov[,c]) / 
								sample_totals[,2]))
		se_prod[c] <- sqrt(vrp)
	}
	
	# Calculate confidence intervals
	
	overall_acc_min <- (overall_acc - (overall_acc * 1.96 * se_overall))
	overall_acc_max <- (overall_acc + (overall_acc * 1.96 * se_overall))
	users_acc_min <- (users_acc - (users_acc * 1.96 * se_usr))
	users_acc_max <- (users_acc + (users_acc * 1.96 * se_usr))
	producers_acc_min <- (producers_acc - (producers_acc * 1.96 * se_prod))
	producers_acc_max <- (producers_acc + (producers_acc * 1.96 * se_prod))
	
	
	return(list(overall_acc = overall_acc, overall_acc_min = overall_acc_min, 
							overall_acc_max = overall_acc_max, users_acc = users_acc, 
							users_acc_min = users_acc_min, users_acc_max = users_acc_max,
							producers_acc = producers_acc, producers_acc_min = producers_acc_min,
							producers_acc_max = producers_acc_max))
}
