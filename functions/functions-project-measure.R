
#' doSampleVariance
#'
#' Computes the sample variance with (n-1) ...
#' 
#' @param x numeric vector
#' @param method "two-pass" prevents "naive" floating-point issues
#'
#' @return list (x.bar, s.var, s.sd)
#' @export
#'
#' @examples
#' doSampleVariance( c(1) ); # returns null
#' doSampleVariance( 1:2 ); 
#' 
doSampleVariance = function(x, method="two-pass")
	{
	x = na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;
		
		for(i in 1:length(x))  ## na.omit(x)
			{
			n = n + 1;
			sum = sum + x[i];
			sum2 = sum2 + x[i]*x[i];
			}
		
		if(n < 2) { return(NULL);} # 
			x.bar = sum/n;
			s.var = (sum2 - (sum*sum)/n)/(n-1);

		} else	{
				# two-pass algorithm # testing
				n = sum = sum2 = 0;
				## first pass 
				for(i in 1:length(x))  ## na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} # 
				x.bar = sum/n;
				## second pass 
				for(i in 1:length(x))  ## na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}

