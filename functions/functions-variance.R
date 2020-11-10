doStatsSummary = function(x)
	{
	result = list();
		result$length = length(x);
	xx = stats::na.omit(x);
		result$length.na = length(x) - length(xx);
		result$length.good = length(xx);
	result$mean = mean(xx);
	result$mean.trim.05 = mean(xx, trim=0.05);
	result$mean.trim.20 = mean(xx, trim=0.20);

	result$median = stats::median(xx);
	result$MAD = stats::mad(xx);
	result$IQR = stats::IQR(xx);
	result$quartiles = stats::quantile(xx, prob=c(.25,.5,.75));
	result$deciles = stats::quantile(xx, prob=seq(0.1,0.9,by=0.1) );
	result$centiles = stats::quantile(xx, prob=seq(0.01,0.99,by=0.01) );

	result$median.weighted = matrixStats::weightedMad(xx);
	result$MAD.weighted = matrixStats::weightedMedian(xx);

	result$max = max(xx);
	result$min = min(xx);
	result$range = result$max - result$min;
	result$xlim = range(xx);

	result$max.idx = whichMax(x);
	result$min.idx = whichMin(x);

	result$mode = result$freq.max = doMode(x);  # elements with highest frequency
	result$which.min.freq = doModeOpposite(x);

	result$ylim = c( freqMin(xx), freqMax(xx) );

	# you could later get indexes of each mode(freq.max)/freq.min using findAllIndexesWithValueInVector

	result$sd = stats::sd(xx);
	result$var = stats::var(xx);

	result$var.naive = doSampleVariance(x,"naive");
	result$var.2step = doSampleVariance(x,"2step");


	## normality
	result$shapiro = stats::shapiro.test(xx);
	result$shapiro.is.normal = list("0.10" = isTRUE(result$shapiro$p.value > 0.10), "0.05" = isTRUE(result$shapiro$p.value > 0.05), "0.01" = isTRUE(result$shapiro$p.value > 0.01) );

	result$outliers.z = findOutliersUsingZscores(x);
	result$outliers.IQR = findOutliersUsingIQR(x);

	#result$z = calculateZscores(x);

	result;
  }

#################################################################

doSampleVariance = function(x, method="two-pass")
	{
	x = stats::na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;

		for(i in 1:length(x))  ## stats::na.omit(x)
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
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} #
				x.bar = sum/n;
				## second pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}

#################################################################

doMode = function(x) # alias ?
	{
	whichMaxFreq(x);
  }


whichMaxFreq = function(x)  # doMode
	{
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.max,];
	xs = as.numeric( as.vector (x.list$x) );
	xs;
	}
