The polling file: Polls.csv
========================================================

This is a short description of the file Polls.csv. The file contains all polls conducted in Sweden regarding political sympathies from 2000. The file originated from [Novus](http://www.novus.se/vaeljaropinionen/ekotnovus-poll-of-polls.aspx) but has been updated and variables added.

The purpose of using this file instead of the file produced by [Novus](http://www.novus.se) is threefold. 
1. The file produced by Novus is quite badly formatted for analysis. 
2. The file is not updated as each poll is . So the latest polls will not be included until Novus releases their last poll of polls. 
3. The file at Novus change the name each month so it cant be used directly from the web.

### Data description

Variable      | Description
------------- | -------------
PublYearMonth | Month and year of publication
Company	      | Company name at publication
M	- SD	      | Poll results for parties
Uncertain	    | Uncertain voters
n	            | The number of observations
PublDate	    | Date of publication
collectPeriodFrom	| Start date of data collection
collectPeriodTo	| End date of data collection
approxPeriod | Indicator if the period is known or if it is an approximation of the period
house | The latest companyname (if the name has been changed)

The value ```r NA```` follows the R standard and means missing value. 

### If you spot any faults or want to add old polls...
If you spot any errors or want to contribute, feel free to send me a merge request with your suggested corrections and/or additions. Or send an e-mail to [mons.magnusson@gmail.com](mailto:mons.magnusson@gmail.com).


### To download the file direct into R
To download this file directly to R just use the following commands and you are good to go:

```r 
library(devtools)
source_gist(id = "https://gist.github.com/MansMeg/c0527fd762580006daed", quiet=TRUE)
polls <- source_GitHubData(url = "https://github.com/MansMeg/Ada/raw/master/Data/Polls.csv", sep = ",", dec = ".", header = TRUE)
```

If you just want to download the csv-file as is you can find the file [here](https://github.com/MansMeg/Ada/blob/master/Data/Polls.csv). To download it, just click "Raw".

