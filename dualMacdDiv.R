#^GSPC S&P 500

getSymbols("^GSPC")
# calculate DVI indicator
dvi <- DVI(Cl(GSPC))  # Cl() extracts the close price column

# create signal: (long (short) if DVI is below (above) 0.5)
# lag so yesterday's signal is applied to today's returns
sig <-  Lag(ifelse(dvi$e1 < 0.5, 1, -1))
# calculate signal-based returns
ret <- ROC(Cl(GSPC))*sig
# subset returns to match data in Excel file
ret <- ret['2016-02-15/2016-04-07']
# use the PerformanceAnalytics package
#install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)
# create table showing drawdown statistics
table.Drawdowns(ret, top=10)
# create table of downside risk estimates
table.DownsideRisk(ret)
# chart equity curve, daily performance, and drawdowns
charts.PerformanceSummary(ret)

#following to calculuate MACD and signal
macd <- MACD( GSPC[,"GSPC.Close"], 12, 26, 9, maType="EMA")
macdDiv <- macd$macd - macd$signal
chartSeries(macdDiv,
            type = c("auto"), 
            subset='last 2 months', 
            show.grid = TRUE,
            major.ticks='auto', minor.ticks=TRUE)


combinedSig <- (ifelse((macdDiv > 0.1 & sig == 1), 1, 0))
