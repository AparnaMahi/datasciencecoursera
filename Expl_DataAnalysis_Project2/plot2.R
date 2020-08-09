
# Emissions in Baltimore City over Years(1999,2002,2005 & 2008)

NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]
totalNEI <- NEI[fips=='24510', lapply(.SD, sum, na.rm = TRUE)
                , .SDcols = c("Emissions")
                , by = year]

png(filename='plot2.png')

barplot(totalNEI[, Emissions]
        , names = totalNEI[, year]
        , xlab = "Years", ylab = "Emissions" ,col="lightgreen"
        , main = "Emissions in Baltimore City over Years(1999,2002,2005 & 2008)")

dev.off()
