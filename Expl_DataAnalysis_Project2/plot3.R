
# Emissions, Baltimore City 1999,2002,2005,2008 by Source Type

# Subset NEI data by Baltimore
baltimoreNEI <- NEI[fips=="24510",]


png("plot3.png")

ggplot(baltimoreNEI,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity", width = 0.45, fill = "steelblue") +
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999,2002,2005,2008 by Source Type"))

dev.off()

