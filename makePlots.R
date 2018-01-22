makePlots <- function() {
	    ### 2007-02-01 and 2007-02-02
	    powerCons = read.table("household_power_consumption.txt",skip=grep("^1/2/2007", readLines("household_power_consumption.txt")),nrows=2880, sep = ";")
	    head(powerCons)

	    pdf(file="hist.pdf")
	    hist(powerCons$V3, col = "red", main = "Global active Power", xlab="Global active Power (Kilowats)")
	    dev.off()

	    dateAndTime <- paste(powerCons$V1, powerCons$V2)
	    DT <- strptime(dateAndTime, "%d/%m/%Y %H:%M:%S")

	    pdf(file="Global.pdf")
	    plot(DT, powerCons$V3, ylab="Global active Power (Kilowats)", xlab= "Time", pch = '.')
	    lines(DT, powerCons$V3)
	    dev.off()

	    pdf(file="EnergySub.pdf")
	    plot(DT, powerCons$V7, ylab="Energy Sub metering", xlab= "Time", pch = '.')
	    lines(DT, powerCons$V7)
	    lines(DT, powerCons$V8, col = "red")
	    lines(DT, powerCons$V9, col = "blue")
	    legend("topright", col = c("black", "red","blue"), legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), pch = 20)
	    dev.off()

	    pdf(file="All.pdf")
	    par(mfrow=c(2,2),mar=c(4,4,2,1))
            plot(DT, powerCons$V3, ylab="Global active Power (Kilowats)", xlab= "Time", pch = '.')
            lines(DT, powerCons$V3)
	     plot(DT, powerCons$V5, ylab="Voltage", xlab= "Time", pch = '.')
            lines(DT, powerCons$V5)
	    plot(DT, powerCons$V7, ylab="Energy Sub metering", xlab= "Time", pch = '.')
            lines(DT, powerCons$V7)
            lines(DT, powerCons$V8, col = "red")
            lines(DT, powerCons$V9, col = "blue")
            legend("topright", col = c("black", "red","blue"), legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), pch = 20)
            plot(DT, powerCons$V4, ylab="Global reactive power", xlab= "Time", pch = '.')
            lines(DT, powerCons$V4)
	    dev.off()
}