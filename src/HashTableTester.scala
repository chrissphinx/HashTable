import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object HashTableTester extends App {
	/********** VALUES ************************************/
	val whichHF		= Array("times", "plus")
	val whichCRA	= Array("linEmb", "chSep")
	val whatSizeSam = Array(30, 31)
	val whatSizeAll = Array(250, 251)
	val logFile		= new java.io.File("log.txt")
					  if(logFile.exists) logFile.delete
	val pw			= new java.io.PrintWriter(
					  new java.io.FileWriter(logFile, true), true)

	/********** MAIN **************************************/
	runExperiment("Sample", whichHF, whichCRA, whatSizeSam)
	runExperiment("All", whichHF, whichCRA, whatSizeAll)

	/********** METHODS ***********************************/
	def runExperiment(suffix: String, hashes: Array[String],
					  resolutions: Array[String], sizes: Array[Int]) = {
		// LOCALS
		val countryCodes = ArrayBuffer.empty[String]
		var data = Array.ofDim[String](15)

		pw.println("++++++++++++++++++++ data set: "
				 + suffix.formatted("%1$-7s") + "++++++++++++++++++++")

		Source.fromFile("RawData" + suffix + ".csv", "ISO-8859-1")
						.getLines().foreach(l => { 
			data = ",".r split l
			countryCodes += data(1)
		})

		for(h <- hashes; r <- resolutions; s <- sizes) {
			val codeIndex = new CodeIndex(h, r, s)
			for(c <- countryCodes.toArray)
				codeIndex.insert(c)
			codeIndex.finish(suffix == "Sample")
		}
	}
}