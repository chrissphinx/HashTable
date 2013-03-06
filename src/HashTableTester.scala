/* PROJECT: HashTableTester (Scala)					SOURCE: HashTableTester
 * AUTHOR: Colin MacCreery
 * FILES ACCESSED: RawDataAll.csv, RawDataSample.csv. Log.txt
 * DESCRIPTION: This is the main controller program. Scala allows for-loops to
 *				iterate over multiple arrays, yielding all possible combinations.
 *				Because of this, only unique values are stored in the whichHF,
 *				whichCRA, whatSizeSam and whatSizeAll arrays.
 *
 *				Scala also does not have a switch statement, but instead uses
 *				match expressions. These are very versatile and because it is a
 *				functional lanuage you can have function blocks enclosed in {}
 *				pretty much anywhere you want. I use this to embed match expressions
 *				or conditionals inside of method parameters (like println).
 *
 *				The language API has a built in way to read from files called
 *				Source. Unforunately, it does not have a similar way to WRITE to a
 *				file so Java objects must be used (logFile, pw).
 *
 *				Instead of writing a main method (def main(args: Array[String])) you
 *				can simply have an object extend App and the entire object body is
 *				treated as the main method.
 ******************************************************************************/

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
	private def runExperiment(suffix: String, hashes: Array[String],
							  resolutions: Array[String], sizes: Array[Int]) = {
		// LOCALS
		val countryCodes = ArrayBuffer.empty[String]
		var data = Array.ofDim[String](15)

		// DATA SET HEADER
		pw.println("++++++++++++++++++++ data set: "
				   + { suffix match {
						   case "Sample" => "SAMPLE "
						   case "All" => "ALL +++"
					   }}
				   + "++++++++++++++++++++")

		// GET LINES FROM FILE AND SPLIT THEM
		Source.fromFile("RawData" + suffix + ".csv", "ISO-8859-1")
						.getLines().foreach(l => { 
			data = ",".r split l
			countryCodes += data(1)
		})

		// DO EVERY COMBINATION WITH A CODE INDEX OBJECT
		for(h <- hashes; r <- resolutions; s <- sizes) {
			val codeIndex = new CodeIndex(h, r, s)
			for(c <- countryCodes.toArray)
				codeIndex.insert(c)
			codeIndex.finish(suffix == "Sample")
		}
	}
}