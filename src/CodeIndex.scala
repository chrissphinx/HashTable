import scala.io.Source

class CodeIndex {
	val file = "RawDataSample.csv"
	var data = Array.ofDim[String](15)

	Source.fromFile(file, "ISO-8859-1").getLines().foreach(l => { 
		data = ",".r split l
		print(data(0).toInt.formatted("%02d"))
		println(" " + data(1))
	})
}