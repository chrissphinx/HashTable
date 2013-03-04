class CodeIndex(whichHF: String, whichCRA: String, MAX_N_HOME_LOC: Int) {
	val ARRAY_SIZE = 440
	val countryCodes = Array.ofDim[String](ARRAY_SIZE)
	val links = Array.ofDim[String](ARRAY_SIZE)
	val pw = new java.io.PrintWriter(
			 new java.io.FileWriter("log.txt", true), true)

	def insert(code: String) {
		println("CODE: " + code + "\tHASH: " + hash(code, whichHF))
		insert(code, hash(code, whichHF))
	}

	def finish(print: Boolean) {
		if(print) {
			pw.println("PRINTING HASH TABLE")
		} else pw.println("JUST LOGGING AVERAGE SEARCH PATH")
			println(countryCodes.mkString("\n"))
			println(whichHF)
	}

	private def insert(code: String, loc: Int) {
		whichCRA match {
			case "linEmb" => if(countryCodes(loc) == null) countryCodes(loc) = code
							 else if(loc == MAX_N_HOME_LOC) insert(code, 0)
							 else insert(code, loc + 1)

			case "chSep" =>  // println("INSERTING: " + code + " " + loc)
							 if(countryCodes(loc) == null) countryCodes(loc) = code
							 else {
							 	if(loc < MAX_N_HOME_LOC) insert(code, MAX_N_HOME_LOC)
							 	else insert(code, loc + 1)
							 }
		}
	}

	private def hash(code: String, whichHF: String): Int = {
		whichHF match {
			case "times" => code.toArray.foldLeft(1)(_ * _) % MAX_N_HOME_LOC
			case "plus" => code.toArray.foldLeft(0)(_ + _) % MAX_N_HOME_LOC
		}
	}
}