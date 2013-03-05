class CodeIndex(whichHF: String, whichCRA: String, MAX_N_HOME_LOC: Int) {
	/********** VALUES ************************************/
	val ARRAY_SIZE = 440
	val countryCodes = Array.fill(ARRAY_SIZE){""}
	val links = Array.ofDim[Int](ARRAY_SIZE)
	val nHome = Array.ofDim[Int](220)
	val pw = new java.io.PrintWriter(
			 new java.io.FileWriter("log.txt", true), true)

	/********** VARIABLES *********************************/
	var n = 1
	var nColl = 0
	var aPath = 0.0f

	/********** METHODS ***********************************/
	def insert(code: String) {
		insert(code, hash(code, whichHF))
	}

	def finish(printSwitch: Boolean) {
		pw.println(">> " + whichHF.toUpperCase + " HashFunction, "
				   + { whichCRA match {
						   case "linEmb" => "LINEAR / EMBEDDED"
						   case "chSep" => "CHAINING / SEPARATE"
					   }}
				   + " CollisionRes")
		pw.println(">>    MaxNHomeLoc: " + MAX_N_HOME_LOC.formatted("%3d"))
		pw.println(">> NHome: " + nHome(0).formatted("%3d")
				   + ",  NColl: " + nColl.formatted("%3d")
				   + ",  average search path: " + aPath.formatted("%4.1f"))

		if(printSwitch) {
			whichCRA match {
				case "linEmb" =>
					for(e <- countryCodes.zipWithIndex.view(0, MAX_N_HOME_LOC)) {
						pw.println("[" + e._2.formatted("%02d") + "] " + e._1)
					}
				case "chSep" =>
					for(e <- countryCodes.zipWithIndex.view(0, MAX_N_HOME_LOC * 2)){
						pw.println("[" + e._2.formatted("%02d") + "] " + e._1 + " "
								   + { if(links(e._2) != 0)
										   links(e._2).formatted("%2d")
										   else ""
									   })
					}
			}
			
		}

		pw.println("----------------------------------------------------------\n")
	}

	/********** PRIVATE METHODS ***************************/
	private def insert(code: String, loc: Int) {
		if(countryCodes(loc) == "") {
			countryCodes(loc) = code
			nHome(0) += 1
		} else whichCRA match {
			case "linEmb" =>
				nColl += 1
				linearInsert(code, loc + 1)
			case "chSep" =>
				nColl += 1
				chainInsert(code, MAX_N_HOME_LOC)
		}
	}

	private def linearInsert(code: String, loc: Int) {
		var curLoc = loc % MAX_N_HOME_LOC
		n += 1
		if(countryCodes(curLoc) == "") {
			countryCodes(curLoc) = code
			nHome(n) += 1
			n = 0
		}
		else linearInsert(code, curLoc + 1)
	}

	private def chainInsert(code: String, loc: Int) {
		
	}

	private def hash(code: String, whichHF: String): Int = {
		whichHF match {
			case "times" => code.toArray.foldLeft(1)(_ * _) % MAX_N_HOME_LOC
			case "plus" => code.toArray.foldLeft(0)(_ + _) % MAX_N_HOME_LOC
		}
	}
}