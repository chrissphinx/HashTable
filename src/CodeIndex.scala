/* PROJECT: HashTableTester (Scala)					SOURCE: CodeIndex
 * AUTHOR: Colin MacCreery
 * DESCRIPTION: 
 ******************************************************************************/

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

	/********** METHODS ***********************************/
	def insert(code: String) {
		insert(code, hash(code, whichHF))
	}

	def finish(print: Boolean) {
		// CALCULATE AVERAGE SEARCH PATH
		var aPath = (((0.0f, 1) /: nHome) {
			case ((a, i), e) => (a + (e * i), i + 1) })._1
		aPath /= (nColl + nHome(0))

		// PRINT HEADER AND TABLE INFORMATION
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

		// PRINT CODE ARRAY (OPTIONALLY WITH LINKS) IF PRINT SWITCH IS TRUE
		if(print) {
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

		// FOOTER
		pw.println("----------------------------------------------------------\n")
	}

	/********** PRIVATE METHODS ***************************/
	private def insert(code: String, loc: Int) {
		// INSERT INTO EMPTY SPOT IF POSSIBLE
		if(countryCodes(loc) == "") {
			countryCodes(loc) = code
			links(loc) = -1
			nHome(0) += 1

		// OTHERWISE BRANCH TO RESPECTIVE RECURSIVE FUNCTION
		} else whichCRA match {
			case "linEmb" =>
				linearInsert(code, loc + 1)
				nColl += 1
			case "chSep" =>
				if(links(loc) == -1) links(loc) = MAX_N_HOME_LOC + nColl
				chainInsert(code, links(loc))
				nColl += 1
		}
	}

	private def linearInsert(code: String, loc: Int) {
		// RECURSIVE LINEAR INSERT WITH WRAP-AROUND
		n += 1
		if(countryCodes(loc % MAX_N_HOME_LOC) == "") {
			countryCodes(loc % MAX_N_HOME_LOC) = code
			nHome(n) += 1
			n = 0
		}
		else linearInsert(code, loc % MAX_N_HOME_LOC + 1)
	}

	private def chainInsert(code: String, loc: Int) {
		// RECURSIVE CHAIN INSERT, ADDING TO END OF CHAIN
		n += 1
		if(countryCodes(loc) == "") {
			countryCodes(loc) = code
			links(loc) = -1
			nHome(n) += 1
			n = 0
		} else {
			if(links(loc) == -1) links(loc) = MAX_N_HOME_LOC + nColl
			chainInsert(code, links(loc))
		}
	}

	private def hash(code: String, whichHF: String): Int = {
		// RESPECTIVE HASH FUNCTIONS
		whichHF match {
			case "times" => (1 /: code.toArray)(_ * _) % MAX_N_HOME_LOC
			case "plus" => (0 /: code.toArray)(_ + _) % MAX_N_HOME_LOC
		}
	}
}