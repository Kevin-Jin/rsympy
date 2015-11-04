
sympyStart <- function() {

	# like system.file but on Windows uses \ in path rather than /
	system.file. <- function(...) {
		s <- system.file(...)
		if (.Platform$OS == "windows") gsub("/", "\\", s, fixed = TRUE) else s
	}

	if (!pyIsConnected()) pyConnect()

	pyExecp("import sys")
	pyExecp( paste( "sys.path.append(", system.file( "Lib", package = "rSymPy" ), ")", sep = '"' ) )
	pyExecp("from sympy import *")
	pyExecp("from sympy.printing.mathml import mathml")
	pyExecp("from sympy.utilities.lambdify import lambdify")

	assign('.SympyConnected', TRUE)
}

sympy <- function(..., retclass = c("character", "Sym"), debug = FALSE) {
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
	retclass <- if (is.null(retclass)) NULL else match.arg(retclass)
	if (!is.null(retclass)) {
		pyExec(paste("__Rsympy=None"))
		pyExecp(paste("__Rsympy=", ..., sep = ""))
		if (debug) pyExecp("print(__Rsympy)")
		pyExec(paste("__Rsympy = str(__Rsympy)"))
		out <- pyGet("__Rsympy")
		if (!is.null(out) && retclass == "Sym") structure(out, class = "Sym")
		else out
	} else pyExecp(paste(...))
}



