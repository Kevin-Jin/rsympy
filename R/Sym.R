
# constructor

Sym <- function(..., retclass = c("Sym", "character")) {
	args <- list(...)
	retclass <- match.arg(retclass)
	value <- if (length(args) > 1) paste("(", ..., ")") else paste(args[[1]])
	if (retclass == "Sym") class(value) <- c("Sym", "character")
	value
}

# methods

as.character.Sym <- function(x, ...) as.character(unclass(x))

Ops.Sym <- function (e1, e2) 
	if (missing(e2)) { Sym(.Generic, e1)
	} else Sym(e1, .Generic, e2)

Math.Sym <- function(x, ...) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	Sym(fn, "(", x, ")")
}

print.Sym <- function(x, ...) print(sympy(unclass(x), ...))

deriv.Sym <- function(expr, name = "x", n = 1, ...) 
	Sym("diff(", expr, ", ", name, ",", n, ")")

if (!isGenericS3("limit")) setGenericS3("limit")
limit.Sym <- function(expr, name = "x", value) 
	Sym("limit(", expr, ",", name, ",", value, ")")

solve.Sym <- function(a, b, method = c("'GE'", "'ADJ'", "'LU'"), ...) {
	stopifnot(missing(b))
	Sym(paste("(", a, ").inv(", match.arg(method), ")"))
}

if (!isGenericS3("integrate")) setGenericS3("integrate", dontWarn = "stats")
integrate.Sym <- function(x, ...) Sym("integrate(", paste(x, ..., sep = ","), ")")

t.Sym <- function(x) Sym(paste("(", x, ").transpose()"))

# static factories

Var <- function(x, retclass = c("Sym", "character")) {
	x <- paste("var('", x, "')", sep = "")
	sympy(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass))
}

List <- function(...) Sym("[", paste( ..., sep = ","), "]")
Matrix <- function(...) Sym("Matrix(", paste(..., sep = ","), ")")
Zero <- function(n) Sym(paste("zero(", n, ")"))
Eye <- function(n) Sym(paste("eye(", n, ")"))
Zeros <- function(m, n) Sym(paste("zero(", m, ",", n, ")"))

