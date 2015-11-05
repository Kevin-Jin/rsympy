# methods

# Ops.Sym group generic overrides
Ops.Sym <- function (e1, e2) if (missing(e2)) Sym(.Generic, e1) else Sym(e1, .Generic, e2)
`^.Sym` <- function(e1, e2) if (missing(e2)) Sym("**", e1) else Sym(e1, "**", e2)
`%%.Sym` <- function(e1, e2) if (missing(e2)) Sym("mod(", e1, ")") else Sym("Mod(", e1, ",", e2, ")")
`%/%.Sym` <- function(e1, e2) if (missing(e2)) trunc(Sym("/", e1)) else trunc(Sym(e1, "/", e2)) # "//" doesn't work
`!.Sym` <- function(e1, e2) if (missing(e2)) Sym("~", e1) else Sym(e1, "~", e2)
# Safer: `&` => And(), `|` => Or(), `!` => Not(), `==` => Eq(), `!=` => Ne(),
# `<` => Lt(), `<=` => Le(), `>=` => Ge(), `>` => Gt()


# Math.Sym group generic overrides
transtab <- rbind(
	c("Abs", NA, "abs"),
	c("lgamma", NA, "loggamma")
)
Math.Sym <- function(x, ...) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	Sym(fn, "(", x, ")")
}
# TODO: this evaluates the passed expression three times. Very inefficient
trunc.Sym <- function(x) Sym("Piecewise((floor(", x, "),", x, ">= 0), (ceiling(", x, "), True))")
round.Sym <- function(x, digits = 0) Sym("round(", x, ",", digits, ")")
signif.Sym <- function(x, digits = 6) Sym("round(", x, ",", digits, "- (floor(log(", x, ", 10)) + 1))")
expm1.Sym <- function(x) Sym("exp(", x, ") - 1")
log1p.Sym <- function(x) Sym("log(1 +", x, ")")
cospi.Sym <- function(x) Sym("cos(", x, " * pi)")
sinpi.Sym <- function(x) Sym("sin(", x, " * pi)")
tanpi.Sym <- function(x) Sym("tan(", x, " * pi)")
cumsum.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
cumprod.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
cummax.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
cummin.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))


# Summary.Sym group generic overrides
all.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
any.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
sum.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
prod.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))
min.Sym <- function(x, na.rm = FALSE) Sym("Min(", x, ")")
max.Sym <- function(x, na.rm = FALSE) Sym("Max(", x, ")")
range.Sym <- function(x) stop(paste(match.call()[[1]], "() not implemented", sep = ""))

# Complex.Sym group generic overrides
Arg.Sym <- function(x) Sym("arg(", x, ")")
Conj.Sym <- function(x) Sym("conjugate(", x, ")")
Im.Sym <- function(x) Sym("im(", x, ")")
Mod.Sym <- function(x) abs(x)
Re.Sym <- function(x) Sym("re(", x, ")")

# TODO: autogenerate wrappers with setGenericS3() for all functions in http://docs.sympy.org/dev/modules/functions/index.html#contents
# that are not in transtab[, 3] and do not have a method named "xxx.Sym"
