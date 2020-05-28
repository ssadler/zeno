
dev:
	ghcid -c 'stack repl zeno:lib'

dev-test:
	ghcid -c 'stack repl zeno --test --main-is :zeno-test' --test ':main --color always'


# Usage: make dot | xdot -
dot:
	@graphmod -p ##  --collapse=Bits.DB --collapse=Bits.Types --collapse=Bits.Web.API --collapse=Bits.Solver --collapse=Bits.App --collapse=Bits.Utils

