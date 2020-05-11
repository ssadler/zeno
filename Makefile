
dev:
	ghcid -c 'stack repl --test zeno:lib'

dot:
	@graphmod -p ##  --collapse=Bits.DB --collapse=Bits.Types --collapse=Bits.Web.API --collapse=Bits.Solver --collapse=Bits.App --collapse=Bits.Utils

