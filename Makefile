default: leibniz

leibniz: Parsley.lhs HaLay.lhs Block.hs Overlay.hs Tm.hs Main.hs ANSIEscapes.hs
	ghc -lncurses --make Main -o leibniz
