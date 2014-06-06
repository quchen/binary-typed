all :
	cabal build -j

haddock :
	haddock -h -odoc Main.hs