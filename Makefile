all :
	cabal build -j

haddock :
	haddock -h -odoc BinaryTyped.hs