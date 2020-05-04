# zeno


Dev notes

* Ok, I spent AGES fighting with the signatures. Learnings:
  - `ecrecover` inside solidity expects v to be v+27. But, when singing the transaction it's chainId*2+v+35.
  - Haskell Secp256k1 library has R and S backwards, hense wrapper type. 
