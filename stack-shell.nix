{ghc}:
let pkgs = import <nixpkgs> {};
in pkgs.haskell.lib.buildStackProject {
   name = "redis-job";
   buildInputs = with pkgs; [
     postgresql
     zlib
     ghc
   ];
}
