let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
   buildInputs = with pkgs; [
     docker
     docker_compose
     nodejs
     postgresql
     ruby
     zlib
   ];
}
