{ system ? builtins.currentSystem
, pkgs ? import ((import ./thunk-source.nix ../thunks/nix-haskell) + "/pins/nixpkgs") { inherit system; }
}:

let nixHaskellSrc = import ./thunk-source.nix ../thunks/nix-haskell;
    eval = import (nixHaskellSrc + "/eval.nix") { inherit system pkgs; };

    options = (eval (import ./module.nix)).options;

in import (nixHaskellSrc + "/docs.nix") {
  inherit pkgs;
  options = { jenga = options.jenga; };
}
