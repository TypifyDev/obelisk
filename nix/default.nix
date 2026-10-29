{ system }:

let lib = import ./lib.nix { inherit system; };

    thunkSource = import ./thunk-source.nix;
    nixHaskellSrc = thunkSource ../thunks/nix-haskell;
    nix-haskell = import nixHaskellSrc { inherit system; };

    pkgs = import (nixHaskellSrc + "/pins/nixpkgs") { inherit system; };

    module = import ./module.nix;

in lib // {
  inherit module;
  inherit (lib) extraCabalProject serverModule;

  inherit (lib) jenga jenga-command-pkg;

  project = userModule:
    let eval = nix-haskell {
          imports = [
            module
            userModule
          ];
        };
        proj = eval.haskell-nix.project;
        pkgs = eval.nixpkgs;
        serverExe = {
          wasm = lib.mkServerExe { inherit proj; target = "wasm"; };
          js = lib.mkServerExe { inherit proj; target = "js"; };
        };
    in proj // {
      config = eval.config;
      nixpkgs = eval.nixpkgs;

      exe = {
        wasm = (proj.override { jenga.frontend.target = "wasm"; }).hsPkgs.backend.components.exes.backend;
        js = (proj.override { jenga.frontend.target = "js"; }).hsPkgs.backend.components.exes.backend;
      };

      inherit serverExe;

      containerImage = {
        wasm = lib.mkContainerImage { inherit proj; target = "wasm"; };
        js = lib.mkContainerImage { inherit proj; target = "js"; };
      };

      server = { exe ? serverExe.wasm, ... }@args:
        let nixos = import (pkgs.path + /nixos);
        in nixos {
          inherit system;
          configuration = {
            imports = [ lib.serverModule ];
            services.jenga = { enable = true; } // args;
          };
        };
    };
}
