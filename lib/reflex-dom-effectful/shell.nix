{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:

# Standalone dev shell for reflex-dom-effectful. This is INDEPENDENT of the
# ace-ws / typify-ws obelisk build so the honest-port smoke suite can iterate
# without rebooting `ob run`. Consumers inside obelisk get reflex-dom-effectful
# via the obelisk overlay; this shell.nix is only for library-local hacking.
#
# Deliberately does NOT depend on ../reflex-platform. Every reflex library
# needed is resolved from nixpkgs' haskellPackages, with jailbreak/dontCheck
# escape hatches only for packages known to carry stale bounds.

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  hlib = pkgs.haskell.lib;

  reflex-drv          = hlib.dontCheck (hlib.doJailbreak haskellPackages.reflex);
  reflex-dom-core-drv = hlib.dontCheck (hlib.doJailbreak haskellPackages.reflex-dom-core);

  drv = haskellPackages.callCabal2nix "reflex-dom-effectful" ./. {
    reflex          = reflex-drv;
    reflex-dom-core = reflex-dom-core-drv;
  };
in
pkgs.mkShell {
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.pkg-config
    pkgs.zlib
  ];
}
