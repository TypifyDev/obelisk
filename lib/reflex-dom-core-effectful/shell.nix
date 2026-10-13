{ nixpkgs ? import <nixpkgs> {} }:
let
  pkgs = nixpkgs;
  hp   = pkgs.haskellPackages;

  # Only the test-suite Haskell deps that aren't in the port's
  # cabal-resolved set: nixpkgs has these patched to compile together
  # (upstream Hackage versions do not — hspec-webdriver-1.2.2 imports
  # symbols removed in webdriver 0.11+).
  testDeps = with hp; [
    hspec-webdriver
    webdriver
    jsaddle-warp
    ghcjs-dom
    hspec
    hspec-core
    HUnit
    async
    exceptions
    silently
    temporary
    process
    which
    linux-namespaces
    dependent-map
    dependent-sum
    dependent-sum-template
    constraints-extras
    ref-tf
    lens
    mtl
    network
    http-types
    wai
    warp
    websockets
    random
    some
    directory
    filepath
  ];

  ghc = hp.ghcWithPackages (_: testDeps);
in
pkgs.mkShell {
  buildInputs = [
    ghc
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.pkg-config
    pkgs.zlib
    pkgs.pcre
    # Runtime: browser + driver + selenium (fetched lazily on first invoke).
    pkgs.chromium
    pkgs.chromedriver
    pkgs.selenium-server-standalone
  ];
}
