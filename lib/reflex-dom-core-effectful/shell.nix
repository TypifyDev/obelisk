{ nixpkgs ? import <nixpkgs> {} }:
let
  pkgs = nixpkgs;
  hp   = pkgs.haskellPackages;
  chromium = if pkgs.stdenv.hostPlatform.isDarwin then
    let browserPkgs = import pkgs.path {
          system = pkgs.stdenv.hostPlatform.system;
          config.allowUnfreePredicate = p: pkgs.lib.getName p == "google-chrome";
        };
    in pkgs.writeShellScriptBin "chromium" ''
      exec ${browserPkgs.google-chrome}/bin/google-chrome-stable "$@"
    ''
    else pkgs.chromium;

  # Test-suite dependencies for the standalone shell. cabal.project and
  # the browser components constrain WebDriver to the compatible API.
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
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ hp.linux-namespaces ];

  ghc = hp.ghcWithPackages (_: testDeps);
in
pkgs.mkShell {
  JENGA_DEV_PLATFORM = if pkgs.stdenv.hostPlatform.isDarwin then "macos" else "linux";
  buildInputs = [
    ghc
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.pkg-config
    pkgs.zlib
    pkgs.pcre
    # Runtime: browser + driver + selenium (fetched lazily on first invoke).
    chromium
    pkgs.chromedriver
    pkgs.selenium-server-standalone
  ];
}
