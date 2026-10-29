{ system ? builtins.currentSystem }:
let jenga = import ./thunks/jenga { inherit system; };
in jenga.project (import ./project.nix)
