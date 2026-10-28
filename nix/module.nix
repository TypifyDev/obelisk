# nix-haskell module that wires Jenga overrides into a project.
# Declares `jenga.static`, `jenga.frontend.js`, and `jenga.frontend.wasm` options;
# `jenga.frontend.target` selects which pipeline feeds the backend.
{ config, lib, pkgs, system, nix-haskell-patches, ... }:

let jengaLib = import ./lib.nix { inherit system; };

    assets = import ./assets.nix { nixpkgs = pkgs; };

    # Merge singular path (at root) with named paths (as subdirs).
    allStaticPaths =
      (lib.optionalAttrs (config.jenga.static.path != null) {
        "" = config.jenga.static.path;
      })
      // config.jenga.static.paths;

    rawStatic = if allStaticPaths == {} then null
      else if allStaticPaths ? "" && builtins.length (builtins.attrNames allStaticPaths) == 1
      then allStaticPaths.""  # Single root path — pass through directly (backward compat)
      else pkgs.runCommand "merged-static" {} (
        "mkdir -p $out\n"
        + lib.concatStringsSep "\n" (lib.mapAttrsToList (name: src:
            if name == ""
            then "cp -rL ${src}/. $out/"
            else "ln -s ${src} $out/${name}"
          ) allStaticPaths)
      );

    # Hash and copy static files into a flat directory with cache-busting names.
    hashedStatic = if rawStatic != null
      then pkgs.runCommand "hashed-static" ({
        LANG = "en_US.UTF-8";
      } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isGnu {
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      }) ''
        ${jengaLib.jenga-asset-manifest-generate} --module-only ${rawStatic} "$TMPDIR" Jenga.Generated.Static $out
      ''
      else null;

    static = config.jenga.static.compressed;

    frontendJs = config.jenga.frontend.js.package;

    frontendWasm = config.jenga.frontend.wasm.package;

    # Select which frontend pipeline feeds the backend based on target.
    frontendOutput =
      if config.jenga.frontend.target == "wasm"
      then { inherit (config.jenga.frontend.wasm) optimized compressed; }
      else { inherit (config.jenga.frontend.js) optimized compressed; };

    compressedFrontendJs = frontendOutput.compressed;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  options.jenga = {
    static = {
      path = lib.mkOption {
        type = lib.types.nullOr (lib.types.either lib.types.path lib.types.package);
        default = null;
        description = "Static assets path or derivation.";
      };

      paths = lib.mkOption {
        type = lib.types.attrsOf (lib.types.either lib.types.path lib.types.package);
        default = {};
        description = ''
          Named static asset directories. Each value is a path or derivation.
          Keys become subdirectory prefixes in the merged output.
          Use alongside jenga.static.path (which merges at root level).
        '';
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to compress static assets with zopfli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering asset generation.
        default = if lib.inNixShell then null
          else if hashedStatic != null && config.jenga.static.compress
            then assets.mkAssets hashedStatic
            else hashedStatic;
        defaultText = lib.literalExpression "assets.mkAssets hashedStatic";
        description = "Hashed static assets after optional compression. Used by overrides.";
      };
    };

    frontend.target = lib.mkOption {
      type = lib.types.enum [ "js" "wasm" ];
      default = "wasm";
      description = "Frontend compilation target.";
    };

    frontend.js = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null else jengaLib.frontendJs config;
        defaultText = lib.literalExpression "jengaLib.frontendJs config";
        description = "GHCJS-compiled frontend derivation.";
      };

      optimization = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to run closure-compiler on frontend JS.";
        };

        level = lib.mkOption {
          type = lib.types.enum [ "BUNDLE" "WHITESPACE_ONLY" "SIMPLE" "TRANSPILE_ONLY" "ADVANCED" ];
          default = "ADVANCED";
          description = "Closure-compiler optimization level.";
        };

        externs = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default = [];
          description = "Extern files passed to closure-compiler via --externs.";
        };

        extraFlags = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = "Extra flags passed to closure-compiler.";
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let opt = config.jenga.frontend.js.optimization;
              externFlags = map (e: "--externs ${e}") opt.externs;
              flags = lib.concatStringsSep " " ([
                "--language_in" "UNSTABLE"
                "--compilation_level" opt.level
                "--warning_level" "QUIET"
                "--isolation_mode" "IIFE"
                "--assume_function_wrapper"
                "--emit_use_strict"
                "--jscomp_off=undefinedVars"
              ] ++ externFlags ++ opt.extraFlags);
          in if frontendJs == null then null
            else if opt.enable
            then pkgs.runCommand "frontend.jsexe.optimized" {
              nativeBuildInputs = [ pkgs.closurecompiler ];
            } ''
              cp -r ${frontendJs}/bin/frontend.jsexe $out
              chmod -R u+w $out
              closure-compiler ${flags} \
                --externs $out/all.externs.js \
                --js $out/all.js \
                --js_output_file $out/all.js.opt
              mv -f $out/all.js.opt $out/all.js
            ''
            else "${frontendJs}/bin/frontend.jsexe";
        defaultText = lib.literalExpression "closure-compiler frontendJs";
        description = "Closure-compiled frontend jsexe.";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.jenga.static.compress;
        description = "Whether to compress frontend JS with brotli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let jsexe = config.jenga.frontend.js.optimized;
          in if jsexe == null then null
            else if config.jenga.frontend.js.compress
            then assets.mkAssets jsexe
            else jsexe;
        defaultText = lib.literalExpression "assets.mkAssets optimized";
        description = "Compressed frontend jsexe for jenga-asset-serve-snap.";
      };
    };

    frontend.wasm = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null else jengaLib.frontendWasm config;
        defaultText = lib.literalExpression "jengaLib.frontendWasm config";
        description = "WASM-compiled frontend derivation.";
      };

      optimization = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to run wasm-opt on frontend WASM.";
        };

        level = lib.mkOption {
          type = lib.types.enum [ "0" "1" "2" "3" "4" "s" "z" ];
          default = "2";
          description = "wasm-opt optimization level (-O).";
        };

        extraFlags = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "-ol" "2" "-s" "1" "--low-memory-unused" "--strip-dwarf" "--converge" ];
          description = "Extra flags passed to wasm-opt.";
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let opt = config.jenga.frontend.wasm.optimization;
              wasmBin = "${frontendWasm}/bin/frontend.wasm";
              ghc = config.haskell-nix.project.projectCross.wasi32.pkg-set.config.ghc.package;
              flags = lib.concatStringsSep " " ([ "-all" "-O${opt.level}" ] ++ opt.extraFlags);
          in if frontendWasm == null then null
            else pkgs.runCommand "frontend.jsexe.wasm" {
              nativeBuildInputs = [ pkgs.nodejs pkgs.binaryen pkgs.wasm-tools ];
            } ''
              mkdir -p $out

              # Extract JSFFI bindings
              node $(${ghc}/bin/wasm32-unknown-wasi-ghc --print-libdir)/post-link.mjs \
                -i ${wasmBin} -o $out/ghc_wasm_jsffi.js

              # Optimize and strip WASM binary
              ${if opt.enable
                then ''
                  wasm-opt ${flags} ${wasmBin} -o $out/frontend.wasm
                  wasm-tools strip -a $out/frontend.wasm -o $out/frontend.wasm
                ''
                else ''cp ${wasmBin} $out/frontend.wasm''}

              # Assemble jsexe directory
              cp ${./wasm/shim.js} $out/all.js
              cp ${jengaLib.wasi-shim}/dist/*.js $out/
              mv $out/index.js $out/wasi-shim.js
            '';
        defaultText = lib.literalExpression "wasm-opt + post-link.mjs";
        description = "Optimized WASM frontend jsexe directory.";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.jenga.static.compress;
        description = "Whether to compress frontend WASM with brotli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let jsexe = config.jenga.frontend.wasm.optimized;
          in if jsexe == null then null
            else if config.jenga.frontend.wasm.compress
            then assets.mkAssets jsexe
            else jsexe;
        defaultText = lib.literalExpression "assets.mkAssets optimized";
        description = "Compressed WASM frontend for jenga-asset-serve-snap.";
      };
    };
  };

  config = {
    _module.args.jengaLib = jengaLib;

    inherit (jengaLib) extraCabalProject;
    # source-repository-packages disabled; using optional-packages in cabal.project instead.
    # inherit (jengaLib) source-repository-packages;

    compiler-nix-name = lib.mkDefault "ghc9141";

    overrides = [
      jengaLib.buildTypeOverride
      jengaLib.jsexeOverride
      (jengaLib.frontendDataOverride { static = hashedStatic; compressedStatic = static; })
      (jengaLib.backendDataOverride {
        static = hashedStatic;
        compressedStatic = static;
        frontendJs = frontendOutput.optimized;
        inherit compressedFrontendJs;
      })
      (jengaLib.staticManifestOverride { static = rawStatic; })
    ];

    shell.nativeBuildInputs = [
      pkgs.haskellPackages.ghcid
      jengaLib.jenga-command-pkg
    ];

    shell.shellHook = ''
      export JENGA_WASI_SHIM="${jengaLib.wasi-shim}"

      echo ""
      echo "=== jenga ==="
      echo ""
      jenga --help
      echo ""
      echo "=== jenga run ==="
      echo ""
      jenga run --help
      echo ""
      echo "=== jenga repl ==="
      echo ""
      jenga repl --help
      echo ""
      echo "=== jenga hoogle ==="
      echo ""
      jenga hoogle --help
      echo ""
    '';
  };
}
