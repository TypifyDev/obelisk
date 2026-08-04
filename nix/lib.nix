{ system }:

let src = ../.;

    # Filesystem path to the jenga checkout (not a store path).
    # Used by jenga init to create deps/jenga symlinks in spawned projects.
    jengaRoot = toString ../.;

    # Copy a standalone dep repo to the store, stripping its .git directory.
    # Needed because the nix-haskell source-repository-package builder does
    # `git init && git commit` which fails if .git already exists.
    depSrc = name: builtins.path {
      inherit name;
      path = ../deps + "/${name}";
      filter = p: _: baseNameOf p != ".git";
    };

    nix-haskell = import ../deps/nix-haskell { inherit system; };

    # Standalone project built only to produce the manifest generator executable.
    jenga-asset-manifest = nix-haskell {
      name = "jenga-asset-manifest";
      src = src + "/lib/asset/manifest";
    };

    jenga-asset-manifest-generate =
      "${jenga-asset-manifest.haskell-nix.project.hsPkgs.jenga-asset-manifest.components.exes.jenga-asset-manifest-generate}/bin/jenga-asset-manifest-generate";

    # nix-thunk CLI, built with haskell.nix directly using our GHC 9.14 pin.
    # Uses allow-newer for base/template-haskell bounds.
    haskell-nix-raw = import ../deps/nix-haskell/pins/haskell.nix {};
    haskell-nix-pkgs = import haskell-nix-raw.sources.nixpkgs
      (haskell-nix-raw.nixpkgsArgs // { inherit system; });
    nix-thunk-src = import ../deps/nix-haskell/deps/nix-thunk/thunk.nix;
    nix-thunk-project = haskell-nix-pkgs.haskell-nix.project {
      src = haskell-nix-pkgs.haskell-nix.haskellLib.cleanGit {
        name = "nix-thunk";
        src = nix-thunk-src;
      };
      compiler-nix-name = "ghc9122";
      cabalProjectLocal = ''
        allow-newer:
            *:base
          , *:template-haskell
      '';
      modules = [
        ({ config, lib, ... }:
          let optional = lib.filterAttrs (name: _: config.packages ? ${name});
          in {
            packages = optional {
              cli-git.components.library.build-tools = [ haskell-nix-pkgs.git ];
              cli-nix.components.library.build-tools = [
                haskell-nix-pkgs.nix-prefetch-git
                haskell-nix-pkgs.nix
              ];
            };
          })
      ];
    };
    nix-thunk-cli = nix-thunk-project.nix-thunk.components.exes.nix-thunk;

    # Clean skeleton for embedding as Cabal data-files.
    # Strips build artifacts, absolute symlinks (nix-shell leftovers),
    # and the cyclic deps/jenga -> ../../ link.
    cleanSkeleton = pkgs.runCommand "clean-skeleton" {} ''
      cp -rP ${src}/skeleton $out
      chmod -R +w $out

      # Remove absolute symlinks (build artifacts from nix-shell)
      find $out -type l | while read link; do
        target=$(readlink "$link")
        case "$target" in
          /*) rm -f "$link" ;;
        esac
      done

      # Remove cyclic deps/jenga -> ../../ (jenga init creates the proper symlink)
      rm -f $out/deps/jenga

      # Remove build artifacts that leaked into the source tree
      rm -rf $out/dist-wasm $out/dist-newstyle $out/result $out/ob-run-output.txt
    '';

    # Standalone build of the jenga CLI.
    # build-tools adds binaries to nativeBuildInputs so staticWhich finds them at compile time.
    # preBuild on the library populates data/skeleton for Paths_jenga_command.
    jenga-command = nix-haskell {
      name = "jenga-command";
      src = src + "/lib/command";
      overrides = [
        {
          # staticWhich resolves tool paths at compile time (TH splices in library).
          # build-tools adds these to nativeBuildInputs → PATH during compilation.
          packages.jenga-command.components.library.build-tools = [
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hoogle
            pkgs.git
            pkgs.nix
            pkgs.openssh
            pkgs.rsync
            nix-thunk-cli
          ];
          # Marker file so Cabal creates the data directory (for getDataDir).
          # Package-level preBuild applies to both lib and exe components.
          packages.jenga-command.preBuild = ''
            mkdir -p data
            touch data/.keep
          '';
          # Install skeleton and Jenga root path into the data directory
          # for Paths_jenga_command.getDataDir.
          packages.jenga-command.components.library.postInstall = ''
            for datadir in $data/share/*/*/jenga-command-*; do
              cp -r ${cleanSkeleton} "$datadir/skeleton"
              echo -n "${jengaRoot}" > "$datadir/jenga-root"
            done
          '';
        }
      ];
    };

    jenga-command-pkg = jenga-command.haskell-nix.project.hsPkgs.jenga-command.components.exes.jenga;

    jenga = "${jenga-command-pkg}/bin/jenga";

    # Vendored browser WASI shim for WASM frontend.
    wasi-shim = builtins.fetchTarball {
      url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz";
      sha256 = "0j8xls87rl2gjr12z4k6jsmc65idrbpilcg9277mlhcrg1l9qdsz";
    };

    # Keep only overrides whose package name exists in the project,
    # so overrides for absent packages are silently skipped.
    mkOptionalPackages = { config, lib }:
      lib.filterAttrs (name: _: config.packages ? ${name});

    assets = import ./assets.nix { nixpkgs = (import ../deps/nix-haskell/pins/nixpkgs { inherit system; }); };

    docs = import ./docs.nix { inherit system; };

    pkgs = import ../deps/nix-haskell/pins/nixpkgs { inherit system; };

    serverModule = ./server.nix;

    # haskell-src-exts uses legacy Build-Tools: happy >= 1.19 which cabal
    # resolves independently from constraints, pulling in happy-2.2 alongside
    # ghc-lib-parser's happy-2.1.7.  Upgrade to build-tool-depends so
    # constraints: happy < 2.2 applies and both use happy-2.1.7.
    haskell-src-exts-patched = pkgs.runCommand "haskell-src-exts" {} ''
      cp -r ${builtins.fetchTarball {
        url = "https://hackage.haskell.org/package/haskell-src-exts-1.23.1/haskell-src-exts-1.23.1.tar.gz";
        sha256 = "144q88agqqfpc8z1h2jr6mgx5xs72wxkrx4kbpsfg9cza3jm9fbx";
      }} $out
      chmod -R +w $out
      sed -i 's/Cabal-Version:.*>=.*1.10/cabal-version: 2.0/' $out/haskell-src-exts.cabal
      sed -i 's/Build-Tools:.*happy >= 1.19/build-tool-depends: happy:happy >= 1.19 \&\& < 2.2/' $out/haskell-src-exts.cabal
    '';

in rec {
  inherit src jenga jenga-command-pkg jenga-asset-manifest-generate wasi-shim assets docs serverModule;

  frontendJs = config:
    config.haskell-nix.project.projectCross.ghcjs.hsPkgs.frontend.components.exes.frontend;

  frontendWasm = config:
    config.haskell-nix.project.projectCross.wasi32.hsPkgs.frontend.components.exes.frontend;

  source-repository-packages = {
    jenga-asset-manifest = src + "/lib/asset/manifest";
    jenga-asset-serve-snap = src + "/lib/asset/serve-snap";
    jenga-backend = src + "/lib/backend";
    jenga-executable-config-inject = src + "/lib/executable-config/inject";
    jenga-executable-config-lookup = src + "/lib/executable-config/lookup";
    jenga-frontend = src + "/lib/frontend";
    jenga-route = src + "/lib/route";
    jenga-snap-extras = src + "/lib/snap-extras";
    jenga-setup = src + "/lib/setup";
    tabulation = src + "/lib/tabulation";

    jenga-auth-common = src + "/lib/jenga-auth-common";
    jenga-auth-backend = src + "/lib/jenga-auth-backend";
    # jenga-auth-frontend — needs ClasshSS, reflex-classhss, staticAssets, templates
    # jenga-auth-frontend = src + "/lib/jenga-auth-frontend";

    obelisk-oauth-common = src + "/deps/obelisk-oauth/common";
    obelisk-oauth-backend = src + "/deps/obelisk-oauth/backend";
    lamarckian = src + "/lib/lamarckian";
    lamarckian-core = src + "/lib/lamarckian-core";
    scrappy-core = src + "/lib/scrappy-core";
    scrappy-template = src + "/lib/scrappy-template";
    monad-tree = src + "/lib/monad-tree";
    ClasshSS = src + "/lib/ClasshSS";
    reflex-classhss = src + "/lib/reflex-classhss";
    reflex-effectful = src + "/lib/reflex-effectful";
    grid-effect = src + "/lib/grid-effect";
    jenga-route-servant = src + "/lib/jenga-route-servant";
    jenga-backend-servant = {
      src = src + "/lib/jenga-backend-servant";
      condition = "!(arch(javascript) || arch(wasm32))";
    };
    servant-snap = {
      src = depSrc "servant-snap";
      condition = "!(arch(javascript) || arch(wasm32))";
    };
    servant-jsaddle = depSrc "servant-jsaddle";

    reflex-dom = src + "/deps/reflex-dom/reflex-dom";
    reflex-dom-core = src + "/deps/reflex-dom/reflex-dom-core";
    chrome-test-utils = src + "/deps/reflex-dom/chrome-test-utils";

    haskell-src-exts = haskell-src-exts-patched;

    # --- rhyolite ecosystem ---
    rhyolite-common = src + "/deps/rhyolite/common";
    rhyolite-backend = src + "/deps/rhyolite/backend";
    rhyolite-frontend = src + "/deps/rhyolite/frontend";
    rhyolite-widgets = src + "/deps/rhyolite/widgets";
    semimap = src + "/deps/rhyolite/semimap";
    rhyolite-beam-db = src + "/deps/rhyolite/beam/db";
    rhyolite-beam-orphans = src + "/deps/rhyolite/beam/orphans";
    rhyolite-beam-task-worker-types = src + "/deps/rhyolite/beam/task/types";
    rhyolite-beam-task-worker-backend = src + "/deps/rhyolite/beam/task/backend";
    rhyolite-notify-listen = src + "/deps/rhyolite/notify-listen/notify-listen";
    rhyolite-notify-listen-beam = src + "/deps/rhyolite/notify-listen/notify-listen-beam";
    rhyolite-email = src + "/deps/rhyolite/email";
    mime-mail-orphans = src + "/deps/rhyolite/email/mime-mail-orphans";
    rhyolite-account-types = src + "/deps/rhyolite/account/types";
    rhyolite-account-backend = src + "/deps/rhyolite/account/backend";
    signed-data = src + "/deps/rhyolite/signed-data/signed-data";
    signed-data-clientsession = src + "/deps/rhyolite/signed-data/signed-data-clientsession";
    psql-simple-class = src + "/deps/rhyolite/psql-extras/psql-simple-class";
    psql-simple-beam = src + "/deps/rhyolite/psql-extras/psql-simple-beam";
    psql-serializable = src + "/deps/rhyolite/psql-extras/psql-serializable";

    # --- rhyolite dep thunks (cloned into deps/) ---
    # Standalone repos use depSrc to strip .git; subdirectory refs use src +.
    vessel = depSrc "vessel";
    beam-core = src + "/deps/beam/beam-core";
    beam-postgres = src + "/deps/beam/beam-postgres";
    beam-migrate = src + "/deps/beam/beam-migrate";
    beam-automigrate = depSrc "beam-automigrate";
    gargoyle = src + "/deps/gargoyle/gargoyle";
    gargoyle-postgresql = src + "/deps/gargoyle/gargoyle-postgresql";
    gargoyle-postgresql-nix = src + "/deps/gargoyle/gargoyle-postgresql-nix";
    gargoyle-postgresql-connect = src + "/deps/gargoyle/gargoyle-postgresql-connect";
    monoid-map = depSrc "monoid-map";
    bytestring-aeson-orphans = depSrc "bytestring-aeson-orphans";
    postgresql-simple-interpolate = depSrc "postgresql-simple-interpolate";
    postgresql-lo-stream = depSrc "postgresql-lo-stream";
  };

  extraCabalProject = [
    (builtins.readFile (src + "/lib/cabal.project.config"))
  ];

  inherit mkOptionalPackages;

  # Generate Jenga.Generated.Static module before building jenga-generated-static.
  # Needed because build-type is overridden to Simple (no Setup.hs runs in nix).
  staticManifestOverride = { static }: { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages =
        let preBuild = lib.optionalString (static != null) ''
          rm -rf src
          mkdir -p src
          ${jenga-asset-manifest-generate} --module-only ${static} . Jenga.Generated.Static data/static
        '';
        in optional {
          jenga-generated-static.components.library.preBuild = preBuild;
          jenga-generated-static-custom.components.library.preBuild = preBuild;
        };
    };

  # Force Simple build type so haskell.nix doesn't run a Setup.hs configure step.
  buildTypeOverride = { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages = optional {
        backend.package.buildType = lib.mkOverride 75 "Simple";
        frontend.package.buildType = lib.mkOverride 75 "Simple";
        frontend-js.package.buildType = lib.mkOverride 75 "Simple";
        frontend-wasm.package.buildType = lib.mkOverride 75 "Simple";
        jenga-generated-static.package.buildType = lib.mkOverride 75 "Simple";
        jenga-generated-static-custom.package.buildType = lib.mkOverride 75 "Simple";
      };
    };

  # Copy frontend.jsexe directory into $out/bin after GHCJS build.
  jsexeOverride = { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages = optional {
        frontend.components.exes.frontend.postInstall = ''
          if [ -d dist/build/frontend/frontend.jsexe ]; then
            cp -r dist/build/frontend/frontend.jsexe $out/bin/
          fi
        '';
      };
    };

  # Symlink static assets into frontend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  frontendDataOverride = { static ? null, compressedStatic ? null }:
    ({ config, lib, pkgs, ... }:
      let optional = mkOptionalPackages { inherit config lib; };
          dataDir = if config.packages ? frontend
            then config.packages.frontend.package.dataDir
            else "";
      in {
        packages = optional {
          frontend.components.library.preBuild = lib.optionalString (dataDir != "") ''
            mkdir -p ${dataDir}
            ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
            ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
          '';
          frontend.components.library.postInstall = ''
            for datadir in $data/share/*/*/frontend-*; do
              ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
              ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
            done
          '';
        };
      }
    );

  # Symlink static assets and frontend jsexe into backend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  backendDataOverride = { static ? null, compressedStatic ? null, frontendJs ? null, compressedFrontendJs ? null }:
    ({ config, lib, pkgs, ... }:
      let optional = mkOptionalPackages { inherit config lib; };
          dataDir = if config.packages ? backend
            then config.packages.backend.package.dataDir
            else "";
      in {
        packages = optional {
          backend.components.library.preBuild = lib.optionalString (dataDir != "") ''
            mkdir -p ${dataDir}
            ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
            ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
            ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} ${dataDir}/frontend.jsexe'' else ""}
            ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} ${dataDir}/frontend.jsexe.assets'' else ""}
          '';
          backend.components.library.postInstall = ''
            for datadir in $data/share/*/*/backend-*; do
              ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
              ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
              ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} "$datadir/frontend.jsexe"'' else ""}
              ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} "$datadir/frontend.jsexe.assets"'' else ""}
            done
          '';
        };
      }
    );

  # Assemble a flat deployment directory for the given frontend target.
  # Contains the backend binary, compressed static/frontend assets.
  mkServerExe = { proj, target }:
    let targetProj = proj.override { jenga.frontend.target = target; };
        backendExe = targetProj.hsPkgs.backend.components.exes.backend;
        compressedStatic = targetProj.config.jenga.static.compressed;
        compressedFrontend = targetProj.config.jenga.frontend.${target}.compressed;
    in pkgs.runCommand "server-exe" {} ''
      mkdir $out
      set -eux
      ln -s ${backendExe}/bin/* $out/
      ${pkgs.lib.optionalString (compressedStatic != null) ''
        ln -s ${compressedStatic} $out/static.assets
      ''}
      ${pkgs.lib.optionalString (compressedFrontend != null) ''
        ln -s ${compressedFrontend} $out/frontend.jsexe.assets
      ''}
    '';

  # Build an OCI container image (podman/docker) for the given frontend target.
  mkContainerImage = { proj, target, name ? proj.config.name, tag ? "latest" }:
    let serverExe = mkServerExe { inherit proj target; };
        appDir = pkgs.runCommand "app-dir" {} ''
          mkdir -p $out/app
          ln -s ${serverExe}/* $out/app/
        '';
    in pkgs.dockerTools.buildLayeredImage {
      inherit name tag;
      contents = [ appDir pkgs.cacert pkgs.gnutar pkgs.glibcLocales ];
      config = {
        Cmd = [ "/app/backend" "--port=8000" ];
        ExposedPorts = { "8000/tcp" = {}; };
        WorkingDir = "/app";
        Env = [
          "LANG=en_US.UTF-8"
          "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
        ];
      };
    };

}
