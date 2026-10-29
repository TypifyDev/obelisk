# Jenga

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/TypifyDev/obelisk/blob/master/LICENSE)

<p align="center"><img src="docs/jenga-logo-640.png" width="50%" alt="Jenga Logo"></p>

Functional reactive web and mobile applications, with batteries included. Jenga's goal is to represent a cohesive, highly-curated set of choices that [Obsidian Systems](https://obsidian.systems/) has made for building these types of applications in a way that is extremely fast but does not compromise on production readiness.
Supports both WASM and GHCJS frontend targets, GHC 9.14, and deploys to NixOS servers or OCI containers.

- [Overview](#overview)
  - [Who should consider using it?](#who-should-consider-using-it)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Project Structure](#project-structure)
- [Development](#development)
  - [ob run](#ob-run)
  - [ob repl](#ob-repl)
  - [ob hoogle](#ob-hoogle)
- [Nix Module System](#nix-module-system)
  - [project.nix](#projectnix)
  - [Adding packages](#adding-packages)
  - [Package overrides](#package-overrides)
  - [Options](#options)
- [Building](#building)
- [Deployment](#deployment)
  - [ob deploy](#ob-deploy)
  - [NixOS Server](#nixos-server)
  - [OCI Container](#oci-container)
  - [Native Apps](#native-apps)
- [Skeleton](#skeleton)

## Overview

Jenga allows you to build high-quality web and mobile applications very quickly using [Reflex](https://reflex-frp.org/). In minutes you can go from an empty directory to an interactive application that works on web, iOS, and Android, all sharing the same Haskell codebase! Jenga's development environment also enables extremely rapid development and feedback. You can take advantage of Haskell's type system across the frontend and backend boundary. This means changes to your backend that would break your frontend are immediately detected during development and vice versa. Jenga uses Haskell's compiler to give you a complete "TODO list" of what needs to be updated.

Jenga is targeted primarily at Haskell developers who want to build high-quality web and/or mobile applications in Haskell, without the distractions of manually choosing and integrating technology for every piece of the system.

### Who should consider using it?

Jenga assumes basic knowledge of [Haskell](https://www.haskell.org/) and [Reflex/Reflex-DOM](https://reflex-frp.org/), web technologies like [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) and [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS), and a terminal shell like [Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell)). Knowledge of [Nix](https://nixos.org/) helps but is not strictly necessary.

## Installation

### One-liner (no clone required)

Create a new project directly from the remote repository:

```bash
nix-shell -p '(import (fetchTarball "https://github.com/TypifyDev/obelisk/archive/next.tar.gz") {}).jenga-command-pkg' --run 'ob init my-app'
```

### System-wide via NixOS

Add `ob` to your `configuration.nix`:

```nix
let jenga = import (builtins.fetchGit {
      url = "https://github.com/TypifyDev/obelisk";
      ref = "next";
    }) { system = "x86_64-linux"; };
in {
  environment.systemPackages = [ jenga.jenga-command-pkg ];
}
```

Then create projects from anywhere: `ob init my-app`

### From a local clone

```bash
git clone https://github.com/TypifyDev/obelisk
cd jenga
nix-shell -p '(import ./nix {}).jenga-command-pkg' --run 'ob init my-app'
```

## Quick Start

```bash
ob init my-app        # or any installation method above
cd my-app
nix-shell             # or: nix develop 'git+file:.'
ob run
```

Open http://localhost:8000 in your browser.

## Project Structure

A typical Jenga project:

```
my-app/
  backend/             # Snap backend server
  common/              # Shared types and routes
  frontend/            # Reflex-DOM frontend (library + executable)
    js/                # GHCJS cross-compilation wrapper
    wasm/              # WASM cross-compilation wrapper
  static/              # Static assets (css, images, etc.)
    generate           # Static assets generation script
    generated/         # Generated static module (jenga-generated-static)
      custom/          # Custom Setup.hs for static manifest generation
  cabal.project        # Cabal project file
  project.nix          # Nix-haskell project configuration
  default.nix          # Nix entry point
  flake.nix            # Flake entry point (optional)
```

## Development

Enter the nix shell to get all build tools (GHC, cabal, cross-compilers, hoogle):

```bash
nix-shell  # or: nix develop 'git+file:.'
```

### ob run

Watch-and-rebuild development server. Rebuilds the backend on `.hs`, `.cabal`, or `.project` file changes, or when you press Enter:

```bash
ob run
```

Disables optimizations (`-O0`) for fast rebuilds. Cross-compiles the frontend (WASM by default) in the background during each build via the backend's custom Setup.hs.

### ob repl

Start a REPL with optimizations disabled:

```bash
ob repl              # loads backend, common, frontend
ob repl lib:common   # load specific target
```

### ob hoogle

Local Hoogle documentation server:

```bash
ob hoogle            # starts on port 8080
ob hoogle -p 9090    # custom port
```

## Nix Module System

Jenga uses a [nix-haskell](https://github.com/reflex-frp/nix-haskell) module that declares all build configuration as NixOS-style options.

### project.nix

See [`docs/module.md`](docs/module.md) for Jenga-specific options and [`docs/nix-haskell`](docs/nix-haskell) for the full nix-haskell module documentation.

```nix
{ pkgs, jengaLib, ... }:
{
  name = "my-app";
  src = ./.;

  # Jenga libraries provided as source-repository-packages
  inherit (jengaLib) source-repository-packages;

  # Static assets path (hashed + compressed automatically)
  jenga.static.path = import ./static { inherit pkgs; };

  # Frontend target: "wasm" (default) or "js"
  # jenga.frontend.target = "wasm";

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs wasi32 ];
    withHoogle = true;
  };
}
```

### Adding packages

Add dependencies to the `build-depends` field in the appropriate `.cabal` file. Nix picks up the corresponding packages from the haskell.nix package set automatically.

To add extra local packages, add the package directory to the `packages:` stanza in `cabal.project` and create a `.cabal` file for it.

### Package overrides

For packages from git, use native `source-repository-package` stanzas in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/someone/some-package
  tag: abc123
```

Use `source-repository-packages` in `project.nix` for local packages or nix-thunks:

```nix
{
  source-repository-packages = {
    some-local-package = ./thunks/some-local-package;
    some-remote-package = ./thunks/some-remote-package;  # nix-thunk
  };
}
```

Use `hackage-overlays` in `project.nix` to make custom packages visible to the nix cabal solver:

```nix
{
  hackage-overlays = [
    {
      name = "some-package";
      version = "0.1.0";
      src = pkgs.fetchFromGitHub {
        owner = "someone";
        repo = "some-package";
        rev = "abc123";
        sha256 = "...";
      };
    }
  ];
}
```

Use `overrides` for haskell.nix module-level overrides (flags, patches, etc.):

```nix
{
  overrides = [
    ({ config, lib, ... }: {
      packages.some-package.flags.some-flag = true;
    })
  ];
}
```

### Options

Key module options (see [`docs/module.md`](docs/module.md) for full reference):

| Option | Default | Description |
|--------|---------|-------------|
| `jenga.static.path` | `null` | Static assets path or derivation |
| `jenga.static.compress` | `true` | Compress with brotli + gzip |
| `jenga.frontend.target` | `"wasm"` | `"wasm"` or `"js"` |
| `jenga.frontend.js.optimization.enable` | `true` | Run closure-compiler |
| `jenga.frontend.js.optimization.level` | `"ADVANCED"` | Closure optimization level |
| `jenga.frontend.wasm.optimization.enable` | `true` | Run wasm-opt |
| `jenga.frontend.wasm.optimization.level` | `"2"` | wasm-opt -O level |

## Building

### With nix

```bash
# Full production build (backend + optimized/compressed frontend)
nix-build -A serverExe.wasm
nix-build -A serverExe.js
# or: nix build 'git+file:.#serverExe.wasm'

# OCI container image
nix-build -A containerImage.wasm
# or: nix build 'git+file:.#containerImage.wasm'
```

### With cabal

```bash
cabal build backend     # native backend
cabal run backend       # run with WASM frontend cross-build
```

The backend's custom Setup.hs automatically cross-compiles the frontend (WASM or GHCJS) and links static assets during `cabal build`.

The nix shell is not strictly required, if you have `wasm32-unknown-wasi-cabal` and/or `javascript-unknown-ghcjs-cabal` in your PATH, plain `cabal build` will work without nix.

## Deployment

### ob-deploy

`ob deploy` builds a NixOS system closure, copies it to remote hosts, and activates it over SSH.

```bash
# Initialize a deployment staging directory
ob deploy init my-deploy

# Edit configuration
vim my-deploy/config/route_host       # your domain
vim my-deploy/config/admin_email      # ACME email
vim my-deploy/hosts                   # target host(s)
cp ~/.ssh/id_ed25519 my-deploy/secrets/ssh_key
chmod 600 my-deploy/secrets/ssh_key

# Deploy
ob deploy push my-deploy
```

The staging directory is separate from your project (SSH keys stay out of your repo). `push` resolves the current branch to a git rev, builds the NixOS system, transfers via `nix-copy-closure`, syncs config via `rsync`, and activates with kernel change detection (switch vs reboot).

### NixOS Server

For manual NixOS configuration (without `ob deploy`), import the server module directly:

```nix
{ config, ... }:
let
  app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  imports = [ app.serverModule ];

  services.jenga = {
    enable = true;
    exe = app.serverExe.wasm;
    routeHost = "myapp.example.com";
    enableHttps = true;
    adminEmail = "admin@example.com";
  };
}
```

This configures nginx (with WebSocket proxy), ACME/Let's Encrypt, systemd service with auto-restart, and firewall rules.

### OCI Container

```bash
nix-build -A containerImage.wasm
# or: nix build 'git+file:.#containerImage.wasm'

# Load and run with podman or docker
podman load < result
podman run -p 8000:8000 my-app:latest
```

### Native Apps

The WASM frontend compiles to a static set of web assets (HTML, JS, WASM) that can be wrapped into native desktop and mobile applications using existing web-to-native tools. Since the frontend runs as compiled WASM rather than interpreted JavaScript, you get native-level performance.

[CapacitorJS](https://capacitorjs.com/) is one such tool, it wraps your web app in a native shell for iOS, Android, and Electron, and provides JavaScript hooks to interact with the operating system (camera, filesystem, push notifications, etc.). Your Haskell frontend code calls these APIs via GHC's JavaScript FFI.

Other options include [Tauri](https://tauri.app/) for desktop and [ElectronJS](https://www.electronjs.org/).


## Skeleton

The `skeleton/` directory provides a minimal project template. Use `ob init` to create a new project from it (see [Installation](#installation)).

The skeleton includes:
- Backend with Snap server and asset serving
- Frontend with Reflex-DOM and route handling
- Common route types
- WASM and GHCJS cross-compilation wrappers
- Static asset generation pipeline
- Nix and flake configuration


## Frequently Asked Questions (FAQ)

Refer to [FAQ](FAQ.md).


## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.
