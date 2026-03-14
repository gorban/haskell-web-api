# Haskell Development Environment Setup

This guide provides detailed instructions for setting up a Haskell development environment on MacOS and
Linux (e.g. Ubuntu or Fedora).

For Windows, see deprecation in [changelog v0.1.0.1](CHANGELOG.md#v0101).

## GHCup Prerequisites

Fulfill any system requirements outlined by the
[GHCup official instructions](https://www.haskell.org/ghcup/install/#system-requirements).

### Example: Ubuntu Prerequisites

Commands to install prerequisites in Ubuntu. Also tested in WSL2 on Windows:

```bash
sudo apt update
sudo apt upgrade -y
sudo apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev git
```

### Example: Fedora Prerequisites

Prerequisites for Fedora provided by Distrobox container, making them compatible whether you are using an
Atomic/immutable edition of Fedora or not. So first, if you don't have [Distrobox](https://distrobox.it/):
```bash
curl -s https://raw.githubusercontent.com/89luca89/distrobox/main/install | sudo sh
```

Example Distrobox container definition, e.g. save as `distrobox.ini`:
```ini
[devbox]
additional_packages="gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl git"
image="registry.fedoraproject.org/fedora:latest"
```

Then to assemble and run the container:
```bash
distrobox assemble create --name haskellbox --file distrobox.ini
distrobox enter haskellbox
```

### Example: MacOS Prerequisites

According to the [GHCup official instructions](https://www.haskell.org/ghcup/install/#system-requirements),
simply running the GHCup installer below should install some prerequisites, but it notes:

> On Darwin M1 you might also need a working llvm installed (e.g. via brew) and have the toolchain exposed in PATH.

## Install GHCup

Install GHCup itself; answering as provided any questions asked during execution (some answers or
questions may be different or omitted entirely depending on target operating system):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

1.  Press ENTER to accept the only available ghcup install location ($HOME/.ghcup).
2.  Press ENTER to acknowledge that the System requirements had just been installed.
3.  Use 'D' (or 'G' on MacOS and Fedora) to use the Default (GHCup maintained) binary distribution
    "channel".
4.  Answer 'N' to disable the pre-releases channel.
5.  Answer 'Y' to enable the cross channel (GHCJS, WASM, etc.).
6.  Answer 'P' to automatically add (prepend) the required PATH variable to "$HOME/.bashrc" (or
    "$HOME/zshrc" on MacOS).
7.  Answer 'Y' to install HLS (Haskell Language Server), we need it for IDE support.
8.  Answer 'Y' to install stack (we might be able to build our project with Cabal alone, but other
    projects might need stack).
9.  Answer 'Y' to enable better integration of stack with GHCup (so stack uses GHCup's own GHC versions).
10. MacOS only: look for security warnings installing with GHCup. They may appear as popups like "llc" Not
    Opened - Apple could not verify "llc" ...
    1. Select "Done" to not move each file to the Trash.
    2. After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have to
       unblock the last executable in System Preferences:
       1. Open System Preferences -> Privacy & Security -> scroll down to Security section.
       2. There should be a message like "llc" was blocked to protect your Mac.
       3. Click Allow Anyway.
    3. Repeat for other files like "opt", etc. until no more popups appear.
    4. If any popups appeared, you may want to kill the ghcup installer (Ctrl+C), clean out
       "$HOME/.ghcup", and re-run it, so that it can proceed with installing with now unblocked
       executables.
    5. On rerun you may get popups again for the same executables like Open "llc"? This time, click Open
       Anyway for each. It may also ask for your password to allow the installation to proceed.
11. For the new PATH variable to take effect:
    1. You can immediately get `ghcup` on the system PATH by running command:
       ```bash
       . "$HOME/.ghcup/env"
       ```
    2. To ensure it comes up automatically with a terminal restart, GHCup may have tried to update
       `$HOME/.bashrc`(or `$HOME/.zshrc` on MacOS), but it may not respect if you are using a non-default
       terminal (e.g. Zsh on Ubuntu / Fedora). So you may need to copy this line (e.g. to `$HOME/.zshrc`):
       ```bash
       [ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
       ```

## Install Haskell Toolchain

In the past, the latest versions had not all been compatible with each other or an otherwise compatible
version set segfaulted on specific operating systems. So we maintain a list of known compatible versions
here:
- GHC 9.14.1
- Cabal 3.16.1.0
- Stack 3.9.1
- HLS 2.13.0.0

Run `ghcup tui` to manage installed versions of GHC, Cabal, Stack, HLS, etc.
1. Immediately change GHC to the latest version that is "hls-powered" (compatible with Haskell Language
   Server) by selecting it in the list and pressing 's'. On MacOS, I did not see any "hls-powered"
   versions, so just select a version that would be "hls-powered" on Linux (can test via a Linux VM or
   another machine), or use GHC 9.14.1 if it's still current enough.
   1. Answer 'Y' for whether you would like to install the selected version if it is not already installed.
   2. MacOS only: look for security warnings installing with GHCup. They may appear as popups like
      "clang-18" Not Opened - Apple could not verify "clang-18" ...
      1. Select "Done" to not move each file to the Trash.
      2. After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have
         to unblock the last executable in System Preferences:
         1. Open System Preferences -> Privacy & Security -> scroll down to Security section.
         2. There should be a message like "llc" was blocked to protect your Mac.
         3. Click Allow Anyway.
      3. Repeat for other files until no more popups appear.
   3. Press enter if asked to "continue" after each successful install.
      1. MacOS only: if any popups appeared, you may want to use undo by setting the GHC version to the
         "recommended" one with 's', selecting the newer one again and uninstalling with 'u', then
         pressing 's' to install and set it again, so that it can proceed with installing with now
         unblocked executables.
      2. On rerun you may get popups again for the same executables like Open "clang-18"? This time, click
         Open Anyway for each. It may also ask for your password to allow the installation to proceed.
2. You should be returned back into `ghcup tui`. For cabal, HLS, and stack, select the latest versions and
   press 's' to set them as default.
   1. Similarly answer 'Y' for whether you would like to install the selected version if it is not already
      installed.
   2. Also, press enter after each success and finally answer 'q' to exit ghcup.
3. Also, install with 'i', but do not 's'et the latest GHC version with a JavaScript backend. It may not be
   a corresponding version to the latest "hls-powered" GHC. For example if 9.14.1 is "hls-powered", the
   latest with a JavaScript backend might be:\
   `javascript-unknown-ghc-9.12.2`
   - **Note** any messages that is pauses on right after you start the 'i'nstall like:
     > [ Warn  ] To use this bindist, you have to use emscripten version 3.1.74

     We will need to install this exact emscripten version.
4. Check the versions with `ghc --version`, `cabal --version`,
   `haskell-language-server-wrapper --version`, and `stack --version` for last installed, respectively.

### Emscripten for JavaScript Backend for GHC

We need to install the exact emscripten version messaged by the `ghcup tui` when we tried to install
`javascript-unknown-ghc-9.12.2`. For example, change any 3.1.74 to the exact version from the message:

1. To install emscripten:
   ```bash
   cd "$HOME"
   git clone https://github.com/emscripten-core/emsdk.git --depth 1 --branch 3.1.74 emsdk-3-1-74
   cd emsdk-3-1-74
   ./emsdk install latest
   ./emsdk activate latest
   source ./emsdk_env.sh
   ```
2. `emsdk` command should then be available in the terminal, but to persist we need to update any shell
   startup scripts:
   ```
   source "$HOME/emsdk-3-1-74/emsdk_env.sh"
   ```
   1. If `bash` is your shell (default in Ubuntu and Fedora), add this line to "$HOME/.bash_profile".
   2. If `zsh` is your shell (default in MacOS), add this line to "$HOME/.zshrc".
