# haskell-web-api

## Components

packages/
  - web-api: The main application package providing currently only an putStrLn (not a real web API yet).
  - test-lib: A library with shared test utilities and custom preprocessors.
  - hspec-expectations-match: A fork of the third-party package `hspec-expectations-match` with local changes to get it to work with current versions of Template Haskell.
    - No public GitHub repo, so not sure if we can get these changes upstreamed to Hackage:\
      <https://hackage.haskell.org/package/hspec-expectations-match>

## Prerequisites

The following is a detailed guide to set up a Haskell development environment on Windows, MacOS, and Linux (e.g. WSL2 / Ubuntu).

- Haskell GHC Compiler and Cabal
- (recommended) IDE/editor with Haskell support (e.g. Visual Studio Code with Haskell extension)
  - (recommended IDE-helpers) Haskell Language Server (HLS), hlint, Haskell debugger (hdb), and Haskell Debugger
    - TODO: debugger doesn't currently work on Windows until issue is resolved:\
      <https://github.com/well-typed/haskell-debugger/issues/149>
      > Consider updating these prerequisites for Windows to recommend the Haskell GHCi Debug Adapter instead.
- (optional) Stack (some third party projects might require it)

### Windows

- Windows: Install [GHCup](https://www.haskell.org/ghcup/)\
  (non-admin PowerShell terminal):
  ```powershell
  Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
  ```
  - Asks for an install location, press Enter to accept the default (C:\\)
  - Asks for a Cabal directory, press Enter to accept the default (C:\\cabal)
  - Answer 'Y' to install HLS (Haskell Language Server), we need it for IDE support
  - Answer 'Y' to install stack (we might be able to build our project with Cabal alone, but other projects might need stack)
  - Asks for whether you want to create Desktop shortcuts, answer 'Y' or 'N' as you prefer
  - Answer 'Y' to install MSys2 (I have noticed better compatibility with some libraries)
  - Usage after installation is done:
    - Set up a [Windows Terminal profile](https://www.msys2.org/docs/terminals/) or use the "Haskell GHCup" desktop shortcut
      - If you copy the profile JSON from the MSys2 docs, make sure to change some values:
        - For `<<ORIGINAL_INSTALL_LOCATION>>`, fill in the value provided for install location when you installed GHCup above, e.g. `C:/`, so the whole starting directory might be `C:/ghcup/msys64/home/%USERNAME%`.
        - For `<<MSYS_COMMAND>>`, use `-ucrt64` for `"name": "UCRT64 / MSYS2"` profile and use `-msys` for `"name": "MSYS / MSYS2"` profile.
        - `"startingDirectory": "<<ORIGINAL_INSTALL_LOCATION>>/ghcup/msys64/home/%USERNAME%"`
        - `"commandline": "<<ORIGINAL_INSTALL_LOCATION>>/ghcup/msys64/msys2_shell.cmd -defterm -here -no-start <<MSYS_COMMAND>>"`
        - Icon has two options, for a mintty icon or purple msys2 icon (look at the files on disk to see which you prefer):
          - `"icon": "<<ORIGINAL_INSTALL_LOCATION>>/ghcup/msys64/usr/share/icons/hicolor/256x256/apps/mintty.png"`
          - `"icon": "<<ORIGINAL_INSTALL_LOCATION>>/ghcup/msys64/msys2.ico"`
    - Now run `ghcup tui` to manage installed versions of GHC, Cabal, Stack, HLS, etc.
      - Immediately change GHC to the latest version that is "hls-powered" (compatible with Haskell Language Server) by selecting it in the list and pressing 's'.
        - Answer 'Y' for whether you would like to install the selected version if it is not already installed.
        - It got stuck for me after install on "Success" and "Press enter to continue", but it was installed successfully when I restarted my Msys2 shell and ran `ghc --version`.
      - Go back into `ghcup tui`. For cabal, HLS, and stack, select the latest versions and press 's' to set them as default.
        - Similarly answer 'Y' for whether you would like to install the selected version if it is not already installed.
        - Also, if it gets stuck again after install, just restart the Msys2 shell and check the versions with `cabal --version`, `haskell-language-server-wrapper --version`, and `stack --version` for last installed, respectively.
      - In the past, the latest versions had not all been compatible with each other or an otherwise compatible version set segfaulted on Windows (and sometimes only when not compiled with the Msys2 toolchain). So we maintain a list of known compatible versions here:
        - GHC 9.12.2
        - Cabal 3.16.1.0
        - Stack 3.9.1
        - HLS 2.12.0.0
      - If the HLS version is tied to a mismatching GHC version like `2.12.0.0 (GHC: 9.8.4)`, you can have ghcup install a different HLS version that matches your GHC version.
        - First, make sure to close any IDE/editor that might be using HLS (otherwise, you'll get an error like `ghcup.exe: C:\ghcup\bin\haskell-language-server-9.12.2~2.12.0.0.exe: DeleteFile "\\\\?\\C:\\ghcup\\bin\\haskell-language-server-9.12.2~2.12.0.0.exe": permission denied (Access is denied.)`).
        - Then, run:
          ```powershell
          ghcup compile hls -v 2.12.0.0 --ghc 9.12.2
          ```
      - If the highest GHC version that is "hls-powered" is not at least 9.14, also install the "latest" version of GHC with "i" (but don't set it as default with "s"), as the better haskell-debugger (hdb) requires at least GHC 9.14.
    - Stack seems to have its own GHC version configured outside of ghcup, so if you use stack, you might have to configure the GHC version in `stack.yaml` files.
      - To check what GHC version stack is using globally, run:
        ```powershell
        stack ghc -- --version
        ```
      - I made a naive attempt to set the resolver to the same GHC version as above, but it produced an invalid stack.yaml file. My command:
        ```powershell
        stack config set resolver ghc-9.12.2
        ```
      - Either way, the above stack command at least created the `stack.yaml` file if it does not already exist. Find it at:\
        `C:\Users\bruck\AppData\Roaming\stack\global-project\stack.yaml`
        Ensure it contains the matching GHC version as set via `ghcup tui` above, e.g. I had to change just one line from:
        ```yaml
        resolver: compiler: ghc-9.12.2
        ```
        To:
        ```yaml
        resolver: ghc-9.12.2
        ```
    - In the future, to install system libraries and update msys2/mingw64, open the "Mingw haskell shell" and the "Mingw package management docs" desktop shortcuts.
- Haskell debugger (hdb), supported by Visual Studio Code haskell debugger extension:
  - You must use the higher GHC version between the one set as default in ghcup and any 9.14+ version you installed additionally for hdb support
    - Only if the highest "hls-powered" GHC version was below 9.14+, fill in the "latest" version in the following command and run it (skip if the "hls-powered" GHC version is already 9.14+):
      ```powershell
      ghcup set ghc <<ghc_version>>
      ```
      E.g. for GHC 9.14.1:
      ```powershell
      ghcup set ghc 9.14.1
      ```
  - Now fill in the same, higher GHC version to install haskell-debugger. **Note:** if not on Windows, add `--enable-executable-dynamic` to the end of the command:
    ```powershell
    cabal install -w ghc-<<ghc_version>> haskell-debugger "--installdir=$env:APPDATA\cabal\bin" --allow-newer=base,time,containers,ghc,ghc-bignum,template-haskell
    ```
    E.g. for GHC 9.14.1:
    ```powershell
    cabal install -w ghc-9.14.1 haskell-debugger "--installdir=$env:APPDATA\cabal\bin" --allow-newer=base,time,containers,ghc,ghc-bignum,template-haskell
    ```
  - If you had to `ghcup set ghc` to a different version than your default, you need to switch back to the "hls-powered" version with:
    ```powershell
    ghcup set ghc <<hls_powered_ghc_version>>
    ```
    E.g. for GHC 9.12.2:
    ```powershell
    ghcup set ghc 9.12.2
    ```
    - If this is the case, you need the higher version when you want to use the debugger, and the "hls-powered" version for normal development (so you get proper language server support). For the debugger to work, you may also need to update the `cabal.project` file in the repository root to specify the higher GHC version: `with-compiler: ghc-<<higher_version>>`, e.g.:
      ```
      with-compiler: ghc-9.14.1
      ```
      - And update any `*.cabal` files for any `base` version bounds that might prevent building with the higher GHC version, e.g. change from `build-depends:    base ^>= 4.21.0.0` to:
        ```
        build-depends:    base ^>= 4.22.0.0
        ```
      - Don't forget to undo changes to `cabal.project` and `*.cabal` files when you want to go back to normal development with HLS.
  - You might still have issues trying to debug if you are on Windows, e.g. I am seeing:
    ```
    haskell-debugger did not signal readiness in time
    ```
    You may have to wait till resolution of issue:
    - <https://discourse.haskell.org/t/the-haskell-debugger-for-ghc-9-14/13499/7>
    - <https://github.com/well-typed/haskell-debugger/issues/149#issuecomment-3733646010>
    - Workaround: use WSL2 (Windows Subsystem for Linux) instead of native Windows installation.
- IDE: Install [Visual Studio Code](https://code.visualstudio.com/) or another Haskell-compatible IDE/editor.
  - In VS Code, install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) by the Haskell Foundation and [Haskell Debugger](https://marketplace.visualstudio.com/items?itemName=Well-Typed.haskell-debugger-extension) by Well-Typed.
    - You might have to "Switch to Pre-Release Version" for compatibility with the latest GHC/HLS versions.
    - If it gives an option to "Restart Extensions", do so.
- For Haskell Language Server (HLS), to be able to process our files with custom preprocessors, as well as debug support, we need to install `spec-preprocessor`, `hspec-discover`, `ghci-dap`, `haskell-debug-adapter`, and `hlint` globally.
  - **NOTE:** some of the following dependencies will not be necessary anymore after the switch from Haskell GHCi Debug Adapter to Haskell Debugger extension.
  - In a PowerShell terminal (non-admin), navigate to packages/test-lib directory and run:
    ```bash
    cabal update
    cabal install ghci-dap haskell-debug-adapter hspec-discover hlint "--installdir=$env:APPDATA\cabal\bin" --overwrite-policy=always
    cabal build test-lib:spec-preprocessor
    Copy-Item (cabal list-bin test-lib:exe:spec-preprocessor | Select-Object -First 1) "$env:APPDATA\cabal\bin" -Force
    ```
  - And in case it matters the last known working version of hlint, you can find it from running the following command; example compatible version is v3.10:
    ```powershell
    cabal exec $env:APPDATA\cabal\bin\hlint -- --version
    ```
  - Edit system PATH environment variable to include `%APPDATA%\cabal\bin` if it is not already included.
    - You can check whether it is included by running `echo $env:PATH` in a PowerShell terminal and looking for the path.
    - Add it in another PowerShell (requires admin):
      ```powershell
      [Environment]::SetEnvironmentVariable("Path", $env:Path + ";$env:APPDATA\cabal\bin", [EnvironmentVariableTarget]::Machine)
      ```
    - If you had to add it, restart your IDE/editor and terminal(s) to pick up the change.
  - Additionally, in VS Code, I was seeing error logs in output `Haskell (haskell-web-api)` like `semanticTokens is disabled globally in your config.`. See issue:\
    <https://github.com/haskell/haskell-language-server/issues/4081>
    - To fix this, open VS Code settings (Ctrl+,), search for `semantic tokens`, and ensure that `Haskell › Plugin › Semantic Tokens: Global On` (description: `Enables semanticTokens plugin`) is checked.
    - You may have to restart the Haskell Language Server for the change to take effect. In VS Code command palette (Ctrl+Shift+P), run `Haskell: Restart Haskell LSP Server`.
  - **NOTE:** if you make any updates to `spec-preprocessor`, you need to re-run the `cabal build cabal build test-lib:spec-preprocessor` and `Copy-Item ...` commands if you want the updated version to be the one used when you GHCi debug.

## MacOS / Linux (e.g. WSL2 / Ubuntu)

### WSL2 on Windows

If you want to do a Linux install but you have Windows, consider a WSL2 install.
- This works around a (currently unresolved) issue for debugging in VS Code on Windows:\
  <https://github.com/well-typed/haskell-debugger/issues/149>
- Then continue with the MacOS / Linux / WSL2 instructions below.

### MacOS / Linux / WSL2
- Linux (either WSL2 or native): Install [GHCup](https://www.haskell.org/ghcup/)
  - Ubuntu (including WSL2):
    ```bash
    sudo apt update
    sudo apt upgrade -y
    sudo apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ```
  - MacOS:
    - First, you need to procure LLVM. Either use Homebrew command `brew install llvm`, or follow instructions here:\
      <https://releases.llvm.org/download.html>
      - E.g. for version 18.1.8 on M-series Macs, you can download `clang+llvm-18.1.8-arm64-apple-macos11.tar.xz`
    - Regardless of installation method, ensure that the `bin` directory of the LLVM installation is in your PATH environment variable. For Homebrew, you can run:
      ```bash
      echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc
      source ~/.zshrc
      ```
    - Then, install GHCup:
      ```bash
      curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
      ```
  - Press ENTER to accept the only available ghcup install location ($HOME/.ghcup)
  - Press ENTER to acknowledge that the System requirements had just been installed
  - Answer 'D' (or 'G' on MacOS) to use the Default (GHCup maintained) binary distribution "channel"
  - Answer 'N' to disable the pre-releases channel
  - Answer 'Y' to enable the cross channel (GHCJS, WASM, etc.)
  - Answer 'P' to automatically add (prepend) the required PATH variable to "$HOME/.bashrc" (or "$HOME/.zshrc" on MacOS)
  - Answer 'Y' to install HLS (Haskell Language Server), we need it for IDE support
  - Answer 'Y' to install stack (we might be able to build our project with Caba alone, but other projects might need stack)
  - MacOS only: look for security warnings installing with GHCup. They may appear as popups like "llc" Not Opened - Apple could not verify "llc" ...
    - Select "Done" to not move each file to the Trash
    - After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have to unblock the last executable in System Preferences:
      - Open System Preferences -> Privacy & Security -> scroll down to Security section
      - There should be a message like "llc" was blocked to protect your Mac.
      - Click Allow Anyway
    - Repeat for other files like "opt", etc. until no more popups appear
    - If any popups appeared, you may want to kill the ghcup installer (Ctrl+C), clean out "$HOME/.ghcup", and re-run it, so that it can proceed with installing with now unblocked executables.
    - On rerun you may get popups again for the same executables like Open "llc"? This time, click Open Anyway for each. It may also ask for your password to allow the installation to proceed.
  - For the new PATH variable to take effect, either restart the WSL terminal or run:
    ```bash
    . "$HOME/.ghcup/env"
    ```
  - Now run `ghcup tui` to manage installed versions of GHC, Cabal, Stack, HLS, etc.
    - Immediately change GHC to the latest version that is "hls-powered" (compatible with Haskell Language Server) by selecting it in the list and pressing 's'. On MacOS, I did not see any "hls-powered" versions, so just select a version that would be "hls-powered" on Windows / Linux (can test via a Linux VM or another machine), or use GHC 9.12.2 if it's still current enough.
      - Answer 'Y' for whether you would like to install the selected version if it is not already installed.
      - MacOS only: look for security warnings installing with GHCup. They may appear as popups like "clang-18" Not Opened - Apple could not verify "clang-18" ...
        - Select "Done" to not move each file to the Trash
        - After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have to unblock the last executable in System Preferences:
          - Open System Preferences -> Privacy & Security -> scroll down to Security section
          - There should be a message like "llc" was blocked to protect your Mac.
          - Click Allow Anyway
        - Repeat for other files until no more popups appear
      - Press enter after "Success"
        - MacOS only: if any popups appeared, you may want to use undo by setting the GHC version to the "recommended" one with 's', selecting the newer one again and uninstalling with 'u', then pressing 's' to install and set it again, so that it can proceed with installing with now unblocked executables.
        - On rerun you may get popups again for the same executables like Open "clang-18"? This time, click Open Anyway for each. It may also ask for your password to allow the installation to proceed.
    - You should be returned back into `ghcup tui`. For cabal, HLS, and stack, select the latest versions and press 's' to set them as default.
      - Similarly answer 'Y' for whether you would like to install the selected version if it is not already installed.
      - Also, press enter after each "Success" and finally answer 'q' to exit ghcup
    - Check the versions with `ghc --version`, `cabal --version`, `haskell-language-server-wrapper --version`, and `stack --version` for last installed, respectively.
    - In the past, the latest versions had not all been compatible with each other or an otherwise compatible version set segfaulted on Windows (and sometimes only when not compiled with the Msys2 toolchain). So we maintain a list of known compatible versions here:
      - GHC 9.12.2
      - Cabal 3.16.1.0
      - Stack 3.9.1
      - HLS 2.12.0.0
    - If the HLS version is tied to a mismatching GHC version like `2.12.0.0 (GHC: 9.10.3)`, you can have ghcup install a different HLS version that matches your GHC version.
      - First, make sure to close any IDE/editor that might be using HLS (otherwise, you'll get an error like `ghcup.exe: C:\ghcup\bin\haskell-language-server-9.12.2~2.12.0.0.exe: DeleteFile "\\\\?\\C:\\ghcup\\bin\\haskell-language-server-9.12.2~2.12.0.0.exe": permission denied (Access is denied.)`).
      - Then, run:
        ```bash
        ghcup compile hls -v 2.12.0.0 --ghc 9.12.2
        ```
    - If the highest GHC version that is "hls-powered" is not at least 9.14, also install the "latest" version of GHC with "i" (but don't set it as default with "s"), as the better haskell-debugger (hdb) requires at least GHC 9.14.
  - Stack seems to have its own GHC version configured outside of ghcup, so if you use stack, you might have to configure the GHC version in `stack.yaml` files.
    - To check what GHC version stack is using globally, run:
      ```bash
      stack ghc -- --version
      ```
    - I made a naive attempt to set the resolver to the same GHC version as above, but it produced an invalid stack.yaml file. My command:
      ```bash
      stack config set resolver ghc-9.12.2
      ```
      - If you get an error like `$HOME/.stack/global-project/stack.yaml: withFile: does not exist (No such file or directory)`, then first run:
        ```bash
        mkdir -f "$HOME/.stack/global-project"
        touch "$HOME/.stack/global-project/stack.yaml"
        ```
        Then run `stack config set resolver ghc-9.12.2` again.
    - Either way, the above stack command at least filled in the `stack.yaml` file. Find it at:\
      `$HOME/.stack/global-project/stack.yaml`
      Ensure it contains the matching GHC version as set via `ghcup tui` above, e.g. I had to change just one line from:
      ```yaml
      resolver: compiler: ghc-9.12.2
      ```
      To:
      ```yaml
      resolver: ghc-9.12.2
      ```
- Haskell debugger (hdb), supported by Visual Studio Code Haskell Debugger extension:
  - You must use the higher GHC version between the one set as default in ghcup and any 9.14+ version you installed additionally for hdb support
    - Only if the highest "hls-powered" GHC version was below 9.14+, fill in the "latest" version in the following command and run it (skip if the "hls-powered" GHC version is already 9.14+):
      ```bash
      ghcup set ghc <<ghc_version>>
      ```
      E.g. for GHC 9.14.1:
      ```bash
      ghcup set ghc 9.14.1
      ```
  - Now fill in the same, higher GHC version to install haskell-debugger:
    ```bash
    cabal install -w ghc-<<ghc_version>> haskell-debugger --allow-newer=base,time,containers,ghc,ghc-bignum,template-haskell --enable-executable-dynamic
    ```
    E.g. for GHC 9.14.1:
    ```bash
    cabal install -w ghc-9.14.1 haskell-debugger --allow-newer=base,time,containers,ghc,ghc-bignum,template-haskell --enable-executable-dynamic
    ```
  - ghcup puts a line in `~/.bashrc` to set up PATH for ghcup-installed tools, but VS Code server for WSL will not execute that file. Copy the line to `~/.bash_profile` so that VS Code can find ghc, hlint, haskell-debugger, and whatever else that's used by VS Code extensions:
    ```bash
    [ -f "/home/bruck/.ghcup/env" ] && . "/home/bruck/.ghcup/env" # ghcup-env
    ```
    - If you don't do this, starting a debugger with Haskell Debugger extension in VS Code will fail with an EACCES or ENOENT error.
  - If you had to `ghcup set ghc` to a different version than your default, you need to switch back to the "hls-powered" version with:
    ```bash
    ghcup set ghc <<hls_powered_ghc_version>>
    ```
    E.g. for GHC 9.12.2:
    ```bash
    ghcup set ghc 9.12.2
    ```
    - If this is the case, you need the higher version when you want to use the debugger, and the "hls-powered" version for normal development (so you get proper language server support). For the debugger to work, you may also need to update the `cabal.project` file in the repository root to specify the higher GHC version: `with-compiler: ghc-<<higher_version>>`, e.g.:
      ```
      with-compiler: ghc-9.14.1
      ```
      - And update any `*.cabal` files for any `base` version bounds that might prevent building with the higher GHC version, e.g. change from `build-depends:    base ^>= 4.21.0.0` to:
        ```
        build-depends:    base ^>= 4.22.0.0
        ```
      - Don't forget to undo changes to `cabal.project` and `*.cabal` files when you want to go back to normal development with HLS.
- IDE: Install [Visual Studio Code](https://code.visualstudio.com/) or another Haskell-compatible IDE/editor. If using WSL, you can install VS Code on the host operating system (Windows).
- For Haskell Language Server (HLS), to be able to process our files with custom preprocessors, as well as debug support, we need to install `spec-preprocessor`, `hspec-discover`, and `hlint` globally.
  - Back in Bash terminal (WSL), navigate to packages/test-lib directory (e.g. `cd "/mnt/c/Users/$(whoami)/source/repos/haskell-web-api/packages/test-lib"`) and run:
    ```bash
    cabal update
    cabal install hlint --overwrite-policy=always
    cabal build test-lib:spec-preprocessor
    cp "$(cabal list-bin test-lib:exe:spec-preprocessor)" "$HOME/.cabal/bin/"
    ```
  - And in case it matters the last known working version of hlint, you can find it from running the following command; example compatible version is v3.10:
    ```powershell
    cabal exec "$HOME/.cabal/bin/hlint" -- --version
    ```
  - If you can also install `hspec-discover`, do so with:
    ```bash
    cabal install hspec-discover --overwrite-policy=always
    ```
    - On MacOS, I have seen that fail with error: "filepath wildcard 'package.yaml' does not match any files.", so you will have to build it from source manually if needed.
    - Download the source from: <https://hackage.haskell.org/package/hspec-discover>\
      E.g. download `hspec-discover-2.11.16.tar.gz`, extract it, and in the extracted directory run:
      ```bash
      cabal v2-build
      cabal v2-install --installdir="$HOME/.cabal/bin" --overwrite-policy=always
      ```
  - Additionally, in VS Code, I was seeing error logs in output `Haskell (haskell-web-api)` like `semanticTokens is disabled globally in your config.`. See issue:\
    <https://github.com/haskell/haskell-language-server/issues/4081>
    - To fix this, open VS Code settings (Ctrl+,) (or command+, on a Mac), search for `semantic tokens`, and ensure that `Haskell › Plugin › Semantic Tokens: Global On` (description: `Enables semanticTokens plugin`) is checked.
    - You may have to restart the Haskell Language Server for the change to take effect. In VS Code command palette (Ctrl+Shift+P), run `Haskell: Restart Haskell LSP Server`.
  - **NOTE:** if you make any updates to `spec-preprocessor`, you need to re-run the `cabal build test-lib:spec-preprocessor` and `cp ...` commands if you want the updated version to be the one used when you GHCi debug.
- Windows host only: To hook up host VS Code to the now set up WSL environment, install the
  [Remote - WSL extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl)
  by Microsoft in the host VS Code.
  - After installing the extension, in the host VS Code command palette (Ctrl+Shift+P),
    run `WSL: Connect to WSL in New Window` to open a new VS Code window connected to WSL.
    - In the WSL VS Code window (identified by the left side of the bottom status bar saying "WSL: Ubuntu"), install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) by the Haskell Foundation and [Haskell Debugger](https://marketplace.visualstudio.com/items?itemName=Well-Typed.haskell-debugger-extension) by Well-Typed.
    - You might have to "Switch to Pre-Release Version" for compatibility with the latest GHC/HLS versions.
    - If it gives an option to "Restart Extensions", do so.

## Running tests and capturing coverage

1. Run all tests (Unit and Integration):
   ```bash
   cabal test all
   ```
2. Generate coverage report (must be based on Unit tests only):
   ```bash
   ./generate-code-coverage.ps1
   ```
   - Works on both Windows PowerShell and Linux bash (e.g. WSL2 / Ubuntu), it is both a PowerShell and bash script (polyglot).
   - For PowerShell only, opens `hpc_index.html` in default browser when done, but you can also open it manually.
   - It gives a non-zero exit code if coverage is below 100% (currently not configurable). Useful for CI pipelines.

## License

This project is licensed under the BSD+Patent License - see the [LICENSE](LICENSE) file for details.
