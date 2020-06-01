# CL-LSP
CL-LSP is an implementation of the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP) for Common Lisp.

## Getting Started

The CL-LSP server is included with the Swift toolchain. Depending on how you installed Swift, you may already have SourceKit-LSP. Make sure you build your package with the same toolchain as you use sourcekit-lsp from to ensure compatibility.

1. Get CL-LSP Installed

    1. Install [roswell](https://github.com/roswell/roswell/)

    2. Then install `CL-LSP`: `$ ros install cxxxr/cl-lsp`

    3. Verfiy installation: `~/.roswell/bin/cl-lsp`.

2. Configure your editor to use CL-LSP. See [Editors](Editors) for more information about editor integration.

3. Enjoy!

# Visual Studio Code

Depends `CL-LSP` on [vscode-lisp](https://github.com/mattn/vscode-lisp). See [vscode](vscode) for more information about Visual Studio Code integration.

```
$ cd vscode
$ npm run createDevPackage
$ code --install-extension out/cl-lsp-vscode-dev.vsix
```
