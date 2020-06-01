# CL-LSP for Visual Studio Code

This extension adds support to Visual Studio Code for using CL-LSP, a
language server for Common Lisp.

**Note**: CL-LSP is under heavy development and this should be considered
a preview. Users will need to provide the `cl-lsp` executable.

## Building and Installing the Extension

Currently, the way to get the extension is to build and install it from source.
You will also need the `cl-lsp` language server executable and a Common Lisp toolchain. For more information about cl-lsp, see [here](https://github.com/cxxxr/cl-lsp).

**Prerequisite**: To build the extension, you will need Node.js and npm: https://www.npmjs.com/get-npm.

The following commands build the extension and creates a `.vsix` package in the `out` directory.

```
$ cd vscode/
$ npm run createDevPackage
```

You can install the package from the command-line using the `code` command if available (see [Launching from the command line](https://code.visualstudio.com/docs/setup/mac#_launching-from-the-command-line)).

```
code --install-extension out/cl-lsp-vscode-dev.vsix
```

Or you can install from within the application using the `Extensions > Install from VSIX...` command from the command palette.

**Finally**, restart Visual Studio Code and don't forget to set the `Server Path` and add the argument `'stdio'` to the `Server Arguments`. Please see Configuration below

### Developing the Extension in Visual Studio Code

As an alternative, you can open the extension directory from Visual Studio Code and build it from within the application.

1. Run `npm install` inside the extension directory to install dependencies.
2. Open the extension directory in Visual Studio Code.
3. Hit `F5` to build the extension and launch an editor window that uses it.

This will start debugging a special instance of Visual Studio Code that will have "[Extension Development Host]" in the window title and use the new extension.

There is extensive documentation for developing extensions from within Visual Studio Code at https://code.visualstudio.com/docs/extensions/overview.

## Configuration

Settings for CL-LSP can be found in `Preferences > Settings` under
`Extensions > CL-LSP` or by searching for the setting prefix
`cl-lsp.`.

* Server Path: The path of the cl-lsp executable. Usually, after installing with [roswell](https://github.com/roswell/roswell/), the server path is `/Users/YOUR_USERNAME/.roswell/bin/cl-lsp` (Use absolute path).
* Toolchain Path: (optional) The path of the common lisp toolchain (sets `CL_LSP_TOOLCHAIN_PATH`). By default, cl-lsp uses the toolchain it is installed in.

The extension will find the `cl-lsp` executable automatically if it is in
`PATH`, or it can be provided manually using this setting.