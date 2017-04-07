/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as path from 'path';
import * as child_process from 'child_process';
import {
	workspace, Disposable, ExtensionContext, languages,
	window, commands, InputBoxOptions
} from 'vscode';
import {
	LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions,
	TransportKind, TextDocumentIdentifier, TextDocumentPositionParams
} from 'vscode-languageclient';

let languageClient: LanguageClient;

function startRepl() {
	//let outputChannel = window.createOutputChannel("repl");
	//outputChannel.show(true);
	// let option: InputBoxOptions = {
	// 	ignoreFocusOut: false,
	// 	password: false,
	// 	prompt: "Eval",
	// }
	// window.showInputBox(option);

	console.log(window.activeTextEditor.selection.active);
}

function getTextDocumentIdentifier() {
	let document = window.activeTextEditor.document;
	let params: TextDocumentIdentifier = {
		uri: document.uri.toString()
	}
	return params;
}

function compileAndLoadFile() {
	let params = getTextDocumentIdentifier();
	languageClient.sendNotification("lisp/compileAndLoadFile", params);
}

function getTextDocumentPositionParams() : TextDocumentPositionParams {
	let params: TextDocumentPositionParams = {
		textDocument: getTextDocumentIdentifier(),
		position: window.activeTextEditor.selection.active
	}
	return params;
}

function evalLastSexp() {
	let params = getTextDocumentPositionParams();
	languageClient.sendNotification("lisp/evalLastSexp", params);
}

function interrupt() {
	languageClient.sendNotification("lisp/interrupt", {});
}

function indentLine() {
	let params = getTextDocumentPositionParams();
	languageClient.sendRequest("lisp/indentLine", params);
}

export function activate(context: ExtensionContext) {
	let serverOptions: ServerOptions;

	serverOptions = () => new Promise<child_process.ChildProcess>((resolve, reject) => {
		function spawnServer(...args: string[]): child_process.ChildProcess {
			let childProcess = child_process.spawn("cl-lsp.ros", ["stdio"]);
			return childProcess;
		}
		resolve(spawnServer());
	});

	let clientOptions: LanguageClientOptions = {
		documentSelector: ["lisp"],
		synchronize: {
			configurationSection: 'languageServerExample'
		}
	}

	languageClient = new LanguageClient("Common Lisp Language Server", serverOptions, clientOptions);
	context.subscriptions.push(commands.registerCommand("lisp.indentLine", () => indentLine()));
	context.subscriptions.push(commands.registerCommand("lisp.compileAndLoadFile", () => compileAndLoadFile()));
	context.subscriptions.push(commands.registerCommand("lisp.evalLastSexp", () => evalLastSexp()));
	context.subscriptions.push(commands.registerCommand("lisp.interrupt", () => interrupt()));
	context.subscriptions.push(commands.registerCommand("lisp.replStart", () => startRepl()));
	context.subscriptions.push(languageClient.start());
}
