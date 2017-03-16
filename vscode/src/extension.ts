/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as path from 'path';
import * as child_process from 'child_process';
import { workspace, Disposable, ExtensionContext, languages, window, commands } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, TextDocumentIdentifier } from 'vscode-languageclient';
import * as vscode from 'vscode';

let languageClient: LanguageClient;

function startRepl() {
	//let outputChannel = window.createOutputChannel("repl");
	//outputChannel.show(true);
}

function compileAndLoadFile() {
	let document = window.activeTextEditor.document;
	let params : TextDocumentIdentifier = {
		uri: document.uri.toString()
	}
	languageClient.sendNotification("lisp/compileAndLoadFile", params);
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
	context.subscriptions.push(commands.registerCommand("lisp.compileAndLoadFile", () => compileAndLoadFile()));
	context.subscriptions.push(commands.registerCommand("lisp.replStart", () => startRepl()));
	context.subscriptions.push(languageClient.start());
}
