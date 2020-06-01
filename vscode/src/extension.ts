/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import {
	workspace, ExtensionContext, window, commands
} from 'vscode';
import {
	LanguageClient, LanguageClientOptions, ServerOptions, TextDocumentIdentifier, TextDocumentPositionParams, Executable
} from 'vscode-languageclient';

let languageClient: LanguageClient;

function startRepl() {
}

function getTextDocumentIdentifier() {
	let document = window.activeTextEditor!.document;
	let params: TextDocumentIdentifier = {
		uri: document.uri.toString()
	}
	return params;
}

function compileAndLoadFile() {
	let params = getTextDocumentIdentifier();
	languageClient.sendNotification('lisp/compileAndLoadFile', params);
}

function evaluate() {
	let selection = window.activeTextEditor!.selection;
	if (selection.isEmpty) {
		let params: TextDocumentPositionParams = {
			textDocument: getTextDocumentIdentifier(),
			position: selection.active
		}
		languageClient.sendNotification('lisp/eval', params);
	} else {
		let params = {
			textDocument: getTextDocumentIdentifier(),
			range: selection
		}
		languageClient.sendNotification('lisp/rangeEval', params);
	}
}

function interrupt() {
	languageClient.sendNotification('lisp/interrupt', {});
}

function indentLine() {
}

export function activate(context: ExtensionContext) {

	const config = workspace.getConfiguration('cl-lsp');

	const cl: Executable = {
        command: config.get<string>('serverPath', 'cl-lsp'),
        args: config.get<string[]>('serverArguments', [])
	};
	
	const toolchain = config.get<string>('toolchainPath', '');
    if (toolchain) {
        cl.options = { env: { ...process.env, CL_LSP_TOOLCHAIN_PATH: toolchain } };
	}
	
	const serverOptions: ServerOptions = cl;

	let clientOptions: LanguageClientOptions = {
		documentSelector: [
			'lisp'
		],
		synchronize: undefined 
	}

	languageClient = new LanguageClient('cl-lsp', 'Common Lisp Language Server', serverOptions, clientOptions);
	languageClient.onReady().then(function (x) {
		languageClient.onNotification('lisp/evalBegin', function (f) {
			window.setStatusBarMessage('Eval...');
		})
		languageClient.onNotification('lisp/evalEnd', function (f) {
			window.setStatusBarMessage('Done');
		})
	})

	context.subscriptions.push(languageClient.start());

	context.subscriptions.push(commands.registerCommand('lisp.indentLine', () => indentLine()));
	context.subscriptions.push(commands.registerCommand('lisp.compileAndLoadFile', () => compileAndLoadFile()));
	context.subscriptions.push(commands.registerCommand('lisp.eval', () => evaluate()));
	context.subscriptions.push(commands.registerCommand('lisp.interrupt', () => interrupt()));
	context.subscriptions.push(commands.registerCommand('lisp.replStart', () => startRepl()));

	console.log('CL-LSP is now active!');
}
