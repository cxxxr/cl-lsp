/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as path from 'path';
import * as child_process from 'child_process';
import { workspace, Disposable, ExtensionContext, languages, window } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind } from 'vscode-languageclient';

export function activate(context: ExtensionContext) {
	let serverOptions: ServerOptions;

	serverOptions = () => new Promise<child_process.ChildProcess>((resolve, reject) => {
		function spawnServer(...args: string[]): child_process.ChildProcess {
			let childProcess = child_process.spawn("server.ros", ["stdio"]);
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

	let lc = new LanguageClient("Common Lisp Language Server", serverOptions, clientOptions);
	let disposable = lc.start();
	context.subscriptions.push(disposable);
}
