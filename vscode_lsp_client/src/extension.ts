import * as cp from "child_process";
import path = require("path");
import { commands, ExtensionContext, StatusBarAlignment, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  State,
  StreamInfo,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  console.log("Crontab Language Server extension activated");

  const statusBar = window.createStatusBarItem(StatusBarAlignment.Left, 0);
  statusBar.text = "$(loading~spin) crontab-ls: starting...";
  statusBar.show();
  context.subscriptions.push(statusBar);

  const relativeServerExe = "../../_build/default/lsp_server/main.exe";
  const serverExe = path.resolve(__dirname, relativeServerExe);
  console.log(serverExe);

  const serverOptions = () => {
    const child = cp.spawn(serverExe, [], {
      stdio: ["pipe", "pipe", "pipe"], // stdin, stdout, stderr
    });

    child.stderr.on("data", (data) => {
      console.error(`[LSP stderr] ${data}`);
    });

    const streamInfo: StreamInfo = {
      reader: child.stdout!,
      writer: child.stdin!,
    };

    return Promise.resolve(streamInfo);
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "crontab" }],
    outputChannelName: "Crontab Language Server",
    progressOnInitialization: true,
  };

  client = new LanguageClient(
    "crontab-ls-client",
    "Crontab Language Server",
    serverOptions,
    clientOptions
  );

  client.onDidChangeState((e) => {
    if (e.newState === State.Running) {
      statusBar.text = "$(check) crontab-ls: ready";
      statusBar.tooltip = "Crontab Language Server is running";
    } else if (e.newState === State.Stopped) {
      statusBar.text = "$(issue-opened) crontab-ls: stopped";
      statusBar.tooltip = "Crontab Language Server stopped unexpectedly";
    }
  });

  client
    .start()
    .then(() => {
      console.log("Crontab language client started successfully");
    })
    .catch((err) => {
      console.error("Failed to start language server:", err);
      statusBar.text = "$(error) crontab-ls: failed";
      statusBar.tooltip = `Failed to start Crontab Language Server: ${
        err.message || err
      }`;
    });

  const restartCommand = commands.registerCommand(
    "extension.restartLanguageServer",
    async () => {
      if (client) {
        await client.stop();
        client.start();
        window.showInformationMessage("Crontab Language Server restarted.");
      }
    }
  );

  context.subscriptions.push(restartCommand);
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
  }
}
