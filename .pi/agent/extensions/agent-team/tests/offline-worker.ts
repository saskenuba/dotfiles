import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import workerBridge from "../worker.js";
import offlineProvider from "./offline-provider.js";

/** Test-only explicit extension, never discovered by the normal worker launcher. */
export default function offlineWorker(pi: ExtensionAPI) {
  offlineProvider(pi);
  workerBridge(pi);
}
