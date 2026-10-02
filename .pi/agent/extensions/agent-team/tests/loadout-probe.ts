import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Test-only introspection command, no model calls. */
export default function loadoutProbe(pi: ExtensionAPI) {
  pi.registerCommand("team-test-loadout", {
    description: "Record loaded tool names for an offline compatibility test.",
    handler: async () => { pi.appendEntry("team-test-loadout", pi.getAllTools().map((tool) => tool.name)); },
  });
}
