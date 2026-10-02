export const TEAM_VERSION = 1;
export const TEAM_MESSAGE_TYPE = "agent-team-message";
export const TEAM_STATE_TYPE = "agent-team-state";
export const TEAM_UPDATE_EVENT = "agent-team:update";
export const WORKER_MESSAGE_TOOL = "orchestrator_message";
export const MAX_AGENTS = 4;
export const MAX_MESSAGE_CHARS = 16000;
export const MAX_HISTORY = 100;
export const MAX_PENDING_INSTRUCTIONS = 32;
export const MAX_PENDING_ACKS = 32;
export const MAX_DEDUPE_IDS = 512;

export type AgentState = "starting" | "running" | "blocked" | "idle" | "failed" | "closed" | "interrupted";
export type MessageKind = "progress" | "finding" | "question" | "blocker" | "ack" | "result" | "error" | "instruction";
export type DeliveryMode = "steer" | "followUp";
export interface TeamMessage {
  version: 1;
  id: string;
  agentId: string;
  assignmentId: string;
  direction: "to-worker" | "to-orchestrator";
  kind: MessageKind;
  text: string;
  createdAt: number;
  replyTo?: string;
  requiresAck?: boolean;
  delivery?: "pending" | "accepted" | "acknowledged" | "failed";
}
export interface WorkerMessageDetails {
  teamMessage: TeamMessage;
  wait: boolean;
}
export interface AgentRecord {
  id: string;
  name: string;
  persona: string;
  cwd: string;
  model?: string;
  state: AgentState;
  assignmentId: string;
  task: string;
  createdAt: number;
  updatedAt: number;
  sessionFile?: string;
  pendingQuestionId?: string;
  pendingAcknowledgments?: TeamMessage[];
  lastOutput?: string;
  error?: string;
  messages: TeamMessage[];
}
export interface TeamSnapshot {
  version: 1;
  agents: AgentRecord[];
}
export interface StartOptions {
  name: string;
  persona: string;
  systemPrompt: string;
  task: string;
  context?: string;
  cwd: string;
  model?: string;
  thinking?: string;
  tools?: string[];
  parentSession?: string;
}
export interface SendOptions {
  agentId: string;
  message: string;
  mode?: DeliveryMode;
  replyTo?: string;
  requiresAck?: boolean;
  newAssignment?: boolean;
}
export function isLive(state: AgentState): boolean {
  return state === "starting" || state === "running" || state === "blocked" || state === "idle";
}
export function isWorkerMessageDetails(value: unknown): value is WorkerMessageDetails {
  if (!value || typeof value !== "object") return false;
  const details = value as Partial<WorkerMessageDetails>;
  const m = details.teamMessage;
  return typeof details.wait === "boolean" && !!m && m.version === 1 &&
    typeof m.id === "string" && typeof m.agentId === "string" && typeof m.assignmentId === "string" &&
    m.direction === "to-orchestrator" && ["progress", "finding", "question", "blocker", "ack"].includes(m.kind) &&
    typeof m.text === "string" && m.text.length <= MAX_MESSAGE_CHARS && typeof m.createdAt === "number";
}
