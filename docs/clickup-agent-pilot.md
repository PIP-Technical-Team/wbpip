# ClickUp agent integration pilot

This opt-in pilot checks whether a coding agent can keep one ClickUp task and
its GitHub work connected. It introduces no R/package behavior changes.

- Repository: `PIP-Technical-Team/wbpip`.
- Target branch: `PROD`.
- Pilot issue: [#269](https://github.com/PIP-Technical-Team/wbpip/issues/269).
- Test task: [Verify scoped agent updates](https://app.clickup.com/t/868mjzvmu).

## Pilot checklist

1. Read the task and verify the intended Workspace, home List, parent, and
   repository before writing. Reuse the existing test child.
2. Put the GitHub issue or PR URL in every new subtask's initial description;
   create or reuse an issue before the subtask when no target exists.
3. Preserve issue/PR descriptions and add a reciprocal ClickUp link with the
   native task reference `CU-868mjzvmu`.
4. Include both systems' URLs and source comment IDs in discussion snapshots.
   Use stable operation/provenance markers and skip generated mirrors.
5. Repeat reads before retrying writes. A timeout is an uncertain outcome,
   not proof that a task or comment was never created.
6. Keep the pilot PR as a draft. Task completion requires accepted work,
   a linked PR merged into the intended base, and verified latest-head checks.

Session-based CLI/MCP reconciliation runs only when invoked. Native ClickUp
GitHub activity links and optional Automations require separate Workspace and
repository setup; authenticating an agent does not enable those connections.

Do not merge, deploy, or change R/package behavior as part of this integration
check. Remove or revise this pilot note when the team chooses its production
workflow.
