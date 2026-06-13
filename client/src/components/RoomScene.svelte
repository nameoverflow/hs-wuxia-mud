<script lang="ts">
  import { afterUpdate, beforeUpdate } from "svelte";
  import { directionVector, exitLabel, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { Direction, RoomCharacterSummary, RoomExitSummary } from "../protocol";

  export let state: GameState;

  type MapPoint = {
    direction: Direction;
    label: string;
    key: string;
    x: number;
    y: number;
  };

  const currentPoint = { x: 50, y: 56 };

  let selectedCharacter: RoomCharacterSummary | null = null;
  let selectedDirection: Direction | null = null;
  let pendingMapMove: { direction: Direction; roomName: string | null; roomKey: string } | null = null;
  let mapMoving = false;
  let mapElement: HTMLDivElement | null = null;
  let previousRects = new Map<string, DOMRect>();
  let mapMoveTimeout: number | null = null;
  let mapAnimationCleanup: number | null = null;
  let currentRoomKey = roomNameKey("");
  let lastRoomName = "";

  $: if (selectedCharacter && !state.room.characters.some((character) => character.id === selectedCharacter?.id)) {
    selectedCharacter = null;
  }

  $: if (state.room.name !== lastRoomName) {
    currentRoomKey = resolveCurrentRoomKey(state.room.name);
    lastRoomName = state.room.name;
    clearMapMove();
  }

  $: mapPoints = buildMapPoints(state.room.exits);

  beforeUpdate(() => {
    previousRects = captureMapNodeRects();
  });

  afterUpdate(() => {
    animateMapTransition(previousRects);
    previousRects = new Map<string, DOMRect>();
  });

  function move(exit: RoomExitSummary) {
    if (!state.connected) return;
    markMapMove(exit);
    sendAction({ go: exit.direction });
  }

  function act(character: RoomCharacterSummary, action: "talk" | "attack") {
    if (!character.id) return;
    sendAction(action === "talk" ? { talk: character.id } : { attack: character.id });
    selectedCharacter = null;
  }

  function hasAction(character: RoomCharacterSummary, action: string) {
    return character.actions.includes(action) || character.actions.includes(`${action}ing`) || (action === "talk" && character.actions.includes("dialogue"));
  }

  function openNpcModal(character: RoomCharacterSummary) {
    selectedCharacter = character;
  }

  function closeNpcModal() {
    selectedCharacter = null;
  }

  function handleKeydown(event: KeyboardEvent) {
    if (event.key === "Escape" && selectedCharacter) {
      closeNpcModal();
    }
  }

  function handleModalBackdropClick(event: MouseEvent) {
    if (event.target === event.currentTarget) {
      closeNpcModal();
    }
  }

  function markMapMove(exit: RoomExitSummary) {
    const label = exitLabel(state.locale, exit);
    selectedDirection = exit.direction;
    pendingMapMove = {
      direction: exit.direction,
      roomName: exit.roomName,
      roomKey: exitRoomKey(exit, label)
    };
    mapMoving = true;

    if (mapMoveTimeout !== null) window.clearTimeout(mapMoveTimeout);
    mapMoveTimeout = window.setTimeout(clearMapMove, 900);
  }

  function clearMapMove() {
    selectedDirection = null;
    pendingMapMove = null;
    mapMoving = false;
    if (mapMoveTimeout !== null) {
      window.clearTimeout(mapMoveTimeout);
      mapMoveTimeout = null;
    }
  }

  function buildMapPoints(exits: RoomExitSummary[]) {
    const currentPosition = inferCurrentMapPosition(exits);
    return exits.map((exit) => {
      const label = exitLabel(state.locale, exit);
      const targetPosition = exitPosition(exit);
      const fallbackVector = worldDirectionVector(exit.direction);
      const dx = targetPosition ? targetPosition.x - currentPosition.x : fallbackVector.x;
      const dy = targetPosition ? targetPosition.y - currentPosition.y : fallbackVector.y;
      const vector = dx === 0 && dy === 0 ? fallbackVector : { x: dx, y: dy };

      return {
        direction: exit.direction,
        label,
        key: exitRoomKey(exit, label),
        x: clamp(currentPoint.x + vector.x * 28, 14, 86),
        y: clamp(currentPoint.y - vector.y * 30, 18, 88)
      };
    });
  }

  function inferCurrentMapPosition(exits: RoomExitSummary[]) {
    const candidates = exits
      .map((exit) => {
        const position = exitPosition(exit);
        if (!position) return null;
        const vector = worldDirectionVector(exit.direction);
        return {
          x: position.x - vector.x,
          y: position.y - vector.y
        };
      })
      .filter((candidate): candidate is { x: number; y: number } => Boolean(candidate));

    if (candidates.length === 0) return { x: 0, y: 0 };

    const counts = new Map<string, number>();
    for (const candidate of candidates) {
      const key = `${candidate.x},${candidate.y}`;
      counts.set(key, (counts.get(key) || 0) + 1);
    }

    const [bestKey] = [...counts.entries()].sort((a, b) => b[1] - a[1])[0];
    const [x, y] = bestKey.split(",").map(Number);
    return { x, y };
  }

  function exitPosition(exit: RoomExitSummary) {
    const raw = exit.position;
    if (Array.isArray(raw) && raw.length >= 2) {
      return { x: Number(raw[0]), y: Number(raw[1]) };
    }
    if (raw && typeof raw === "object") {
      const position = raw as { x?: unknown; y?: unknown };
      if (Number.isFinite(Number(position.x)) && Number.isFinite(Number(position.y))) {
        return { x: Number(position.x), y: Number(position.y) };
      }
    }
    return null;
  }

  function worldDirectionVector(direction: Direction) {
    const vector = directionVector(direction);
    return { x: vector.x, y: -vector.y };
  }

  function resolveCurrentRoomKey(roomName: string) {
    if (pendingMapMove && (!pendingMapMove.roomName || pendingMapMove.roomName === roomName)) {
      return pendingMapMove.roomKey;
    }
    return currentRoomKey && lastRoomName === roomName ? currentRoomKey : roomNameKey(roomName);
  }

  function exitRoomKey(exit: RoomExitSummary, label: string) {
    if (exit.roomId) return `id:${exit.roomId}`;
    if (exit.roomName) return roomNameKey(exit.roomName);
    return roomNameKey(label || exit.direction);
  }

  function roomNameKey(name: string | null) {
    return `name:${name || "current"}`;
  }

  function pointStyle(point: { x: number; y: number }) {
    return `left: ${point.x}%; top: ${point.y}%`;
  }

  function clamp(value: number, min: number, max: number) {
    return Math.max(min, Math.min(max, value));
  }

  function captureMapNodeRects() {
    const rects = new Map<string, DOMRect>();
    if (!mapElement) return rects;

    mapElement.querySelectorAll<HTMLElement>(".map-node[data-room-key]").forEach((node) => {
      const key = node.dataset.roomKey;
      if (key) rects.set(key, node.getBoundingClientRect());
    });
    return rects;
  }

  function animateMapTransition(rects: Map<string, DOMRect>) {
    if (!mapElement || rects.size === 0) return;

    const animatedNodes: HTMLElement[] = [];
    mapElement.querySelectorAll<HTMLElement>(".map-node[data-room-key]").forEach((node) => {
      const previous = rects.get(node.dataset.roomKey || "");
      if (!previous) return;

      const next = node.getBoundingClientRect();
      const dx = previous.left - next.left;
      const dy = previous.top - next.top;
      if (Math.abs(dx) < 1 && Math.abs(dy) < 1) return;

      node.classList.add("map-node-animating");
      node.style.setProperty("--move-x", `${dx}px`);
      node.style.setProperty("--move-y", `${dy}px`);
      animatedNodes.push(node);
    });

    if (animatedNodes.length === 0) return;
    void mapElement.offsetWidth;

    animatedNodes.forEach((node) => {
      node.classList.remove("map-node-animating");
      node.classList.add("map-node-settling");
      node.style.setProperty("--move-x", "0px");
      node.style.setProperty("--move-y", "0px");
    });

    if (mapAnimationCleanup !== null) window.clearTimeout(mapAnimationCleanup);
    mapAnimationCleanup = window.setTimeout(() => {
      animatedNodes.forEach((node) => {
        node.classList.remove("map-node-settling");
        node.style.removeProperty("--move-x");
        node.style.removeProperty("--move-y");
      });
      mapAnimationCleanup = null;
    }, 240);
  }
</script>

<svelte:window on:keydown={handleKeydown} />

<section class="world-panel">
  <div class="world-grid">
    <section class="map-panel">
      <div class="section-heading">
        <h2>{translate(state.locale, "panel.exits")}</h2>
        <span>{state.room.exits.length}</span>
      </div>
      <div class="room-context">
        <strong>{state.room.name || translate(state.locale, "panel.world")}</strong>
        <p>{state.room.desc || translate(state.locale, "message.initial")}</p>
      </div>
      <div bind:this={mapElement} class:map-moving={mapMoving} class="direction-map">
        <svg class="map-links-svg" viewBox="0 0 100 100" preserveAspectRatio="none" aria-hidden="true">
          {#each mapPoints as point (point.key)}
            <line x1={currentPoint.x} y1={currentPoint.y} x2={point.x} y2={point.y}></line>
          {/each}
        </svg>

        <div class="map-node map-node-current" data-room-key={currentRoomKey} style={pointStyle(currentPoint)}>
          {state.room.name || translate(state.locale, "panel.world")}
        </div>

        {#if mapPoints.length === 0}
          <div class="map-empty">{translate(state.locale, "ui.none")}</div>
        {/if}

        {#each mapPoints as point (point.key)}
          <button
            type="button"
            class="map-node map-node-exit"
            class:map-node-selected={selectedDirection === point.direction}
            data-direction={point.direction}
            data-room-key={point.key}
            style={pointStyle(point)}
            disabled={!state.connected}
            on:click={() => move(state.room.exits.find((exit) => exit.direction === point.direction) || state.room.exits[0])}
          >
            {point.label}
          </button>
        {/each}
      </div>
    </section>

    <section class="people-panel">
      <div class="section-heading">
        <h2>{translate(state.locale, "panel.people")}</h2>
        <span>{state.room.characters.length}</span>
      </div>
      {#if state.room.characters.length === 0}
        <p class="empty">{translate(state.locale, "ui.none")}</p>
      {:else}
        <div class="people-list">
          {#each state.room.characters as character}
            <button
              type="button"
              class:active={selectedCharacter?.id === character.id}
              aria-haspopup="dialog"
              aria-expanded={selectedCharacter?.id === character.id}
              disabled={!character.id}
              on:click={() => openNpcModal(character)}
            >
              <strong>{character.name}</strong>
              <small>{character.desc || character.id}</small>
            </button>
          {/each}
        </div>
      {/if}
    </section>
  </div>
</section>

{#if selectedCharacter}
  <div class="npc-modal" role="presentation" on:click={handleModalBackdropClick}>
    <div class="npc-modal-content" role="dialog" aria-modal="true" aria-labelledby="npc-modal-title" tabindex="-1">
      <button class="npc-modal-close" type="button" aria-label={translate(state.locale, "action.close")} on:click={closeNpcModal}>×</button>
      <h2 id="npc-modal-title">{selectedCharacter.name}</h2>
      {#if selectedCharacter.desc}
        <p class="npc-modal-desc">{selectedCharacter.desc}</p>
      {/if}
      <div class="npc-modal-actions">
        {#if hasAction(selectedCharacter, "talk")}
          <button class="npc-action" type="button" on:click={() => act(selectedCharacter!, "talk")}>{translate(state.locale, "action.talk")}</button>
        {/if}
        {#if hasAction(selectedCharacter, "attack")}
          <button class="npc-action npc-action-attack" type="button" on:click={() => act(selectedCharacter!, "attack")}>{translate(state.locale, "action.attack")}</button>
        {/if}
        {#if !hasAction(selectedCharacter, "talk") && !hasAction(selectedCharacter, "attack")}
          <span class="npc-no-actions">{translate(state.locale, "ui.none")}</span>
        {/if}
      </div>
    </div>
  </div>
{/if}
