<script lang="ts">
  import { afterUpdate, beforeUpdate, onDestroy } from "svelte";
  import { directionVector, exitLabel, sendAction, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { Direction, RoomCharacterSummary, RoomExitSummary, RoomPosition } from "../protocol";

  export let state: GameState;

  type MapPoint = {
    direction: Direction;
    label: string;
    key: string;
    x: number;
    y: number;
  };

  type PositionPoint = {
    x: number;
    y: number;
  };

  type FullMapRoomPoint = {
    key: string;
    roomId: string | null;
    name: string;
    position: PositionPoint;
    x: number;
    y: number;
  };

  type FullMapEdgePoint = {
    key: string;
    from: FullMapRoomPoint;
    to: FullMapRoomPoint;
    current: boolean;
    path: string;
  };

  const currentPoint = { x: 50, y: 56 };

  let selectedCharacter: RoomCharacterSummary | null = null;
  let selectedDirection: Direction | null = null;
  let pendingMapMove: {
    direction: Direction;
    roomName: string | null;
    roomKey: string;
    startRoomName: string;
    roomArrived: boolean;
    slideDone: boolean;
  } | null = null;
  let showFullMap = false;
  let mapMoving = false;
  let mapElement: HTMLDivElement | null = null;
  let previousRects = new Map<string, DOMRect>();
  let mapMoveTimeout: number | null = null;
  let mapSlideTimeout: number | null = null;
  let mapAnimationCleanup: number | null = null;
  let transitionMapPoints: MapPoint[] | null = null;
  let transitionCurrentRoomKey: string | null = null;
  let transitionCurrentRoomName: string | null = null;
  let mapTravelOffset: PositionPoint | null = null;
  let skipNextMapNodeFlip = false;
  let currentRoomKey = roomNameKey("");
  let lastRoomName = "";
  const mapSlideMs = 150;
  const mapMoveFallbackMs = 600;

  $: if (selectedCharacter && !state.room.characters.some((character) => character.id === selectedCharacter?.id)) {
    selectedCharacter = null;
  }

  $: if (state.room.name !== lastRoomName) {
    currentRoomKey = resolveCurrentRoomKey(state.room.name);
    lastRoomName = state.room.name;
    markMapRoomArrived();
  }

  $: mapPoints = buildMapPoints(state.room.exits);
  $: visibleMapPoints = transitionMapPoints || mapPoints;
  $: visibleCurrentRoomKey = transitionCurrentRoomKey || currentRoomKey;
  $: visibleCurrentRoomName = transitionCurrentRoomName ?? state.room.name;
  $: mapTransitioning = transitionMapPoints !== null;
  $: mapLayerStyle = mapTravelOffset ? `--map-slide-x: ${mapTravelOffset.x}%; --map-slide-y: ${mapTravelOffset.y}%` : "";
  $: fullMapRooms = buildFullMapRooms(state.mapOverview);
  $: fullMapEdges = buildFullMapEdges(state.mapOverview, fullMapRooms);
  $: fullMapCurrentKey = currentFullMapRoomKey(state.mapOverview);

  beforeUpdate(() => {
    previousRects = captureMapNodeRects();
  });

  afterUpdate(() => {
    if (skipNextMapNodeFlip) {
      skipNextMapNodeFlip = false;
      previousRects = new Map<string, DOMRect>();
      return;
    }
    animateMapTransition(previousRects);
    previousRects = new Map<string, DOMRect>();
  });

  onDestroy(() => {
    clearMapMoveTimers();
    if (mapAnimationCleanup !== null) window.clearTimeout(mapAnimationCleanup);
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

  function openFullMap() {
    showFullMap = true;
    sendAction({ other: "map" });
  }

  function closeFullMap() {
    showFullMap = false;
  }

  function handleKeydown(event: KeyboardEvent) {
    if (event.key !== "Escape") return;
    if (showFullMap) {
      closeFullMap();
      return;
    }
    if (selectedCharacter) {
      closeNpcModal();
    }
  }

  function handleModalBackdropClick(event: MouseEvent) {
    if (event.target === event.currentTarget) {
      closeNpcModal();
    }
  }

  function handleFullMapBackdropClick(event: MouseEvent) {
    if (event.target === event.currentTarget) {
      closeFullMap();
    }
  }

  function markMapMove(exit: RoomExitSummary) {
    const label = exitLabel(state.locale, exit);
    const roomKey = exitRoomKey(exit, label);
    const targetPoint = mapPoints.find((point) => point.key === roomKey);
    selectedDirection = exit.direction;
    pendingMapMove = {
      direction: exit.direction,
      roomName: exit.roomName,
      roomKey,
      startRoomName: state.room.name,
      roomArrived: false,
      slideDone: false
    };
    transitionMapPoints = mapPoints;
    transitionCurrentRoomKey = currentRoomKey;
    transitionCurrentRoomName = state.room.name;
    mapTravelOffset = targetPoint ? { x: currentPoint.x - targetPoint.x, y: currentPoint.y - targetPoint.y } : { x: 0, y: 0 };
    skipNextMapNodeFlip = true;
    mapMoving = true;

    clearMapMoveTimers();
    mapSlideTimeout = window.setTimeout(() => {
      if (!pendingMapMove) return;
      pendingMapMove = { ...pendingMapMove, slideDone: true };
      finishMapMoveIfReady();
    }, mapSlideMs);
    mapMoveTimeout = window.setTimeout(clearMapMove, mapMoveFallbackMs);
  }

  function markMapRoomArrived() {
    if (!pendingMapMove) return;
    pendingMapMove = { ...pendingMapMove, roomArrived: state.room.name !== pendingMapMove.startRoomName };
    finishMapMoveIfReady();
  }

  function finishMapMoveIfReady() {
    if (!pendingMapMove?.roomArrived || !pendingMapMove.slideDone) return;
    clearMapMove();
  }

  function clearMapMove() {
    selectedDirection = null;
    pendingMapMove = null;
    mapMoving = false;
    transitionMapPoints = null;
    transitionCurrentRoomKey = null;
    transitionCurrentRoomName = null;
    mapTravelOffset = null;
    skipNextMapNodeFlip = true;
    clearMapMoveTimers();
  }

  function clearMapMoveTimers() {
    if (mapMoveTimeout !== null) {
      window.clearTimeout(mapMoveTimeout);
      mapMoveTimeout = null;
    }
    if (mapSlideTimeout !== null) {
      window.clearTimeout(mapSlideTimeout);
      mapSlideTimeout = null;
    }
  }

  function buildMapPoints(exits: RoomExitSummary[]) {
    const currentPosition = inferCurrentMapPosition(exits);
    return exits.map((exit) => {
      const label = exitLabel(state.locale, exit);
      const targetPosition = exitPosition(exit);
      const fallbackVector = worldDirectionVector(exit.direction);
      const dx = targetPosition && currentPosition ? targetPosition.x - currentPosition.x : fallbackVector.x;
      const dy = targetPosition && currentPosition ? targetPosition.y - currentPosition.y : fallbackVector.y;
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

    if (candidates.length === 0) return null;

    const counts = new Map<string, number>();
    for (const candidate of candidates) {
      const key = `${candidate.x},${candidate.y}`;
      counts.set(key, (counts.get(key) || 0) + 1);
    }

    const sorted = [...counts.entries()].sort((a, b) => b[1] - a[1]);
    const [bestKey, bestCount] = sorted[0];
    const [, secondCount = 0] = sorted[1] || [];
    if (bestCount === secondCount) return null;

    const [x, y] = bestKey.split(",").map(Number);
    return { x, y };
  }

  function exitPosition(exit: RoomExitSummary) {
    return positionPoint(exit.position);
  }

  function positionPoint(raw: RoomPosition | null | undefined): PositionPoint | null {
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

  function buildFullMapRooms(mapOverview: GameState["mapOverview"]): FullMapRoomPoint[] {
    const rooms = (mapOverview?.rooms || [])
      .map((room) => {
        const position = positionPoint(room.position);
        if (!position) return null;
        return {
          key: positionKey(position),
          roomId: room.roomId,
          name: room.roomName || room.roomId || translate(state.locale, "panel.world"),
          position,
          x: 50,
          y: 50
        };
      })
      .filter((room): room is FullMapRoomPoint => Boolean(room));

    if (rooms.length === 0) return [];

    const xs = rooms.map((room) => room.position.x);
    const ys = rooms.map((room) => room.position.y);
    const minX = Math.min(...xs);
    const maxX = Math.max(...xs);
    const minY = Math.min(...ys);
    const maxY = Math.max(...ys);
    const centerX = (minX + maxX) / 2;
    const centerY = (minY + maxY) / 2;
    const scale = 82 / Math.max(maxX - minX, maxY - minY, 1);

    return rooms.map((room) => ({
      ...room,
      x: clamp(50 + (room.position.x - centerX) * scale, 9, 91),
      y: clamp(50 - (room.position.y - centerY) * scale, 9, 91)
    }));
  }

  function buildFullMapEdges(mapOverview: GameState["mapOverview"], rooms: FullMapRoomPoint[]): FullMapEdgePoint[] {
    const roomsByPosition = new Map(rooms.map((room) => [positionKey(room.position), room]));
    const currentKey = currentFullMapRoomKey(mapOverview);
    const seen = new Set<string>();
    const edges: FullMapEdgePoint[] = [];

    for (const [index, edge] of (mapOverview?.edges || []).entries()) {
      const fromPosition = positionPoint(edge.from);
      const toPosition = positionPoint(edge.to);
      if (!fromPosition || !toPosition) continue;

      const from = roomsByPosition.get(positionKey(fromPosition));
      const to = roomsByPosition.get(positionKey(toPosition));
      if (!from || !to) continue;

      const sortedKey = [from.key, to.key].sort().join("|");
      if (seen.has(sortedKey)) continue;
      seen.add(sortedKey);

      edges.push({
        key: `${sortedKey}:${index}`,
        from,
        to,
        current: from.key === currentKey || to.key === currentKey,
        path: fullMapEdgePath(from, to)
      });
    }

    return edges;
  }

  function fullMapEdgePath(from: FullMapRoomPoint, to: FullMapRoomPoint) {
    const dx = to.x - from.x;
    const dy = to.y - from.y;
    const length = Math.hypot(dx, dy);
    if (length === 0) return `M ${from.x} ${from.y}`;

    const halfWidth = 4.2;
    const halfHeight = 3.4;
    const startCut = edgeCutRatio(dx, dy, halfWidth, halfHeight);
    const endCut = edgeCutRatio(-dx, -dy, halfWidth, halfHeight);
    const start = {
      x: from.x + dx * startCut,
      y: from.y + dy * startCut
    };
    const end = {
      x: to.x - dx * endCut,
      y: to.y - dy * endCut
    };

    return `M ${start.x} ${start.y} L ${end.x} ${end.y}`;
  }

  function edgeCutRatio(dx: number, dy: number, halfWidth: number, halfHeight: number) {
    const ratios = [];
    if (dx !== 0) ratios.push(halfWidth / Math.abs(dx));
    if (dy !== 0) ratios.push(halfHeight / Math.abs(dy));
    return Math.min(...ratios, 0.48);
  }

  function currentFullMapRoomKey(mapOverview: GameState["mapOverview"]) {
    const position = positionPoint(mapOverview?.currentPosition);
    return position ? positionKey(position) : "";
  }

  function positionKey(position: PositionPoint) {
    return `${position.x},${position.y}`;
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
    }, 200);
  }
</script>

<svelte:window on:keydown={handleKeydown} />

<section class="world-panel">
  <div class="world-grid">
    <section class="map-panel">
      <div class="section-heading map-heading">
        <h2>{translate(state.locale, "panel.exits")}</h2>
        <div class="map-heading-actions">
          <button class="ghost-button" type="button" disabled={!state.connected} on:click={openFullMap}>{translate(state.locale, "action.full_map")}</button>
          <span>{state.room.exits.length}</span>
        </div>
      </div>
      <div class="room-context">
        <strong>{state.room.name || translate(state.locale, "panel.world")}</strong>
        <p>{state.room.desc || translate(state.locale, "message.initial")}</p>
      </div>
      <div bind:this={mapElement} class:map-moving={mapMoving} class:map-transitioning={mapTransitioning} class="direction-map">
        <div class="map-travel-layer" style={mapLayerStyle}>
          <svg class="map-links-svg" viewBox="0 0 100 100" preserveAspectRatio="none" aria-hidden="true">
            {#each visibleMapPoints as point (point.key)}
              <line x1={currentPoint.x} y1={currentPoint.y} x2={point.x} y2={point.y}></line>
            {/each}
          </svg>

          <div
            class="map-node map-node-current"
            class:map-node-departing={mapTransitioning}
            data-room-key={visibleCurrentRoomKey}
            style={pointStyle(currentPoint)}
          >
            {visibleCurrentRoomName || translate(state.locale, "panel.world")}
          </div>

          {#each visibleMapPoints as point (point.key)}
            <button
              type="button"
              class="map-node map-node-exit"
              class:map-node-selected={selectedDirection === point.direction}
              class:map-node-travel-target={mapTransitioning && pendingMapMove?.roomKey === point.key}
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

        {#if visibleMapPoints.length === 0}
          <div class="map-empty">{translate(state.locale, "ui.none")}</div>
        {/if}
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

{#if showFullMap}
  <div class="map-modal" role="presentation" on:click={handleFullMapBackdropClick}>
    <div class="map-modal-content" role="dialog" aria-modal="true" aria-labelledby="full-map-title" tabindex="-1">
      <button class="npc-modal-close" type="button" aria-label={translate(state.locale, "action.close")} on:click={closeFullMap}>×</button>
      <div class="section-heading full-map-heading">
        <h2 id="full-map-title">{state.mapOverview?.mapName || translate(state.locale, "panel.full_map")}</h2>
        <span>{fullMapRooms.length}</span>
      </div>
      <div class="full-map-canvas" aria-label={translate(state.locale, "panel.full_map")}>
        {#if fullMapRooms.length > 0}
          <svg class="full-map-links" viewBox="0 0 100 100" preserveAspectRatio="none" aria-hidden="true">
            {#each fullMapEdges as edge (edge.key)}
              <path
                class="full-map-link-halo"
                class:full-map-link-current={edge.current}
                d={edge.path}
              ></path>
              <path
                class="full-map-link"
                class:full-map-link-current={edge.current}
                d={edge.path}
              ></path>
            {/each}
          </svg>

          {#each fullMapRooms as room (room.key)}
            <div
              class="full-map-room"
              class:full-map-room-current={room.key === fullMapCurrentKey}
              aria-current={room.key === fullMapCurrentKey ? "location" : undefined}
              style={pointStyle(room)}
            >
              <strong>{room.name}</strong>
              <small>{room.position.x},{room.position.y}</small>
            </div>
          {/each}
        {:else}
          <div class="full-map-empty">{state.mapOverview ? translate(state.locale, "ui.none") : translate(state.locale, "ui.loading")}</div>
        {/if}
      </div>
    </div>
  </div>
{/if}
