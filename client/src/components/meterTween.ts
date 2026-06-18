import { apSecondsUntil, normalizeAgility } from "../battle/apTiming";

type GsapApi = typeof import("gsap").gsap;

type MeterTweenParams = {
  value: number;
  duration?: number;
  ease?: string;
  snapDecrease?: boolean;
};

type ApMeterTweenParams = {
  value: number;
  agility: number;
  snapKey?: number | null;
  max?: number;
  resumeAt?: number;
};

let gsapPromise: Promise<GsapApi> | null = null;
const predictedApLimit = 0.99;

export function meterTween(node: HTMLElement, params: MeterTweenParams) {
  let gsapApi: GsapApi | null = null;
  let tween: { kill: () => void } | null = null;
  let target = normalize(params.value);
  let duration = params.duration ?? 0.24;
  let ease = params.ease ?? "power2.out";
  let snapDecrease = params.snapDecrease ?? false;
  let disposed = false;

  node.style.transformOrigin = "left center";
  node.style.transform = `scaleX(${target})`;

  loadGsap().then((loaded) => {
    if (disposed) return;
    gsapApi = loaded;
    gsapApi.set(node, { scaleX: target, transformOrigin: "left center" });
  });

  return {
    update(nextParams: MeterTweenParams) {
      const nextTarget = normalize(nextParams.value);
      const nextDuration = nextParams.duration ?? 0.24;
      const nextEase = nextParams.ease ?? "power2.out";
      const nextSnapDecrease = nextParams.snapDecrease ?? false;
      if (
        Math.abs(nextTarget - target) < 0.001 &&
        nextDuration === duration &&
        nextEase === ease &&
        nextSnapDecrease === snapDecrease
      ) {
        return;
      }

      const shouldSnap = nextSnapDecrease && nextTarget < target;
      target = nextTarget;
      duration = nextDuration;
      ease = nextEase;
      snapDecrease = nextSnapDecrease;

      if (!gsapApi || prefersReducedMotion()) {
        node.style.transform = `scaleX(${target})`;
        return;
      }
      if (shouldSnap) {
        tween?.kill();
        gsapApi.set(node, { scaleX: target, transformOrigin: "left center" });
        return;
      }
      tween?.kill();
      tween = gsapApi.to(node, {
        scaleX: target,
        duration,
        ease,
        overwrite: true
      });
    },
    destroy() {
      disposed = true;
      tween?.kill();
    }
  };
}

export function apMeterTween(node: HTMLElement, params: ApMeterTweenParams) {
  const barNode = resolveBarNode(node);
  const labelNode = resolveLabelNode(node);
  let gsapApi: GsapApi | null = null;
  let tween: { kill: () => void } | null = null;
  let max = params.max ?? 100;
  let base = normalize(params.value / max);
  let agility = normalizeAgility(params.agility);
  let snapKey = params.snapKey ?? null;
  let resumeAt = params.resumeAt ?? 0;
  let pendingDecrease: number | null = null;
  let resumeTimer: number | null = null;
  let disposed = false;
  const renderState = { scale: base };

  barNode.style.transformOrigin = "left center";
  renderAp();

  loadGsap().then((loaded) => {
    if (disposed) return;
    gsapApi = loaded;
    startApGrowth(true);
  });

  return {
    update(nextParams: ApMeterTweenParams) {
      const nextMax = nextParams.max ?? 100;
      const nextBase = normalize(nextParams.value / nextMax);
      const nextAgility = normalizeAgility(nextParams.agility);
      const nextSnapKey = nextParams.snapKey ?? null;
      const nextResumeAt = nextParams.resumeAt ?? 0;
      const snapActive = nextSnapKey !== null;
      const agilityChanged = nextAgility !== agility;
      const resumeChanged = Math.abs(nextResumeAt - resumeAt) > 1;
      const snapKeyChanged = nextSnapKey !== snapKey;
      const baseChanged = Math.abs(nextBase - base) >= 0.001 || nextMax !== max;
      max = nextMax;
      agility = nextAgility;
      resumeAt = nextResumeAt;

      if (pendingDecrease !== null) {
        if (snapActive) {
          snapKey = nextSnapKey;
          applyApBase(pendingDecrease, true, false);
          return;
        }
        if (nextBase < pendingDecrease) pendingDecrease = nextBase;
        snapKey = nextSnapKey;
        return;
      }

      if (nextBase < base - 0.001) {
        if (snapActive) {
          snapKey = nextSnapKey;
          applyApBase(nextBase, true, false);
          return;
        }
        pendingDecrease = nextBase;
        snapKey = nextSnapKey;
        return;
      }

      if (snapActive && snapKeyChanged && nextBase <= base + 0.001 && currentScale() > nextBase + 0.001) {
        snapKey = nextSnapKey;
        applyApBase(nextBase, true, false);
        return;
      }

      if (!baseChanged && !agilityChanged && !resumeChanged) {
        snapKey = nextSnapKey;
        return;
      }

      snapKey = nextSnapKey;
      applyApBase(nextBase, false, !baseChanged);
    },
    destroy() {
      disposed = true;
      clearResumeTimer();
      tween?.kill();
    }
  };

  function applyApBase(nextBase: number, snap: boolean, preserveCurrent: boolean) {
    base = nextBase;
    pendingDecrease = null;
    if (!gsapApi || prefersReducedMotion()) {
      if (!preserveCurrent) renderState.scale = base;
      renderAp();
      return;
    }
    if (snap || !preserveCurrent) {
      renderState.scale = base;
      renderAp();
    }
    startApGrowth(preserveCurrent && !snap);
  }

  function startApGrowth(fromCurrent: boolean) {
    if (!gsapApi || prefersReducedMotion()) return;
    clearResumeTimer();
    tween?.kill();
    const pauseMs = resumeAt - performance.now();
    if (pauseMs > 0) {
      renderAp();
      resumeTimer = window.setTimeout(() => {
        resumeTimer = null;
        if (!disposed) startApGrowth(false);
      }, pauseMs);
      return;
    }
    const startScale = fromCurrent ? currentScale() : base;
    renderState.scale = startScale;
    renderAp();
    if (startScale >= 0.999) {
      renderState.scale = 1;
      renderAp();
      return;
    }
    tween = gsapApi.to(renderState, {
      scale: predictedApLimit,
      duration: apSecondsUntil(startScale, predictedApLimit, agility, max),
      ease: "none",
      overwrite: true,
      onUpdate: renderAp
    });
  }

  function currentScale() {
    return normalize(renderState.scale);
  }

  function renderAp() {
    const scale = currentScale();
    barNode.style.transform = `scaleX(${scale})`;
    if (labelNode) labelNode.textContent = `${Math.round(scale * max)}/${max}`;
  }

  function clearResumeTimer() {
    if (resumeTimer !== null) {
      window.clearTimeout(resumeTimer);
      resumeTimer = null;
    }
  }
}

function resolveBarNode(node: HTMLElement) {
  if (node.tagName.toLowerCase() === "span") return node;
  return node.querySelector("span") || node;
}

function resolveLabelNode(node: HTMLElement) {
  if (node.tagName.toLowerCase() === "em") return node;
  return node.querySelector("em");
}

function loadGsap() {
  gsapPromise = gsapPromise || import("gsap").then((module) => module.gsap);
  return gsapPromise;
}

function normalize(value: number) {
  return Math.max(0, Math.min(1, Number.isFinite(value) ? value : 0));
}

function prefersReducedMotion() {
  return window.matchMedia("(prefers-reduced-motion: reduce)").matches;
}
