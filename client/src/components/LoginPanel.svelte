<script lang="ts">
  import { connect, requestCharacterCreationConfig, testEntryFromUrl, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { CharacterCreationBonus, CharacterCreationChoice, CharacterCreationConfig, CharacterCreationGenderOption, CharacterCreationOption } from "../protocol";
  import { onMount } from "svelte";

  export let state: GameState;

  const emptyBonus: CharacterCreationBonus = { strength: 0, agility: 0, vitality: 0, maxQi: 0, appearance: 0 };

  let username = "";
  let reset = false;
  let config: CharacterCreationConfig | null = null;
  let configError = "";
  let gender = "";
  let origin = "";
  let childhood1 = "";
  let childhood2 = "";
  let step = 0;

  const finalStep = 5;
  const stageLabels = ["名号", "性别", "出身", "幼年", "后来", "预览"];

  onMount(() => {
    loadCreationConfig();
    const entry = testEntryFromUrl();
    if (!entry) return;
    username = entry.user;
    reset = entry.reset;
    connect(entry.user, { reset: entry.reset });
  });

  $: baseStats = config?.baseStats ?? emptyBonus;
  $: genderOptions = config?.genders ?? [];
  $: originOptions = config?.origins ?? [];
  $: childhoodOneOptions = config?.childhood1 ?? [];
  $: childhoodTwoOptions = config?.childhood2 ?? [];
  $: selectedGender = selectedGenderOption(genderOptions, gender);
  $: selectedOrigin = selectedOption(originOptions, origin);
  $: selectedChildhood1 = selectedOption(childhoodOneOptions, childhood1);
  $: selectedChildhood2 = selectedOption(childhoodTwoOptions, childhood2);
  $: currentOptions = step === 2
    ? originOptions
    : step === 3
      ? childhoodOneOptions
      : step === 4
        ? childhoodTwoOptions
        : [];
  $: currentTitle = stepTitle(step);
  $: currentPrompt = stepPrompt(step);
  $: titleIsNarrative = step === 3 || step === 4;
  $: totalBonus = [selectedOrigin, selectedChildhood1, selectedChildhood2].reduce(
    (sum, option) => addBonus(sum, option?.bonus ?? emptyBonus),
    emptyBonus
  );
  $: previewStats = {
    strength: baseStats.strength + totalBonus.strength,
    agility: baseStats.agility + totalBonus.agility,
    vitality: baseStats.vitality + totalBonus.vitality,
    maxQi: baseStats.maxQi + totalBonus.maxQi,
    appearance: clampAppearance(baseStats.appearance + totalBonus.appearance)
  };
  $: previewName = username.trim() || "无名客";
  $: creationReady = Boolean(config && gender && origin && childhood1 && childhood2);

  function submit() {
    if (!config) return;
    if (step < finalStep) {
      setStep(step + 1);
      return;
    }
    if (!creationReady) return;
    const creation: CharacterCreationChoice = { gender, origin, childhood1, childhood2 };
    connect(username, { reset, creation });
  }

  function loadCreationConfig() {
    config = null;
    configError = "";
    requestCharacterCreationConfig()
      .then((nextConfig) => {
        config = nextConfig;
        gender = nextConfig.genders[0]?.id ?? "";
        origin = nextConfig.origins[0]?.id ?? "";
        childhood1 = nextConfig.childhood1[0]?.id ?? "";
        childhood2 = nextConfig.childhood2[0]?.id ?? "";
      })
      .catch(() => {
        configError = "角色创建配置读取失败。请确认游戏服务已启动后重试。";
      });
  }

  function selectedOption(options: CharacterCreationOption[], id: string) {
    return options.find((option) => option.id === id) ?? null;
  }

  function selectedGenderOption(options: CharacterCreationGenderOption[], id: string) {
    return options.find((option) => option.id === id) ?? null;
  }

  function addBonus(left: CharacterCreationBonus, right: CharacterCreationBonus): CharacterCreationBonus {
    return {
      strength: left.strength + right.strength,
      agility: left.agility + right.agility,
      vitality: left.vitality + right.vitality,
      maxQi: left.maxQi + right.maxQi,
      appearance: left.appearance + right.appearance
    };
  }

  function clampAppearance(value: number) {
    return Math.max(0, Math.min(10, value));
  }

  function bonusText(bonus: CharacterCreationBonus) {
    const parts = [
      bonus.strength ? `臂力 +${bonus.strength}` : "",
      bonus.agility ? `身法 +${bonus.agility}` : "",
      bonus.vitality ? `根骨 +${bonus.vitality}` : "",
      bonus.maxQi ? `内力根基 +${bonus.maxQi}` : "",
      bonus.appearance ? `颜值 ${bonus.appearance > 0 ? "+" : ""}${bonus.appearance}` : ""
    ].filter(Boolean);
    return parts.join(" / ");
  }

  function storyText(option: CharacterCreationOption | null | undefined) {
    return option?.story.replaceAll("{name}", previewName) ?? "";
  }

  function setStep(next: number) {
    step = Math.max(0, Math.min(finalStep, next));
  }

  function selectGenderOption(option: CharacterCreationGenderOption) {
    gender = option.id;
    setStep(step + 1);
  }

  function selectCurrentOption(option: CharacterCreationOption) {
    if (step === 2) {
      origin = option.id;
    } else if (step === 3) {
      childhood1 = option.id;
    } else if (step === 4) {
      childhood2 = option.id;
    }
    setStep(step + 1);
  }

  function selectedAtCurrentStep(option: CharacterCreationOption) {
    return (step === 2 && origin === option.id)
      || (step === 3 && childhood1 === option.id)
      || (step === 4 && childhood2 === option.id);
  }

  function stepTitle(index: number) {
    if (index === 0) return "角色姓名";
    if (index === 1) return `${previewName}是：`;
    if (index === 2) return `${previewName}出身于：`;
    if (index === 3) return storyText(selectedOrigin) || "幼年经历";
    if (index === 4) return [storyText(selectedOrigin), storyText(selectedChildhood1)].filter(Boolean).join("\n");
    return "完整预览";
  }

  function stepPrompt(index: number) {
    if (index === 0) return "输入角色姓名，然后继续选择性别、出身和经历。";
    if (index === 1) return "";
    if (index === 2) return "选择一项出身。不同出身会影响初始属性。";
    if (index === 3 || index === 4) return "";
    return "确认姓名、经历和初始属性。";
  }
</script>

<form class="login-panel creation-panel" on:submit|preventDefault={submit}>
  <div class="creation-scene">
    <div class="creation-head">
      <div class="creation-progress" aria-label="创建进度">
        {#each stageLabels as label, index}
          <span class:active={step === index} class:complete={step > index}>{label}</span>
        {/each}
      </div>
      <label class="reset-toggle">
        <input type="checkbox" bind:checked={reset} disabled={state.connected || state.connecting} />
        <span>{translate(state.locale, "ui.test_reset")}</span>
      </label>
    </div>

    <div class="creation-stage-title" class:narrative-title={titleIsNarrative}>
      <span>{stageLabels[step]}</span>
      <h2>{currentTitle}</h2>
      {#if currentPrompt}
        <p>{currentPrompt}</p>
      {/if}
    </div>

    {#if configError}
      <div class="creation-config-state">
        <p>{configError}</p>
        <button type="button" class="primary-button" on:click={loadCreationConfig}>重试</button>
      </div>
    {:else if !config}
      <div class="creation-config-state">
        <p>正在读取角色创建配置。</p>
      </div>
    {:else if step === 0}
      <div class="creation-name-row">
        <input
          type="text"
          bind:value={username}
          disabled={state.connected || state.connecting}
          placeholder={translate(state.locale, "field.name")}
          autocomplete="username"
        />
        <button type="submit" class="primary-button" disabled={!config || state.connected || state.connecting}>
          继续
        </button>
      </div>
    {:else if step === 1}
      <div class="creation-option-grid gender-options">
        {#each genderOptions as option}
          <button
            type="button"
            class:selected={gender === option.id}
            disabled={state.connected || state.connecting}
            on:click={() => selectGenderOption(option)}
          >
            <strong>{option.label}</strong>
          </button>
        {/each}
      </div>
    {:else if step < finalStep}
      <div class="creation-option-grid" class:origin-options={step === 2}>
        {#each currentOptions as option}
          <button
            type="button"
            class:selected={selectedAtCurrentStep(option)}
            disabled={state.connected || state.connecting}
            on:click={() => selectCurrentOption(option)}
          >
            <strong>{option.label}</strong>
            <small>{storyText(option)}</small>
            <em>{bonusText(option.bonus)}</em>
          </button>
        {/each}
      </div>
    {:else}
      <section class="creation-preview" aria-live="polite">
        <div class="creation-preview-copy">
          <p><strong>{previewName}</strong>，{selectedGender?.label ?? gender}，出身于：{selectedOrigin?.label ?? ""}。</p>
          <p>{storyText(selectedOrigin)}</p>
          <p>{storyText(selectedChildhood1)}</p>
          <p>{storyText(selectedChildhood2)}</p>
        </div>
        <dl class="creation-stat-grid">
          <div>
            <dt>性别</dt>
            <dd>{selectedGender?.label ?? gender}</dd>
          </div>
          <div>
            <dt>臂力</dt>
            <dd>{previewStats.strength}</dd>
          </div>
          <div>
            <dt>身法</dt>
            <dd>{previewStats.agility}</dd>
          </div>
          <div>
            <dt>根骨</dt>
            <dd>{previewStats.vitality}</dd>
          </div>
          <div>
            <dt>内力根基</dt>
            <dd>{previewStats.maxQi}</dd>
          </div>
          <div>
            <dt>颜值</dt>
            <dd>{previewStats.appearance}</dd>
          </div>
        </dl>
      </section>
    {/if}

    <div class="creation-actions">
      <button type="button" class="ghost-button" disabled={step === 0 || state.connected || state.connecting} on:click={() => setStep(step - 1)}>
        回退
      </button>
      {#if step === finalStep}
        <button type="submit" class="primary-button" disabled={!creationReady || state.connected || state.connecting}>
          {state.connecting ? "入世中" : "创建 / 进入"}
        </button>
      {/if}
    </div>

    <p>{translate(state.locale, "ui.login_hint")}</p>
  </div>
</form>
