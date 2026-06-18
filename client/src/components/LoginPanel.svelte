<script lang="ts">
  import { connect, testEntryFromUrl, type GameState } from "../game";
  import { translate } from "../i18n";
  import type { CharacterCreationChoice } from "../protocol";
  import { onMount } from "svelte";

  export let state: GameState;

  interface CreationBonus {
    strength: number;
    agility: number;
    vitality: number;
    maxQi: number;
    appearance: number;
  }

  interface CreationOption {
    id: string;
    label: string;
    story: string;
    bonus: CreationBonus;
  }

  const baseStats = { strength: 18, agility: 18, vitality: 18, maxQi: 100, appearance: 5 };
  const emptyBonus: CreationBonus = { strength: 0, agility: 0, vitality: 0, maxQi: 0, appearance: 0 };

  const originOptions: CreationOption[] = [
    {
      id: "martial_family",
      label: "武学世家",
      story: "我家三代都把拳谱和剑诀藏在祖屋横梁上。长辈教我先记仇，再记招；我从小明白，江湖欠下的债不会自己消。",
      bonus: { strength: 3, agility: 1, vitality: 1, maxQi: 12, appearance: 0 }
    },
    {
      id: "scholar_house",
      label: "书香门第",
      story: "我出生在墨香和雨声里。父亲说人心比刀更薄，母亲说读书不是为了做官，是为了看清别人话里的钩。",
      bonus: { strength: 0, agility: 2, vitality: 2, maxQi: 8, appearance: 1 }
    },
    {
      id: "official_house",
      label: "官宦人家",
      story: "我幼时见惯朱门和刑杖，也见过笑脸下面的刀。那座宅子教我站得稳，说话慢，先看路，再看人。",
      bonus: { strength: 1, agility: 1, vitality: 3, maxQi: 0, appearance: 1 }
    },
    {
      id: "medicine_house",
      label: "医药之家",
      story: "我在药柜、银针和苦汤里长大。家里救过恶人，也救过好人；我很早就知道，一口气不断，人就还有账可算。",
      bonus: { strength: 0, agility: 1, vitality: 3, maxQi: 8, appearance: 1 }
    },
    {
      id: "orphan",
      label: "孤儿",
      story: "我没有可说的家门。记事起就跟着破庙的钟声和街角的冷饭活着，没人替我出头，我便自己学会出头。",
      bonus: { strength: 2, agility: 3, vitality: 0, maxQi: 0, appearance: -1 }
    }
  ];

  const childhoodOneOptions: CreationOption[] = [
    {
      id: "courtyard_practice",
      label: "院中偷练",
      story: "我常在无人时照着墙上的影子出拳。拳头打在木桩上，疼得发麻，我却一次比一次站得久。",
      bonus: { strength: 2, agility: 1, vitality: 0, maxQi: 0, appearance: 0 }
    },
    {
      id: "river_chase",
      label: "逐水奔跑",
      story: "我追着渡船和流云奔跑，跌进河里，又从水里爬出来。脚下的泥越滑，我越不肯慢。",
      bonus: { strength: 0, agility: 3, vitality: 0, maxQi: 0, appearance: 1 }
    },
    {
      id: "herb_gathering",
      label: "入山采药",
      story: "我随长辈入山辨草采药，背篓压得肩骨生疼。山路教我忍耐，药性教我分寸。",
      bonus: { strength: 0, agility: 0, vitality: 3, maxQi: 0, appearance: 0 }
    },
    {
      id: "night_reading",
      label: "夜读旧书",
      story: "我伴着灯火读旧书，读到窗纸发白。书里的人大多不得善终，我却学会把心事藏在字句后面。",
      bonus: { strength: 0, agility: 1, vitality: 2, maxQi: 0, appearance: 1 }
    }
  ];

  const childhoodTwoOptions: CreationOption[] = [
    {
      id: "market_brawls",
      label: "市井斗殴",
      story: "我在市井里学会挨打和还手。人群散去后，地上只剩血点，我记住了谁先伸手，谁先退后。",
      bonus: { strength: 2, agility: 0, vitality: 1, maxQi: 0, appearance: -1 }
    },
    {
      id: "mountain_errands",
      label: "翻山跑腿",
      story: "我替人翻山送信取物，雨天走泥路，晴天走碎石。路越远，我越知道哪一步不能省。",
      bonus: { strength: 1, agility: 1, vitality: 1, maxQi: 0, appearance: 0 }
    },
    {
      id: "breath_lessons",
      label: "记住口诀",
      story: "我偶然记住了几句调息口诀。没人肯细讲，我只在夜里慢慢试，直到胸中那口气不再乱撞。",
      bonus: { strength: 0, agility: 0, vitality: 1, maxQi: 12, appearance: 1 }
    },
    {
      id: "cold_watch",
      label: "寒夜守门",
      story: "我在寒夜里守过长门，听风从门缝里钻进骨头。天亮时我还站着，便知道自己没那么容易倒下。",
      bonus: { strength: 1, agility: 0, vitality: 2, maxQi: 0, appearance: 0 }
    }
  ];

  let username = "";
  let reset = false;
  let origin = originOptions[0].id;
  let childhood1 = childhoodOneOptions[0].id;
  let childhood2 = childhoodTwoOptions[0].id;

  onMount(() => {
    const entry = testEntryFromUrl();
    if (!entry) return;
    username = entry.user;
    reset = entry.reset;
    connect(entry.user, { reset: entry.reset });
  });

  $: selectedOrigin = selectedOption(originOptions, origin);
  $: selectedChildhood1 = selectedOption(childhoodOneOptions, childhood1);
  $: selectedChildhood2 = selectedOption(childhoodTwoOptions, childhood2);
  $: totalBonus = [selectedOrigin, selectedChildhood1, selectedChildhood2].reduce(
    (sum, option) => addBonus(sum, option.bonus),
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

  function submit() {
    const creation: CharacterCreationChoice = { origin, childhood1, childhood2 };
    connect(username, { reset, creation });
  }

  function selectedOption(options: CreationOption[], id: string) {
    return options.find((option) => option.id === id) || options[0];
  }

  function addBonus(left: CreationBonus, right: CreationBonus): CreationBonus {
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

  function bonusText(bonus: CreationBonus) {
    const parts = [
      bonus.strength ? `臂力 +${bonus.strength}` : "",
      bonus.agility ? `身法 +${bonus.agility}` : "",
      bonus.vitality ? `根骨 +${bonus.vitality}` : "",
      bonus.maxQi ? `内力根基 +${bonus.maxQi}` : "",
      bonus.appearance ? `颜值 ${bonus.appearance > 0 ? "+" : ""}${bonus.appearance}` : ""
    ].filter(Boolean);
    return parts.join(" / ");
  }
</script>

<form class="login-panel creation-panel" on:submit|preventDefault={submit}>
  <div class="creation-head">
    <div>
      <h2>立身江湖</h2>
      <p>我先定下姓名，再把来处、童年和最早学会的生存方式写清楚。</p>
    </div>
    <label class="reset-toggle">
      <input type="checkbox" bind:checked={reset} disabled={state.connected || state.connecting} />
      <span>{translate(state.locale, "ui.test_reset")}</span>
    </label>
  </div>

  <div class="creation-name-row">
    <input
      type="text"
      bind:value={username}
      disabled={state.connected || state.connecting}
      placeholder={translate(state.locale, "field.name")}
      autocomplete="username"
    />
    <button type="submit" class="primary-button" disabled={state.connected || state.connecting}>
      {state.connecting ? "入世中" : "创建 / 进入"}
    </button>
  </div>

  <section class="creation-stage">
    <div class="creation-stage-title">
      <h3>{previewName}出身于：</h3>
      <span>一</span>
    </div>
    <div class="creation-option-grid origin-options">
      {#each originOptions as option}
        <button
          type="button"
          class:selected={origin === option.id}
          disabled={state.connected || state.connecting}
          on:click={() => origin = option.id}
        >
          <strong>{option.label}</strong>
          <small>{option.story}</small>
          <em>{bonusText(option.bonus)}</em>
        </button>
      {/each}
    </div>
  </section>

  <section class="creation-stage">
    <div class="creation-stage-title">
      <h3>幼年经历：</h3>
      <span>二</span>
    </div>
    <div class="creation-option-grid">
      {#each childhoodOneOptions as option}
        <button
          type="button"
          class:selected={childhood1 === option.id}
          disabled={state.connected || state.connecting}
          on:click={() => childhood1 = option.id}
        >
          <strong>{option.label}</strong>
          <small>{option.story}</small>
          <em>{bonusText(option.bonus)}</em>
        </button>
      {/each}
    </div>
  </section>

  <section class="creation-stage">
    <div class="creation-stage-title">
      <h3>后来我又经历：</h3>
      <span>三</span>
    </div>
    <div class="creation-option-grid">
      {#each childhoodTwoOptions as option}
        <button
          type="button"
          class:selected={childhood2 === option.id}
          disabled={state.connected || state.connecting}
          on:click={() => childhood2 = option.id}
        >
          <strong>{option.label}</strong>
          <small>{option.story}</small>
          <em>{bonusText(option.bonus)}</em>
        </button>
      {/each}
    </div>
  </section>

  <section class="creation-preview" aria-live="polite">
    <div class="creation-stage-title">
      <h3>完整预览</h3>
      <span>{previewName}</span>
    </div>
    <div class="creation-preview-copy">
      <p><strong>{previewName}</strong>出身于：{selectedOrigin.label}。</p>
      <p>{selectedOrigin.story}</p>
      <p>{selectedChildhood1.story}</p>
      <p>{selectedChildhood2.story}</p>
    </div>
    <dl class="creation-stat-grid">
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

  <p>{translate(state.locale, "ui.login_hint")}</p>
</form>
