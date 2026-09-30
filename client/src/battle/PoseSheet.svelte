<script lang="ts">
  import SvgBattleActor from "../components/SvgBattleActor.svelte";
  import { svgPose, svgPoseIds } from "./svgPoseLibrary";
  import type { CombatStyle, VisualProfile } from "./animationTypes";

  // 姿势总览：每个姿势按拳、剑两种骨架各画一格，地面线在 y=0，方便逐个检查比例、关节弯向和落脚。
  let filter = "";
  let profile: VisualProfile = "female";
  /** 女性造型对照：新版少女女侠、旧版高马尾，或两版并排。 */
  let look: "maiden" | "ponytail" | "compare" = "maiden";
  $: looks = look === "compare" ? (["maiden", "ponytail"] as const) : ([look] as const);
  const styles: CombatStyle[] = ["fist", "sword"];
  $: ids = svgPoseIds().filter((id) => id.includes(filter.trim()));
</script>

<main class="pose-sheet">
  <header>
    <h1>剪影姿势总览</h1>
    <label>筛选 <input bind:value={filter} placeholder="姿势名" /></label>
    <label>剪影 <select bind:value={profile}><option value="female">束发</option><option value="male">无发饰</option></select></label>
    <label>女侠造型 <select bind:value={look}><option value="maiden">发髻披发（新）</option><option value="ponytail">高马尾（旧）</option><option value="compare">并排对比</option></select></label>
  </header>
  <section class="grid">
    {#each ids as id}
      {#each styles as style}
        {#each looks as variant}
          <figure data-pose={id} data-style={style} data-look={variant}>
            <div class="cell"><div class="ground"></div><div class="figure"><SvgBattleActor pose={svgPose(id, style)} {style} {profile} look={variant} /></div></div>
            <figcaption>{id} · {style === "fist" ? "拳" : "剑"}{look === "compare" ? (variant === "maiden" ? " · 新" : " · 旧") : ""}</figcaption>
          </figure>
        {/each}
      {/each}
    {/each}
  </section>
</main>

<style>
  .pose-sheet { min-height: 100vh; padding: 16px; background: #101916; color: #d4cfb8; font: 13px/1.4 "Songti SC", serif; }
  header { display: flex; gap: 16px; align-items: center; flex-wrap: wrap; margin-bottom: 12px; }
  h1 { font-size: 18px; margin: 0; }
  input, select { background: #18241e; color: #d4dac9; border: 1px solid #425346; padding: 4px 6px; }
  .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px; }
  figure { margin: 0; background: #16211c; border: 1px solid #26352d; }
  .cell { position: relative; height: 190px; overflow: hidden; color: #e8d5a3; }
  .ground { position: absolute; left: 0; right: 0; top: 165px; border-top: 1px solid #3c4d43; }
  .figure { position: absolute; left: 50%; top: 165px; width: 0; height: 0; }
  figcaption { padding: 4px 8px; color: #9fb0a4; font-size: 12px; }
</style>
