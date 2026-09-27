# 剪影交锋 v2：生成记录

最终方向：保留项目已有的 v13 抽象小人剪影。人物不生成写实服装、面容或完整立绘。原有 image_gen body/hair PNG 作为形状来源；仅通过机械图集拼接与颜色归一化复用。

生成工具：内置 `image_gen`，未调用 OpenAI API 或其他生成服务。两个新增源图均保存在 `generated/`，运行时使用其机械处理结果。

## Background prompt

Use case: illustration-story. Asset type: ONE sparse background plate for a small wuxia silhouette battle stage in a text MUD. Wide panoramic composition, 1536x768. A restrained Chinese ink wash night landscape: distant misty mountain silhouettes, a few out-of-focus bamboo stems framing only the extreme left and right edges, a worn dark stone ground plane across the lower quarter. Very subdued desaturated jade charcoal, smoky blue-gray, faint warm paper undertone. Upper two thirds mostly atmospheric negative space, a soft pale band of distant fog on the horizon, no bright moon or focal object. Bottom central area dark and uncluttered so small ivory and muted amber silhouette fighters remain very readable. Hand-brushed ink textures, quiet wuxia storybook atmosphere, flat side-on theatrical stage perspective. NO people, NO animals, NO weapons, NO text, NO typography, NO symbols, NO UI, NO border, NO dramatic light beams, NO saturated green, NO colorful fantasy. This is supporting background artwork; it must never compete with foreground actors.

Accepted output: `generated/backdrop-source.png`. Actual generated dimensions 1774×887; same requested 2:1 composition. Resized to 1280×640 and encoded as WebP.

## VFX prompt

Use case: stylized-concept. Asset type: ONE production VFX texture atlas for a restrained wuxia silhouette battle game. Canvas 1536x1024, exactly three equal columns and two equal rows, each cell 512x512. GENUINELY TRANSPARENT background alpha, not a painted checkerboard. Six separate WHITE and pale gray dry-ink brush marks, nothing else. Each mark must be fully inside its own cell with 60px transparent safety margins, no overlap between cells. No labels, lettering, borders or grid. ROW 1 left-to-right: 1 a very swift horizontal thrust streak from left to right, long taper at right and frayed dry-brush fibers at left, centered horizontally; 2 a single decisive crescent sweeping diagonally downward from upper-left to lower-right, variable-width ink sword slash; 3 a single ascending crescent stroke sweeping from lower-left to upper-right for a rising sword or kick. ROW 2 left-to-right: 4 a compact jagged ink impact burst with a dense small center and 5-7 short irregular rays, restrained, not an explosion; 5 an incomplete circular defensive brush ring, interrupted rugged edges, quiet and strong; 6 a soft wispy circular qi brush swirl with open center and a few feathered ink traces. Chinese hand-brushed calligraphic energy, high-contrast simple white alpha silhouettes with organic dry-brush transparency, readable small. NO characters, no hands, no weapons, no scenery, no color glow, no blue electricity, no game UI, no text. This sheet will be mechanically sliced and color-tinted in the runtime.

Accepted output: `generated/vfx-source.png`, 1536×1024 RGBA. The PNG has actual alpha (including 0-alpha background), despite the dark appearance of the tool preview. Streak tail margins are smaller than requested; the visible primary marks stay in their cells. The exported effects receive deterministic 12px transparent padding. No shape or effect was redrawn.

## Mechanical pipeline

1. Run `prepare_assets.py` using a Python environment with Pillow to reproduce the seven WebP assets.
2. Run `npm run pack:battle` in `client/` to pack four character body/hair atlases and the VFX atlas.
3. `npm run check` verifies source/output hashes, sizes, frame references, action timing and fixed YAML bindings.
4. Behavioral tests and full-stage browser recording verify the result in motion.

Regenerate only when a needed pose/effect changes shape. Existing accepted alpha is never manually retouched. New ordinary actions should be authored as manifest data and composed from accepted poses and effects before new assets are commissioned.
