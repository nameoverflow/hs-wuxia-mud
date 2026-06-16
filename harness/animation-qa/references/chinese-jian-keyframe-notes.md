# Chinese Jian Keyframe Notes

Date: 2026-06-16

Purpose: guide the current segmented v12 straight-sword keyframes without drifting into Japanese or Korean martial-art silhouettes.

References:

- Wudang Federation Hong Kong, "武當太極劍劍法動作要領": https://www.wdgf.hk/%E6%AD%A6%E7%95%B6%E5%A4%AA%E6%A5%B5%E5%8A%8D%E5%8A%8D%E6%B3%95%E5%8B%95%E4%BD%9C%E8%A6%81%E9%A0%98/
- 武当山道家传统武术馆, "太极剑训练方法": https://www.wudangpai.com/archives/s1238.shtml
- 武术世界频道, "太极剑的十三种基本剑法": https://www.sohu.com/a/373665729_672232

Pose decisions:

- Thrust uses ci jian: wrist, arm, and sword align forward, with the body and front step driving the point.
- Chop uses pi jian: sword comes from a lifted guard/windup into a downward or diagonal line, with the torso rotating rather than a two-handed katana-like cut.
- Rising cut uses liao jian: sword travels from lower rear to forward-up arc, kept close to the body before release.
- Parry uses lan/jia: one-hand straight sword lifts diagonally forward-up to intercept, not a two-handed high block.
- Dodge keeps the sword alive in the lead hand while the root moves back and down; it avoids high kicking or taekwondo-like leg emphasis.
- Hurt folds the torso and weapon hand back while retaining the straight-sword grip, rather than dropping into a broad theatrical collapse.

Implementation notes:

- The visible sword is currently a thin `line` binding on the `sword` bone. This is a tool prop for keyframe design, not final sword artwork.
- The sword bone is parented to `frontArm`, and each sword pose sets the local sword origin to the current `frontWrist` keypoint so the prop stays attached to the hand.
- The attack entries are intentionally separate from the generic fist entry: `segmented.v12.sword.thrust.female`, `segmented.v12.sword.chop.female`, and `segmented.v12.sword.rising_cut.female`.
