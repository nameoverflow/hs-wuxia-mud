import { mount } from "svelte";
import AnimationRigTool from "./AnimationRigTool.svelte";
import "./tool.css";

const app = mount(AnimationRigTool, {
  target: document.getElementById("animation-rig-tool")!
});

export default app;
