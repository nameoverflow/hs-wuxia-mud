import { mount } from "svelte";
import BattleLab from "./BattleLab.svelte";
import "../styles.css";

mount(BattleLab, { target: document.getElementById("app")! });
