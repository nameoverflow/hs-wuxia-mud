import { mount } from "svelte";
import PoseSheet from "./PoseSheet.svelte";
import "../styles.css";

mount(PoseSheet, { target: document.getElementById("app")! });
