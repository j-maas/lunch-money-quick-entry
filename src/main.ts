import { Elm } from "./Main.elm"

import * as tauriStore from '@tauri-apps/plugin-store';

const store = await tauriStore.load("store.json", { autoSave: true });

const today = new Date().toISOString().slice(0, 10);
const token = await store.get<string>("token");
const flags: Elm.Flags = {
    today,
    token,
};

console.debug("Initializing Elm with flags: ", flags)

const node = document.getElementById("elm");
const app = Elm.Main.init({ node, flags });

app.ports.interopFromElm.subscribe((fromElm) => {
    switch (fromElm.tag) {
        case "storeSetting": {
            store.set(fromElm.data.key, fromElm.data.value);
            console.debug(`Stored a setting: ${fromElm.data.key} -> ${fromElm.data.value}`, fromElm.data)
            break;
        }
    }
});
