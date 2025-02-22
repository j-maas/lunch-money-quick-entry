import { Elm } from "./Main.elm"

import * as tauriStore from '@tauri-apps/plugin-store';

init().catch(console.error)

async function init() {
    const store = await tauriStore.load("store.json", { autoSave: true });

    const today = new Date().toISOString().slice(0, 10);
    const token = await store.get<string>("token") || null;
    const insertQueue = await store.get<any>("insertQueue") || null;
    const autofillCache = await store.get<any>("autofillCache") || null;
    const flags: Elm.Flags = {
        today,
        token,
        insertQueue,
        autofillCache,
    };

    console.log("Initializing Elm with flags: ", flags)

    const node = document.getElementById("elm");
    const app = Elm.Main.init({ node, flags });

    app.ports.interopFromElm.subscribe((fromElm) => {
        switch (fromElm.tag) {
            case "storeSetting": {
                store.set(fromElm.data.key, fromElm.data.value);
                console.debug(`Stored a setting: ${fromElm.data.key} -> ${fromElm.data.value}`, fromElm.data)
                break;
            }
            case "log": {
                const { level, message } = fromElm.data
                switch (level) {
                    case "Info": {
                        console.log(message);
                        break;
                    }
                    case "Warning": {
                        console.warn(message);
                        break;
                    }
                    case "Error": {
                        console.error(message);
                        break;
                    }
                    case "Debug": {
                        console.debug(message);
                        break;
                    }
                }
                break;
            }

        }
    });
}
