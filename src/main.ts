import { Elm } from "./Main.elm"

const today = new Date().toISOString().slice(0, 10);
const flags: Elm.Flags = {
    today
};

const node = document.getElementById("elm");
const app = Elm.Main.init({ node, flags });

app.ports.interopFromElm.subscribe((fromElm) => {
    switch (fromElm.tag) {
        case "alert": {
            alert(fromElm.data.message);
            break;
        }
    }
});
