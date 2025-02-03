import { Elm } from "./Main.elm"

const node = document.getElementById("elm");
const flags = null;
const app = Elm.Main.init({ node, flags });

app.ports.interopFromElm.subscribe((fromElm) => {
    switch (fromElm.tag) {
        case "alert": {
            alert(fromElm.data.message);
            break;
        }
    }
});
