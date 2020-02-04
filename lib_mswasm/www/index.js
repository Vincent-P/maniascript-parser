import * as wasm from "lib_mswasm";
import CodeMirror from "codemirror";
import 'codemirror/mode/clike/clike';

const TextArea = document.querySelector('#code-editor');
TextArea.value = "";
const CodeEditor = CodeMirror.fromTextArea(TextArea, {
    lineNumbers: true,
    theme: 'dracula',
    mode: {name: 'clike'},
    value: ''
});

const DumpText = document.querySelector('#dump-text');

CodeEditor.on('change', (instance, change) => {
    const code = CodeEditor.getValue();
    const infos = wasm.infos_from_string(code);
    DumpText.textContent = infos.dump;
});
