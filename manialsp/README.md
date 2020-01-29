# ManiaLSP

A language server for ManiaScript.

/!\ Currently all features are disabled /!\

# Features

- [ ] Lexical errors
  - [ ] Invalid token
  - [ ] Missing tokens
- [ ] Symbols list
  - [ ] Document symbol list
  - [ ] Workspace symbol list
- [ ] Semantic Errors
  - [ ] Use of undefined symbol
  - [ ] Type checking
- [ ] Completion

# Installation

The first step is to download the manialsp binary and add it to the PATH.

## VSCode

- Install the 'ManiaLSP' extension

## Emacs

- First you need to define a major mode for maniascript

```
  (define-derived-mode maniascript-mode prog-mode "maniascript"
    (setq font-lock-defaults '(maniascript-highlights)))
```

- Install the package 'lsp-mode', and paste this after requiring it

```
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "manialsp.exe")
                    :major-modes '(maniascript-mode)
                    :server-id 'manialsp)))
```

## Jetbrains IDEs

- Install the plugin 'LSP Support' made by gtache.
- Go to the settings Language & Frameworks -> Language Server Protocol -> Server Definitions
    - Select 'Executable' on the dropdown
    - Set the extension to txt (Script.txt doesn't seem to work)
    - Set the path to where you downloaded manialsp.exe
- Apply, Save and it should work when you open a maniascript file!

Some extra steps are needed to have syntax highlighting

- Download the textmate bundle for maniascript [here](https://onedrive.live.com/download?cid=2EC0D2E0D9DA402A&resid=2EC0D2E0D9DA402A%2115044&authkey=AM6DM6OsAhaXIO4).
- Extract it in a folder somewhere
- Go to the settings Editor -> TextMate Bundles
  - Click the + button and select the folder containing the textmate bundle
- Apply, Save and you should have syntax highlighting!

## Sublime Text 3

- Install the plugin LSP made by tomv564
- Type 'LSP Settings' in the Command Palette to change the settings and paste this:
```jsonc
{
	"clients": {
	    "manialsp":
	    {
	      "command": ["manialsp.exe"],
  	      // Use: "Show Scope Name" from Sublime's Developer menu
	      "scopes": ["source.ms"],
  	      // Run: view.settings().get("syntax") in console
	      "syntaxes": ["Packages/ManiaScript/ManiaScript.tmLanguage"],
	      "languageId": "maniascript",
	    }
	}
}
```
- Restart sublime
- To start the server type 'LSP Enable Language Server in Project' in the Command Palette then select manialsp
