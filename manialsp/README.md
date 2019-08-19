# ManiLSP

A language server for ManiaScript.

# Features

- [x] Lexical errors
  - [x] Invalid token
  - [x] Missing tokens
- [ ] Symbols list
  - [ ] Document symbol list
  - [ ] Workspace symbol list
- [ ] Semantic Errors
  - [ ] Use of undefined symbol
  - [ ] Type checking

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
- Set the extension to txt (Script.txt doesn't seem to work :neutral-face:)
- Set the path to where you downloaded manialsp.exe
- Apply, Save and it should work when you open a maniascript file!