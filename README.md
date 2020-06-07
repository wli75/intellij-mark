# intellij-mark

An emacs plugin that mimics Intellij's prev/next navigation (https://intellij-support.jetbrains.com/hc/en-us/community/posts/206573029-Way-to-jump-back-to-previous-file-line-after-Ctrl-clicking-function-call-).

## Getting Started 
Add the following to .emacs
```
(add-to-list 'load-path "DIR_CONTAINING_INTELLIJ_MARK_EL_FILE")
(require 'intellij-mark)
(intellij-mark-mode 1)
```

Navigate to prev/next cursor positions using M-b/M-f.
You can change the key binding by changing the following lines in intellij-mark.el
```
(global-set-key (kbd "M-b") 'intellij-mark-ring-prev)
(global-set-key (kbd "M-f") 'intellij-mark-ring-next)
```

Default setup pushes current cursor position to the mark ring upon mouse click or goto-line commands in evil mode. 
You can change the list of commands that push cursor to the mark ring by setting the following.
```
(setq intellij-mark-ring-cmds
  '(evil-mouse-drag-region
    evil-goto-line))
```

## Demo
<img src="https://github.com/wli75/intellij-mark/blob/master/demo.gif" width="500">

