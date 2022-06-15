# emacs-config
for my own use

```
cd ~
git clone https://github.com/cheryllium/emacs-config.git
rm -r ~/.emacs.d # yes this will overwrite anything you have in there
mv ~/emacs-config/{.[!.],}* ~/
rm -r ~/emacs-config
rm README.md
```

note: the `mv` command will give "no such file or directory" output which can be safely ignored

on zsh, that weird mv command should be this instead: 

```
mv ~/emacs-config/*(DN) ~/
```
